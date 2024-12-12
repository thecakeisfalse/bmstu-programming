assume CS:code, DS:data

;; == Big integer configurations ==

bigint_config macro max_number_size, digits, signs
  ifnb <max_number_size>
    const_max_size     equ   max_number_size
  else
    const_max_size     equ   16
  endif

  ifnb <digits>
    const_digits       db    digits, 0
  else
    const_digits       db    "0123456789", 0
  endif
  
  const_digits_count   db    $ - const_digits - 1 ;; -1 to remove last zero

  ifnb <signs>
    const_signs        db    signs, 0
  else
    const_signs        db    "+-", 0
  endif

  const_ends    db 0Dh, 0Ah, '$', 0
endm

bigint_new macro p1,p2,p3,p4,p5,p6,p7,p8
  errifndef const_max_size

  irp name,<p1,p2,p3,p4,p5,p6,p7,p8>
    ifnb <name>
      bigint_new_s name, 0
    endif
  endm
endm

bigint_new_s macro name, additional_size
  errifb <name>

  ;; Big integer structure:
  ;; ======================
  ;; * sign (0 = +, 1 = - or other specified by `const_signs` alphabet)
  ;; * start flag (default: 0xEF = 0EFh)
  ;; * digits (by their position in `const_digits` alphabet)

  &name db    0, 0EFh, const_max_size+additional_size+1 dup('$')
endm

data segment
  _max_input_size          equ 20
  _input_string            db _max_input_size, _max_input_size+1 dup ('$')

  _parse_failed_message    db "Failed to parse big integer", '$'
  _empty_string_message    db "Can't work with empty string", '$'

  ;; octal numbers
  ; bigint_confg 16, "01234567", "+-"

  ;; decimal numbers
  bigint_config 16, "0123456789", "+-"

  ;; hexadecimal number
  ; bigint_config 16, "0123456789", "+-"

  bigint_new num1, num2
  bigint_new_s result, 24
data ends


code segment

;; == Utils ==

search:
  ;; input:  si -- pointer to alphabet
  ;;         al -- symbol to find
  ;; output: bh -- 1 if exists otherwise 0
  ;;         bl -- position in alphabet

  push ax

  xor bx, bx
  mov ah, al
_search_loop:
  lodsb ; si --> ax
  
  cmp al, ah
  je _search_success
  
  inc bl

  cmp al, 0
  je _search_done

  jmp _search_loop
_search_success:
  mov bh, 1
_search_done:
  pop ax
  ret

print_char:
  push ax
  push dx
  mov dl, al
  mov ah, 02h
  int 21h
  pop dx
  pop ax
  ret

print_newline:
  push ax
  push dx
  mov al, 0Dh
  call print_char
  mov al, 0Ah
  call print_char
  pop dx
  pop ax
  ret

;; == Big integer parser ==

_new_check macro name
_check_&name:
  push si
  mov si, offset const_&name&s
  call search
  pop si
  ret
endm

_new_check digit
_new_check sign
_new_check end

;; EBNF grammar
;; ============
;; number = {sign} unsigned
;; sign = '+' | '-' | `const_signs`
;; unsigned = digit {digits}
;; digit = `const_digits`

;; Lexer
;; =====
;; 1) While P(al, sign) = 1,             jmp _bi_parse_sign
;; 2) While P(al, digit) = 1,            jmp _bi_parse_digit
;; 3) If P(al, sign) v P(al, digit) = 0, jmp _bi_parse_failed

_bigint_parse:
  ;; input:  si -- address of input string
  ;;         dl -- size of input string
  ;;         di -- adddress of output big int
  ;; output: ch -- 1 if failed otherwise 0
  ;;         di -- address of output big int

  push di
  push si

  xor ax, ax
  xor cx, cx

  inc di ;; skip `start` byte

  lodsb
_bi_parse_sign:
  call _check_end
  cmp bh, 0
  jne _bi_parse_done

  call _check_digit
  cmp bh, 0
  jne _bi_parse_digit

  call _check_sign
  cmp bh, 0
  je _bi_parse_failed

  xor cl, bl ;; update sign

  dec dl

  lodsb
  jmp _bi_parse_sign
_bi_parse_digit:
  call _check_end
  cmp bh, 0
  jne _bi_parse_done

  call _check_digit
  cmp bh, 0
  je _bi_parse_failed

  ;; update digits in di
  add di, dx
  mov ds:[di], bl
  sub di, dx

  dec dl

  lodsb
  jmp _bi_parse_digit
_bi_parse_failed:
  mov ch, 1
_bi_parse_done:
  pop si
  pop di
  mov ds:[di], cl ;; set sign in big integer
  ret

;; === God, save our souls ===

_bigint_less:
  ;; input:  si -- address of first number
  ;;         di -- address of second number
  ;; output: cl -- result of `num(si) < num(di)`

  push si
  push di

  push ax
  push bx

  xor ax, ax
  xor bx, bx
  xor cl, cl
_bi_compare_signs:
  mov al, ds:[si]
  mov ah, ds:[di]

  inc si
  inc di

  cmp al, ah
  je _bi_compare_digits_pt1

  ;;  al | ah  | F
  ;; --- | --- | --
  ;;  1  |  0  | 1
  ;;  0  |  1  | 0
  ;;
  ;; --> F === al

  mov cl, al
  jmp _bi_cmp_done
_bi_compare_digits_pt1:
  cmp byte ptr ds:[si], '$'
  je _bi_compare_digits_pt2
  inc si
  inc bl
  jmp _bi_compare_digits_pt1
_bi_compare_digits_pt2:
  cmp byte ptr ds:[di], '$'
  je _bi_compare_digits_pt3
  inc di
  inc bh
  jmp _bi_compare_digits_pt2
_bi_compare_digits_pt3:
  dec di
  dec si

  ;; bl ?== bh <===> length num1 ?== length num2

  cmp bl, bh
  jne _bi_cmp_error
_bi_compare_digits_loop:
  mov bh, ds:[di]
  mov bl, ds:[si]

  cmp bh, bl
  jne _bi_cmp_error

  cmp bh, 0EFh
  je _bi_cmp_done

  dec di
  dec si

  jmp _bi_compare_digits_loop
_bi_cmp_error:
  xor cl, al ;; xor with sign

  cmp bl, bh
  jg _bi_cmp_done

  xor cl, 1

  jmp _bi_cmp_done
_bi_cmp_done:
  pop bx
  pop ax

  pop di
  pop si

  ret

;; == Big integer arithmetics ==

_bigint_add:
  ;; input: si, di -- numbers to add
  ;;        dx -- result

  mov al, byte ptr ds:[si]
  mov ah, byte ptr ds:[di]

  cmp al, ah
  je _bi_valid_add

  xor byte ptr ds:[di], 1
  call _bigint_sub
  xor byte ptr ds:[di], 1

  ret
_bi_valid_add:
  push si
  push di
  push dx

  ;; save sign
  push ax

  inc si
  inc di
  
  add dx, 2

  xor bx, bx
  xor ax, ax
_bi_add_loop:
  inc si
  inc di
  xor ah, ah

  mov bl, byte ptr ds:[si]
  mov bh, byte ptr ds:[di]

  cmp bl, '$'
  je _bi_add_skip_digit1

  add al, bl
_bi_add_skip_digit1:
  cmp bh, '$'
  je _bi_add_skip_digit2

  add al, bh
_bi_add_skip_digit2:
  cmp bx, 2424h ; bh = '$' and bl = '$'
  jne _bi_add_skip_digit3

  cmp al, 0 ;; carry = 0
  je _bi_add_restore_sign

  xchg di, dx
  mov byte ptr ds:[di], al

  jmp _bi_add_restore_sign
_bi_add_skip_digit3:
  mov bl, const_digits_count
  div bl

  xchg di, dx
  mov byte ptr ds:[di], ah
  inc di
  xchg dx, di

  jmp _bi_add_loop
_bi_add_restore_sign:
  ;; restore sign
  pop ax

  pop di ;; dx
  mov byte ptr ds:[di], al
  mov dx, di

  pop di
  pop si
  ret

_bigint_sub:
  ;; input: si, di -- numbers to substract
  ;;        dx -- result

  mov al, byte ptr ds:[si]
  mov ah, byte ptr ds:[di]

  cmp al, ah
  je _bi_valid_sub

  xor byte ptr ds:[di], 1
  call _bigint_add
  xor byte ptr ds:[di], 1

  ret
_bi_valid_sub:
  push si
  push di
  push dx

  ;; save sign
  push ax

  mov byte ptr ds:[si], 0
  mov byte ptr ds:[di], 0

  call _bigint_less

  inc si
  inc di

  add dx, 2

  cmp cl, 0
  je _bi_valid_sub_2 ;; si >= di

  pop ax
  xor ah, 1
  push ax
  xchg si, di
_bi_valid_sub_2:
  xor bx, bx
  xor ax, ax
_bi_sub_loop:
  inc si
  inc di
  xchg al, ah
  xor ah, ah

  mov bl, byte ptr ds:[si]
  mov bh, byte ptr ds:[di]

  cmp bl, '$'
  je _bi_sub_restore_sign

  cmp bh, '$'
  je _bi_sub_skip_digit

  add al, bh
_bi_sub_skip_digit:
  cmp bl, al
  jge _bi_sub_digit

  add bl, const_digits_count
  mov ah, 1
_bi_sub_digit:
  sub bl, al

  xchg di, dx
  mov byte ptr ds:[di], bl
  inc di
  xchg dx, di

  jmp _bi_sub_loop
_bi_sub_restore_sign:
  ;; restore sign
  pop ax

  pop di ;; dx
  mov byte ptr ds:[di], ah
  mov dx, di

  pop di
  pop si

  mov byte ptr ds:[si], al
  mov byte ptr ds:[di], al

  ret

_bigint_mul:
  ;; input: si -- first number
  ;;        di -- second number
  ;;        dx -- result of multiplication

  push si
  push di
  push dx

  inc si
  inc di

  add dx, 2

  xor ax, ax
  xor bx, bx
  xor cx, cx ; cl = i, ch = j
_bi_mul_loop1:
  inc si

  cmp byte ptr ds:[si], '$'
  je _bi_mul_loop1_end

  push di
  xor ch, ch
_bi_mul_loop2:
  inc di

  mov bl, byte ptr ds:[di]

  cmp bl, '$'
  je _bi_mul_skip_digit

  mov bh, al
  mov al, byte ptr ds:[si]
  mul bl
  add al, bh
_bi_mul_skip_digit:
  xor ah, ah

  add dl, cl
  add dl, ch

  xchg dx, di

  cmp byte ptr ds:[di], '$'
  je _bi_mul_skip_digit_2

  add al, byte ptr ds:[di]

_bi_mul_skip_digit_2:
  mov bh, al
  cmp bx, '$'
  je _bi_mul_loop2_end

  mov bl, const_digits_count
  div bl

  mov byte ptr ds:[di], ah

  xchg dx, di

  sub dl, cl
  sub dl, ch

  inc ch
  jmp _bi_mul_loop2
_bi_mul_loop2_end:
  xchg di, dx
  sub dl, cl
  sub dl, ch

  pop di

  inc cl
  jmp _bi_mul_loop1
_bi_mul_loop1_end:
  pop dx
  pop di
  pop si
  
  ;; restore sign
  mov al, byte ptr ds:[di]
  mov ah, byte ptr ds:[si]

  xchg di, dx

  xor byte ptr ds:[di], al
  xor byte ptr ds:[di], ah

  xchg di, dx

  ret


bigint_sum macro a, b, ans
  bigint_clear &ans

  mov si, offset &a
  mov di, offset &b
  mov dx, offset &ans

  call _bigint_add
endm

bigint_diff macro a, b, ans
  bigint_clear &ans

  mov si, offset &a
  mov di, offset &b
  mov dx, offset &ans

  call _bigint_sub
endm

bigint_prod macro a, b, ans
  bigint_clear &ans

  mov si, offset &a
  mov di, offset &b
  mov dx, offset &ans

  call _bigint_mul
endm

;; == Big integer utils ==

_bigint_clear:
  ;; input: si -- number to clear
  
  mov byte ptr ds:[si], 0

  inc si
_bi_clear_loop:
  inc si

  cmp byte ptr ds:[si], '$'
  je _bi_clear_done

  mov byte ptr ds:[si], '$'
  jmp _bi_clear_loop
_bi_clear_done:
  ret

bigint_clear macro name
  errifndef &name
  mov si, offset &name
  call _bigint_clear
endm

;; == Big integer input/output ==

_bigint_print:
  ;; input: si -- address of big integer to print
  push si
  push di
  push dx

  xor dx, dx
_bi_print_sign:
  mov di, offset const_signs
  mov dl, byte ptr ds:[si]
  add di, dx
  mov al, byte ptr ds:[di]
  call print_char

  mov di, offset const_digits
_bi_skip_to_end:
  cmp byte ptr ds:[si], '$'
  je _bi_print_loop
  inc si
  jmp _bi_skip_to_end
_bi_print_loop:
  dec si
  mov dl, byte ptr ds:[si]

  cmp dl, 0EFh
  je _bi_print_done

  add di, dx
  mov al, byte ptr ds:[di]
  call print_char
  sub di, dx

  jmp _bi_print_loop
_bi_print_done:
  pop dx
  pop di
  pop si
  ret

bigint_read macro name
  xor ax, ax

  mov dx, offset _input_string
  mov ah, 0Ah
  int 21h

  call print_newline

  cmp byte ptr [_input_string+1], 0
  je _empty_string

  mov di, offset &name
  mov si, offset _input_string+2
  mov dl, byte ptr [_input_string+1]
  call _bigint_parse

  cmp ch, 0
  jne _parse_failed
endm

bigint_print macro name
  errifndef &name
  mov si, offset &name
  call _bigint_print
  call print_newline
endm

_parse_failed:
  mov dx, offset _parse_failed_message
  mov ah, 09h
  int 21h
  call print_newline

  mov ax, 4C00h
  int 21h

_empty_string:
  mov dx, offset _empty_string_message
  mov ah, 09h
  int 21h
  call print_newline

  mov ax, 4C00h
  int 21h

;; == Driver ==

start:
  mov ax, data
  mov ds, ax
  xor ax, ax

  bigint_read num1
  bigint_read num2

  bigint_print num1
  bigint_print num2

  bigint_sum num1, num2, result
  bigint_print result

  bigint_diff num1, num2, result
  bigint_print result

  bigint_prod num1, num2, result
  bigint_print result

  mov ax, 4C00h
  int 21h

code ends
end start
