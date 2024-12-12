assume CS:code, DS:data

data segment
  number db '00000$', 0
  dummy db 0Dh, 0Ah, '$'
data ends

code segment
to_string:
  push cx
  push dx
  mov cx, 5
  push ax
  xor ax, ax
  mov di, offset number
  rep stosb
  pop ax
  mov di, offset number + 5
to_string_loop:
  xor dx, dx
  mov cx, 10
  div cx
  add dl, '0'
  dec di
  mov [di], dl
  cmp ax, 0
  jne to_string_loop
  pop dx
  pop cx
  ret
hex_digit:
  add al, '0'
  cmp al, '9'
  jle hex_end
  sub al, '0'
  sub al, 10
  add al, 'A'
hex_end:
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
print_byte:
  push ax
  push cx
  mov ah, al
  mov cl, 4
  shr al, cl
  call hex_digit
  call print_char
  mov al, ah
  and al, 0Fh
  call hex_digit
  call print_char
  pop cx
  pop ax
  ret
print_word:
  push ax
  xchg al, ah
  call print_byte
  xchg al, ah
  call print_byte
  pop ax
  ret
print_word_array:
  push si
  push cx
  push ax
print_word_array_loop:
  lodsw
  call print_word
  mov al, ' '
  call print_char
  loop print_word_array_loop
  pop ax
  pop cx
  pop si
  ret
print_array:
  push si
  push cx
  push ax
print_array_loop:
  lodsw
  call print_number
  mov al, ' '
  call print_char
  loop print_array_loop
  pop ax
  pop cx
  pop si
  ret
print_number:
  push di
  push ax
  push cx
  push dx
  call to_string
  mov ah, 09h
  mov dx, di
  int 21h
  pop dx
  pop cx
  pop ax
  pop di
  ret
print_newline:
  push ax
  push dx
  mov dx, offset dummy
  mov ah, 09h
  int 21h
  pop dx
  pop ax
  ret
code ends
