assume CS:code,DS:data

data segment
  a equ 12
  b equ 14
  c equ 3
  d equ 17
data ends

; c - (a + b) / 2 + d

code segment
hex_digit:
  add al, '0'
  cmp al, '9'
  jle hex_end
  sub al, '0'
  add al, 'A'
hex_end:
  ret
print_char:
  push ax
  mov dl, al
  mov ah, 02h
  int 21h
  pop ax
  ret
print_byte:
  push ax
  mov ah, al
  mov cl, 4
  shr al, cl
  call hex_digit
  call print_char
  mov al, ah
  and al, 0Fh
  call hex_digit
  call print_char
  pop ax
  ret
start:
  mov al, a
  mov bl, b
  add al, bl
  shr al, 2
  mov bl, c
  sub bl, al
  mov al, d
  add al, bl
  call print_byte ; byte in "al"
  mov ah, 4Ch
  int 21h
code ends
end start
