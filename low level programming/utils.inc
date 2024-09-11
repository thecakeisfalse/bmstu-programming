section .data

; Constants
NULL         equ 0
EXIT_SUCCESS equ 0
STDOUT       equ 1

; System call codes

SYS_exit equ 60
SYS_write equ 1

section .text

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
  xor ah, ah
  push ax
  mov rax, SYS_write
  mov rdi, STDOUT
  mov rdx, 1
  mov rsi, rsp
  syscall
  pop ax
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
  and al, 0x0F
  call hex_digit
  call print_char
  pop ax
  ret

exit:
  mov rax, SYS_exit
  xor rdi, rdi
  syscall
