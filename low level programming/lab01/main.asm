%include "../utils.inc"

section .text

global _start
_start:
  mov al, a
  mov bl, b
  add al, bl
  shr al, 1
  mov bl, c
  sub bl, al
  mov al, d
  add al, bl
  call print_byte
  mov al, 10
  call print_char
  call exit

section .data
a equ 12
b equ 14
c equ 3
d equ 17
