assume CS:code, DS:data

data segment
  byte_var db 10h
  word_var dw 1234h
  a db 10 dup(?)
  a2 dw 16 dup(12)
  constant equ 7331h
data ends

include "../utils.asm"

code segment
volume macro p1,p2,p3,p4,p5,p6,p7
  local total_size
  total_size = 0

  irp Z,<p1,p2,p3,p4,p5,p6,p7>
    ifnb <Z>
      ifdef Z
        if type Z
          total_size = total_size + (size Z)
        endif
      endif
    endif
  endm

  mov ax, total_size
endm

start:
  mov ax, data
  mov ds, ax
  xor ax, ax

  volume 1337h, byte_var, word_var, a, a2, constant, undefinded
  call print_number

  mov ah, 4Ch
  int 21h
code ends
end start
