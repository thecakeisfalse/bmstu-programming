assume CS:code, DS:data

data segment
  string db 100, 99 dup('$')
data ends

include "../utils.asm"

code segment
strlen proc
  push bp
  mov bp, sp

  mov di, [bp+4]
  xor cx, cx
strlen_loop:
  cmp byte ptr [di], 0Dh
  je strlen_end
  inc cx
  inc di
  jmp strlen_loop
strlen_end:
  pop bp
  pop bx
  mov ax, cx
  push ax
  push bx
  ret
strlen endp
start:
  mov ax, data
  mov ds, ax
  xor ax, ax

  mov ah, 0Ah
  mov dx, offset string
  int 21h

  call print_newline

  mov dx, offset string + 2
  push dx
  call strlen
  pop ax

  call print_number

  mov ah, 4Ch
  int 21h
code ends
end start
