assume DS:data, CS:code

data segment
arr dw 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1
len db $ - arr
new dw 100 dup (0)
max dw 0
data ends

include "../utils.asm"

code segment
func:
  and bx, ax
  cmp bx, 0
  jne good
bad:
  mov dx, ax
  ret
good:
  add dx, 1
  mov ax, dx
  ret
start:
  mov ax, data
  mov ds, ax
  mov es, ax
  xor ax, ax
  xor dx, dx
  xor cx, cx

  mov cl, len
  shr cx, 1

  lea si, arr
  lea di, new
iterate:
  lodsw
  push ax
  call func
  pop bx
  stosw
  loop iterate

  mov cl, len
  shr cx, 1

  lea si, new

  call print_array
  call print_newline

  lea di, max
array_max:
  lodsw
  cmp ax, [di]
  jle skip
  mov [di], ax
skip:
  loop array_max

  mov ax, word ptr [max]
  call print_number

  mov ah, 4Ch
  int 21h
code ends
end start
