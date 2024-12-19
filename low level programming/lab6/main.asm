.386p

;; Real mode

assume CS:code_r, SS:stack_r

calculate_linear_address macro segm, name
    xor eax, eax
    mov ax, &segm
    shl eax, 4
    add eax, offset &name
endm

code_r segment para public 'CODE' use16
@start:
    ;; clear screen
    mov ax, 03h
    int 10h

    ;; enable A20
    in al, 92h
    test al, 2
    jnz skip_fast_a20
    or al, 2
    out 92h, al
skip_fast_a20:

    ;; calculate linear address of @pm_start
    calculate_linear_address code_p, @pm_start
    mov dword ptr entry_point, eax

    ;; calculate linear address and load GDT
    calculate_linear_address code_r, GDT
    mov dword ptr GDTR+2, eax
    lgdt fword ptr GDTR

    ;; clear masked and unmasked interrupts
    cli

    in al, 70h
    or al, 80h
    out 70h, al

    ;; swtich to protected mode
    mov eax, cr0
    or al, 1
    mov cr0, eax

    ;; load new segment
    db 66h, 0EAh
    entry_point dd ?
    dw 1 * 8 ;; code selector (#1)

;; Global Descriptor Table (GDT)

;; Descriptor structure
;; --------------------
;; Limit [0:15] | Base [0:23] | P (1) | DPL (2) | S (1) | Type (3) | A (1) | G (1) | D/B (1) | ZERO | AVL (1) | Limit [16:19] | Base [24:31]

;; Descriptor `Type`
;; -----------------
;; 
;; 1st bit - 1 = code, 0 = data;
;; 2nd bit - expansion direction (0 = as usual, 1 = like in stack);
;; 3rd bit - write-enable (0 = disallowed, 1 = allowed);
;;
;;  0 | 1 | 2
;; ---|---|---
;;  0 | ? | 0 --> Read-only
;;  0 | ? | 1 --> Read/write
;; ---|---|---
;;  1 | 0 | 0  \
;;  1 | 0 | 1   --> Only execute
;;  1 | 1 | 0  /
;;  1 | 1 | 1 --> Execute and read?

GDT:  
NULL_descr  db      8 dup(0)
CODE_descr  db      0FFh,0FFh,00h,00h,00h,10011010b,11001111b,00h
DATA_descr  db      0FFh,0FFh,00h,00h,00h,10010010b,11001111b,00h
VIDEO_descr db      0FFh,0FFh,00h,80h,0Bh,10010110b,01000000b,00h
JFF_descr   db      031h,000h,37h,13h,31h,01111101b,11111001b,73h

;; GDTR information
;; ----------------
;; * 2 bytes for table limit (table size in bytes - 1) = 8*N-1
;; * 4 bytes for linear address

GDTR dw $-GDT-1, 0, 0

code_r ends

stack_r segment para stack 'STACK' use16
    db 100h dup(?)
stack_r ends

;; Protected mode

assume CS:code_p, DS:data_p

code_p segment para public 'CODE' use32

@pm_start:
    ;; load data descriptor selector (#2)
    mov ax, 2 * 8
    mov ds, ax
    mov es, ax
 
    ;; create page catalog
    mov edi, 100000h ;; 1 MB
    mov eax, 101007h ;; 1 MB + 4 KB
                     ;; 7 = 111 (bit P    - present in memory,
                     ;;          bit R/W  - read/write,
                     ;;          bit U/S  - available for all privileges)

    ;; load first element
    stosd

    ;; load other 1023 elements
    mov ecx, 3FFh
    xor eax, eax
    rep stosd

    ;; fill page table
    mov eax, 07h
    mov ecx, 1024

fill_page_table:
    stosd
    add eax, 1000h ;; 4 KB per page
    loop fill_page_table

    mov eax, 100000h
    mov cr3, eax

    ;; enable page addressing
    mov eax, cr0
    or eax, 80000000h
    mov cr0, eax

update_video_memory_address:
    mov eax, 0B8007h
    mov ES:00101000h+031h*4,eax

@main:
    sgdt fword ptr temp_gdtr

    ;; init "screen"

    mov word ptr ds:[screen_cursor_pos], 0

    mov dl, VGA_COLOR_BLACK
    mov dh, VGA_COLOR_LIGHT_GREY
    call screen_set_color

    ; irpc ch, <GDT Limit: >
    ;     mov al, '&ch&'
    ;     call screen_putchar
    ; endm

    mov ax, word ptr [temp_gdtr] ;; limit
    ; call screen_print_word

    inc ax
    mov cl, 3
    shr ax, cl
    mov cx, ax

    ; call screen_print_newline

    ; irpc ch, <GDT linear address: >
    ;     mov al, '&ch&'
    ;     call screen_putchar
    ; endm

    mov eax, dword ptr [temp_gdtr+2] ;; base
    ; call screen_print_dword
    mov ebx, eax

    ; call screen_print_newline

    xor edi, edi
_loop_gdt:
    cmp di, cx
    jge _loop_end

    ;; descriptor

    mov dx, di
    mov dh, VGA_COLOR_BLACK
    add dl, 9
    xchg dh, dl
    call screen_set_color

    irpc ch, <Descriptor #>
        mov al, '&ch&'
        call screen_putchar
    endm

    mov ax, di
    add al, '0'
    call screen_putchar

    irpc ch, <: >
        mov al, '&ch&'
        call screen_putchar
    endm

    ;; descriptor structure

    push ecx
    push edx

    mov ecx, dword ptr [ebx+4]

    ;; ZERO = 0010|0000 = 200000h

    test ecx, 200000h
    jz _skip_valid

    
    mov dl, VGA_COLOR_RED
    mov dh, VGA_COLOR_WHITE
    call screen_set_color

    irpc ch, <invalid>
        mov al, '&ch&'
        call screen_putchar
    endm

    mov dx, di
    mov dh, VGA_COLOR_BLACK
    add dl, 9
    xchg dh, dl
    call screen_set_color

_skip_valid:
    mov edx, dword ptr [ebx]

    call screen_print_newline

    ;; -*- base -*-

    irpc ch, <Base: >
        mov al, '&ch&'
        call screen_putchar
    endm

    rol ecx, 8
    mov ax, cx
    xchg al, ah
    ror ecx, 8
    call screen_print_word

    rol edx, 16
    mov ax, dx
    ror edx, 16
    call screen_print_word

    call screen_print_newline
    ;; -*- limit -*-

    irpc ch, <Limit: >
        mov al, '&ch&'
        call screen_putchar
    endm

    mov eax, ecx
    and eax, 0F0000h
    mov ax, dx

    ;; Granularity bit = 1000|0000b --> 800000h

    test ecx, 800000h
    jz _Granularity_skip

    shl eax, 12
    or eax, 0FFFh
_Granularity_skip:
    call screen_print_dword

    call screen_print_newline

    ;; Type bits = 0000|1110b ---> 0E00h

    mov eax, ecx
    and eax, 0E00h
    shr eax, 9

    test al, 0100b ;; check `code` or `data` segment
    jnz _Type_code

    push ax

    irpc ch, <Type: data, >
        mov al, '&ch&'
        call screen_putchar
    endm

    pop ax

    test al, 1 ;; 1 --> read/write, 0 --> read-only
    jz _Type_data_ro

    push ax
    irpc ch, <read/write>
        mov al, '&ch&'
        call screen_putchar
    endm
    pop ax

    jmp _Type_data_dir
_Type_data_ro:
    push ax

    irpc ch, <read-only>
        mov al, '&ch&'
        call screen_putchar
    endm

    pop ax

    jmp _Type_data_dir
_Type_data_dir:
    test al, 10b ;; 1 --> down expansion
    jz _Type_end

    irpc ch, <, >
        mov al, '&ch&'
        call screen_putchar
    endm

    irpc ch, <down expansion>
        mov al, '&ch&'
        call screen_putchar
    endm

    jmp _Type_end
_Type_code:
    push ax

    irpc ch, <Type: code, execute>
        mov al, '&ch&'
        call screen_putchar
    endm

    pop ax

    xor al, 11b ;; 11 --> 00
    test al, 11b ;; 0 --> execute & read?

    jnz _Type_code_cons

    push ax

    irpc ch, <& read>
        mov al, '&ch&'
        call screen_putchar
    endm

    pop ax
_Type_code_cons:
    test al, 10b
    jnz _Type_end

    irpc ch, <, >
        mov al, '&ch&'
        call screen_putchar
    endm

    irpc ch, <consistent>
        mov al, '&ch&'
        call screen_putchar
    endm
_Type_end:
    irpc ch, <, >
        mov al, '&ch&'
        call screen_putchar
    endm

    ;; Type bits = 0000|0001b ---> 0100h

    test ecx, 0100h
    jnz _Accessed

    irpc ch, <not accessed>
        mov al, '&ch&'
        call screen_putchar
    endm

    jmp _Accessed_end
_Accessed:
    irpc ch, <accessed>
        mov al, '&ch&'
        call screen_putchar
    endm
_Accessed_end:
    irpc ch, <. >
        mov al, '&ch&'
        call screen_putchar
    endm

    irpc ch, <Priv level: >
        mov al, '&ch&'
        call screen_putchar
    endm

    mov eax, ecx
    and eax, 6000h ;; DPL = 0110|0000 --> 6000h
    ror eax, 13

    call screen_print_byte

    irpc ch, <. >
        mov al, '&ch&'
        call screen_putchar
    endm

    call screen_print_newline ;; ради хотя бы минимальной красоты...

    ;; P = 1000|0000 --> 8000h

    test ecx, 8000h
    jz _Pim_failed

    irpc ch, <Present in RAM. >
        mov al, '&ch&'
        call screen_putchar
    endm

    jmp _Pim_check_end
_Pim_failed:
    irpc ch, <Not in RAM. >
        mov al, '&ch&'
        call screen_putchar
    endm
_Pim_check_end:

    irpc ch, <User bit: >
        mov al, '&ch&'
        call screen_putchar
    endm

    ;; AVL = 0001|0000 = 100000h

    mov eax, ecx
    and eax, 100000h
    ror eax, 20
    add al, '0'

    call screen_putchar

    irpc ch, <, Default size: >
        mov al, '&ch&'
        call screen_putchar
    endm

    ;; D/B = 0100|0000 = 400000h

    test ecx, 100000h

    jnz _Size_check_32

    irpc ch, <16>
        mov al, '&ch&'
        call screen_putchar
    endm

    jmp _Size_check_end
_Size_check_32:
    irpc ch, <32>
        mov al, '&ch&'
        call screen_putchar
    endm
_Size_check_end:
    irpc ch, < bits.>
        mov al, '&ch&'
        call screen_putchar
    endm

    ;; -*-*-*-*-

    call screen_print_newline

    pop edx
    pop ecx

    inc di
    add ebx, 8
    jmp _loop_gdt
_loop_end:

    jmp $

;; -----

screen_set_color:
    ;; input: dl - background color,
    ;;        dh - foreground color

    shl dl, 4
    or dl, dh

    mov ds:[screen_color], dl
    ret


screen_calc_index:
    ;; input: (dl, dh) = (y, x)
    ;; output: dx - offset in video memory

    push ax
    xor ax, ax

    mov al, 80
    mul dl ;; --> ax

    xor dl, dl
    xchg dh, dl ;; fix (16, 3) overflow

    add ax, dx
    xchg dx, ax
    shl dx, 1

    pop ax
    ret


screen_put_at:
    ;; input: (dl, dh) = (y, x),
    ;;        al - symbol to print

    push eax
    push edx
    push edi

    xor edi, edi
    and edx, 0FFFFh

    mov edi, DEFAULT_VGA

    call screen_calc_index
    add edi, edx

    mov ah, byte ptr ds:[screen_color]
    stosw

    pop edi
    pop edx
    pop eax
    ret


screen_print_newline:
    push edx
    mov dx, word ptr ds:[screen_cursor_pos]

    mov dh, 0
    inc dl
    cmp dl, VGA_HEIGHT
    jl _screen_update_cursor

    mov dl, 0
    jmp _screen_update_cursor
screen_putchar:
    push edx
    ;; input: al - symbol to print

    mov dx, word ptr ds:[screen_cursor_pos]
    call screen_put_at

    inc dh
    cmp dh, VGA_WIDTH
    jl _screen_update_cursor

    mov dh, 0
    inc dl
    cmp dl, VGA_HEIGHT
    jl _screen_update_cursor

    mov dl, 0
_screen_update_cursor:
    mov word ptr ds:[screen_cursor_pos], dx

    pop edx
    ret


screen_print:
    ;; input: cx - length of input
    ;;        si - input location

    push ecx
    push esi
    push edx
    push ax
_screen_print_loop:
    mov al, byte ptr ds:[esi]
    call screen_putchar

    inc esi
    loop _screen_print_loop

    pop ax
    pop edx
    pop esi
    pop ecx
    ret

strlen:
    ;; input:  si - input location
    ;; output: cx - length of input

    push esi
    push ax

    xor cx, cx
_strlen_loop:
    mov al, byte ptr ds:[esi]
    cmp al, 0
    je _strlen_done

    inc esi
    inc cx
    jmp _strlen_loop
_strlen_done:
    pop ax
    pop esi

    ret


screen_print_string:
    ;; input: si - string location

    call strlen
    call screen_print
    ret


screen_print_byte:
    ;; input: al - byte to print

    push ax
    push cx

    mov ah, al
    mov cl, 4
    shr al, cl
    call _hex_digit
    call screen_putchar

    mov al, ah
    and al, 0Fh
    call _hex_digit
    call screen_putchar

    pop cx
    pop ax
    ret

_hex_digit:
    add al, '0'
    cmp al, '9'
    jle _hex_end
    sub al, '0'
    sub al, 10
    add al, 'A'
_hex_end:
    ret


screen_print_word:
    ;; input: ax - word to print

    xchg al, ah
    call screen_print_byte
    xchg al, ah
    call screen_print_byte
    ret


screen_print_dword:
    ;; input: eax - dword to print

    rol eax, 16
    call screen_print_word
    rol eax, 16
    call screen_print_word
    ret


code_p ends
 
data_p segment para public 'DATA' use32
    assume CS:data_p

    VGA_WIDTH equ 80
    VGA_HEIGHT equ 25

    VGA_COLOR_BLACK equ 0
    VGA_COLOR_BLUE equ 1
    VGA_COLOR_GREEN equ 2
    VGA_COLOR_CYAN equ 3
    VGA_COLOR_RED equ 4
    VGA_COLOR_MAGENTA equ 5
    VGA_COLOR_BROWN equ 6
    VGA_COLOR_LIGHT_GREY equ 7
    VGA_COLOR_DARK_GREY equ 8
    VGA_COLOR_LIGHT_BLUE equ 9
    VGA_COLOR_LIGHT_GREEN equ 10
    VGA_COLOR_LIGHT_CYAN equ 11
    VGA_COLOR_LIGHT_RED equ 12
    VGA_COLOR_LIGHT_MAGENTA equ 13
    VGA_COLOR_LIGHT_BROWN equ 14
    VGA_COLOR_WHITE equ 15

    DEFAULT_VGA equ 031000h

    screen_color db 0
    screen_cursor_pos dw 0

    temp_gdtr dd 0, 0, 0
data_p ends

end @start
