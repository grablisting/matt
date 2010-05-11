
%include "asm_io.inc"

BASE     equ     16
LF       equ     0AH

segment .data
;
; initialized data is put in the data segment here
;
prompt1        db  "Enter a decimal to convert to binary: ",0
again_prompt   db  "Do you want to terminate the program? (y/n) ",0 ; Prompt for exitting the program 
hex_table      db  "0123456789ABCDEF"    


segment .bss
;
; uninitialized data is put in the bss segment
;
original   resd  1
quotient   resd  1
count      resb  1
output     resb  10


segment .text
        global  asm_main
asm_main:
        enter   0,0 ; setup routine
        pusha

;
; code is put in the text segment. Do not modify the code before
; or after this comment.
;

main:
	mov     eax, prompt1
	call    print_string

	call    read_int
	mov     dword [original], eax ; Store original input 

	mov     byte [count], 0
	mov     [quotient], eax

	cmp     dword [quotient], 0
	jne     divide_loop

	push    dword 0
	inc     byte [count]
	jmp     end_divide_loop

divide_loop:
	cmp     dword [quotient], 0
	je      end_divide_loop

	mov     edx, 0 ; clear edx for division
	mov     dword eax, [quotient] ; move quotient into eax for division
	mov     dword ecx, BASE ; use ecx register as source for division

	div     ecx ; divide current quotient by base (16)
	
	push    dword edx ; push the remainder for reading later

	mov     dword [quotient], eax ; store the result of the division in to quotient

	inc     byte [count] ; keep track of how many things we've pushed on the stack

	jmp divide_loop ; Loop it

end_divide_loop:

	mov     ebx, output ; let ebx point to output string

store_loop:
	cmp     byte [count], 0
	je      end_store_loop

	pop     edx
	dec     byte [count] ; Decrement counter because we popped the stack
	
	mov     ecx, hex_table ; ecx will point to hex_table as an array
	add     ecx, edx ; add the offset found from edx

	mov     byte al, [ecx]
	mov     byte [ebx], al ; store the byte from ecx
	
	inc     ebx ; increment pointer to ebx, to look at the next character

	jmp     store_loop ; loop back up 

end_store_loop:
	mov     byte [ebx], 0 ; store nul terminator
	mov     eax, output
	call    print_string
	call    print_nl

run_again:
	call    clear_stdin
	mov     eax, again_prompt
	call    print_string
	call    read_char
	cmp     al, 'y'
	je      quit
	cmp     al, 'Y'
	je      quit

	call    clear_stdin
	jmp     main


quit:
        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret

clear_stdin:
	call   read_char ; Get left over characters
	cmp    al, LF ; Read until a LF
	jne    clear_stdin ;
	ret

