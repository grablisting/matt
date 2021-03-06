%include "asm_io.inc"

segment .data
input_prompt db "Enter a string: ",0

segment .bss
input_str resb 80

segment .text
        global  asm_main

asm_main:
        enter   0,0               ; setup routine
        pusha

main:
	mov	eax, input_prompt
	call	print_string
	
	push	input_str
	call	get_str
	call	remove_spaces
	
	mov	eax, input_str
	call	print_string


        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret


