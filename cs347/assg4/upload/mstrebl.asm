; Matt Forbes
; CSCI 347
; Assignment 4
; Main program for taking a string and remove extraneous spaces from it
; Uses subprograms located in strebl.asm (get_line and remove_spaces)

%include "asm_io.inc"

segment .data
input_prompt db "Enter a string: ",0

segment .bss
input_str resb 80

segment .text
        global  asm_main
	extern  get_line, remove_spaces

asm_main:
        enter   0,0               ; setup routine
        pusha

main:
	mov	eax, input_prompt
	call	print_string
	
	; both getline and remove_spaces take the same argument, so it is reused
	push	input_str
	call	get_line
	call	remove_spaces
	pop     eax

	; the transformed string is now in eax ready to be printed
	call	print_string
	call    print_nl

        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret


