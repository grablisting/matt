;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Matt Forbes
; CSCI347
; 5/12/2010
; Convert Hexadecimal to Decimal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

MAX_HEX     equ     5
LF	    equ	    0Ah

segment .data
;
; initialized data is put in the data segment here
;
in_prompt      DB  'Please input a hexadecimal number (max 4 digits): ',0
out_msg        DB  'The decimal equivalent is: ',0
error_msg      db  "Cannot format this number to decimal",0
again_prompt   db  "Do you want to terminate the program? (y/n) ",0 ; Prompt for exitting the program 

segment .bss
;
; uninitialized data is put in the bss segment
;

hex_str      resb  MAX_HEX
hex_val      resd  1
tmp	     resd  1
count        resb  1
pow_sixteen  resw  1

segment .text
        global  asm_main
asm_main:
        enter   0,0 ; setup routine
        pusha

main:
        mov     eax, in_prompt ; print prompt
	call    print_string ; 

	mov     byte [count], 0
	mov     word [pow_sixteen], 16
	mov     ebx, hex_str ; Point the ebx register at hex_str's address

read_hex_str:
	cmp     byte [count], MAX_HEX ; Check to see if the input was too big
	jg      read_error ; If it was too big, jump to error label

	call    read_char ; Get next character - byte is in the al register

	cmp     byte al, LF ; Check to see if we're reading a line feed
	je      read_complete ; When we hit the LF, we're done reading the string

	mov     byte [ebx], al ; Store the character in to hex_str (pointed at by ebx)

	inc     ebx ; Increment interator of hex_str
	inc     byte [count] ; Increment the count

	jmp     read_hex_str ; Go back and read the next character

read_error:
	jmp     error ; Jump to error label

read_complete:
	mov     byte [ebx], 0 ; Add nul terminator to end of the string
	mov     ebx, hex_str ; Point ebx back to the front of hex_str so we can process it

	mov     word [hex_val], 0 ; Zero out hex_val
	sub	byte [count], 2 ; Use 2 less than the number of items pushed when calculating the power of 16
	mov     cx, 0 ; zero out cx
	mov     cl, [count] ; move the new count value into the cl
	imul    cx, 4 ; multiply this number by 4, which gives us the number of shifts to apply to 16
	shl     word [pow_sixteen], cl ; shift pow_sixteen by this number, which raises it to the right power
	mov     ax, [pow_sixteen] ; move this value into the ax

process_hex_str:
	mov    eax, 0	; zero out the eax
	mov    al, [ebx] ; store the current character in the al for quick reference

	cmp    byte al, 0 ; Check for null terminator
	jne    continue_processing ; Keep processing if not end of string
	jmp    process_hex_complete ; We are done processing the string

continue_processing:	
	cmp    byte al, '9' ; Check bounds of digits, and jump to not_digit if it's not in range
	jg     not_digit ; Handle non-digit

	cmp    byte al, '0' ; Lower bound
	jl     not_digit ; Handle non-digit

digit:
	sub    byte al, '0' ; Simply get the numeric value for this character, keep it in al
	mov    byte [tmp], al ; store the decimal value in to tmp
	jmp    add_hex ; Jump down now that we have numeric value

not_digit:
	cmp    byte al, 'a' ; Check bounds of hex alphabet
	jl     not_lower ; This character is not a lower case hex character

	cmp    byte al, 'f' ; Check upper bound
	jg     not_lower; This character is not in bounds

	sub    byte al, 'a' ; Get offset from letter a
	add    byte al, 10 ; Add 10 to get hex value of this character

	mov    byte [tmp], al ; Store the characters hex value in tmp
	jmp    add_hex ; Add this characters hex to the result hex value

not_lower:
	cmp    byte al, 'A' ; Check bounds of hex alphabet for upper case
	jl     error ; Character is not valid hex, error out

	cmp    byte al, 'F' ; Check upper bound
	jg     error ; Character is not valid hex, error out

	sub    byte al, 'A' ; Get offset from 'A'
	add    byte al, 10 ; Add 10 get get hex value

	mov    byte [tmp], al ; Store hex value in tmp
	jmp    add_hex ; Add this characters hex to the result hex value

add_hex:
	mov    ax, 0 ; zero out the ax
	mov    al, [tmp] ; move the decimal value from above into al

	cmp    word [pow_sixteen], 0 ; special case when power of 16 is 0, we just want to add, not multiply
	je     add_it ; in this case, jump ahead

	mov    cx, [pow_sixteen] ; put the current power of sixteen into the cx
	imul   ax, cx ; multiply the decimal value by that power

add_it:
	add    word [hex_val], ax ; add the result to the hex value we are keeping track of
	
incr_loop:
	inc    ebx ; Increment iterator
	mov    cl, 4 ; put 4 in the cl
	shr    word [pow_sixteen], cl ; shift pow_sixteen left 4 times, lowering it's power by 1
	jmp    process_hex_str ; Jump back up to the top of the loop
		
process_hex_complete:
	mov     eax, out_msg ; print out message
	call    print_string 

	mov     eax, 0 ; zero out eax 
	mov     ax, [hex_val] ; put the calculate hex string into the ax
	call    print_int ; print it
	call    print_nl

	jmp     run_again ; see if user wants to run the program again

error:
	mov     eax, error_msg ; print the error message
	call    print_string
	mov     eax, LF
	call    print_char
	jmp     run_again
	
run_again: 
	mov    eax, again_prompt ; see if the user wants to run the program again
	call   print_string
	call   read_char
	cmp    al, 'y'
	je     quit

	cmp    al, 'Y'
	je     quit

clear_stdin:
	call   read_char ; Get left over characters
	cmp    al, LF ; Read until a LF
	jne    clear_stdin ;

	jmp    main

quit:
        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret


