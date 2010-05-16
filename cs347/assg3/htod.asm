;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; file: htod.asm
; This program reads in a hexadecimal number and prints out
; the decimal equivalent.
;
; Input: A hexadecimal number (max 4 chars), letters in 
;        uppercase
;
; Output: The decimal value of the hexadecimal number.
;
; author
;
; date
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%include "asm_io.inc"

MAX_HEX     equ     7
LF	    equ	    0Ah

segment .data
;
; initialized data is put in the data segment here
;
in_prompt      DB  'Please input a hexadecimal number: ',0
out_msg        DB  'The decimal equivalent is: ',0
error_msg      db  "Cannot format this number to decimal",0

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

        mov     eax, in_prompt ; print prompt
	call    print_string ; 

	mov     byte [count], 0
	mov     word [pow_sixteen], 16

	mov     eax, [count]
	call    print_int
	mov     eax, LF
	call    print_char

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
	mov     eax, [count]
	call    print_int
	mov     eax, LF
	call    print_char

	mov     byte [ebx], 0 ; Add nul terminator to end of the string
	mov     ebx, hex_str ; Point ebx back to the front of hex_str so we can process it
	mov     word [hex_val], 0 ; Zero out hex_val
	dec	byte [count]
	mov     cx, 0
	mov     cx, [count]
	;imul    cx, 4

	mov     eax, 0
	mov     ax, cx
	call    print_int
	mov     eax, LF
	call    print_char
	shl     byte [pow_sixteen], cl
	
	mov     eax, [pow_sixteen]
	call    print_int
	mov     eax, LF
	call    print_char


process_hex_str:
	
	mov    byte al, [ebx] ; store the current character in the al for quick reference

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
	mov    ecx, [pow_sixteen]
	imul   ecx, [tmp]
	add    [hex_val], ecx
	
	
incr_loop:
	inc     ebx ; Increment iterator
	dec     byte [count] ; Decremement the count
	jmp     process_hex_str ; Jump back up to the top of the loop

		
process_hex_complete:
	mov     eax, hex_str 
	call    print_string
	mov     eax, LF
	call    print_char
	mov     eax, hex_val
	call    print_int
	jmp     quit

error:
	mov     eax, error_msg
	call    print_string
	mov     eax, LF
	call    print_char
	jmp     quit

	
quit:
        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret


