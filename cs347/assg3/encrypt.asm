;
; file: skel.asm
; This file is a skeleton that can be used to start assembly programs.

%include "asm_io.inc"
LF	equ	0Ah

segment .data
;
; initialized data is put in the data segment here
;
enc_table db "4695031872"
intro_msg db "Encryption program:",LF,0 ; First message to display
finished_msg db "Encrypted string: ", 0 ; To display after encryption
prompt1 db "Enter a string to encrypt: ",0 ; prompt for string

segment .bss
;
; uninitialized data is put in the bss segment
;
to_encrypt resb 256

 

segment .text
        global  asm_main
asm_main:
        enter   0,0               ; setup routine
        pusha

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print Program Message
	mov    eax, intro_msg ; Get intro message ready for printing
	call   print_string

	mov    eax, prompt1
	call   print_string

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Fill up to_encrypt string
	mov    ebx, to_encrypt ; point ebx at the to_encrypt label
get_char_loop:
	call   read_char ; Get a character
	mov    [ebx], eax ; Move the result of get_char to the position in to_encrypt that ebx points to

	cmp    byte [ebx], LF ; Compare the last character read with the carriage return
	je     end_char_loop ; End loop when we hit carriage return

	inc    ebx ; Otherwise, increment ebx to look at next char in to_encrypt
	jmp    short get_char_loop ; jump up and read another character 

end_char_loop:
	mov    byte [ebx], 0 ; Nul terminate string to_encrypt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Encrypt string to_encrypt
	mov    ebx, to_encrypt ; use ebx again as iterator
encrypt_loop:
	mov    al, [ebx] ; Move the currect character we're inspecting to the al register

	cmp    al, 0 ; Check if we're at the end of the string by checkign if it's a nul terminator
	je     end_encrypt_loop ; If we hit the end of the string, jump to end of the loop

	cmp    byte [ebx], '9' ; Check bounds of digits, if greater than 9 ignore it
	jg     not_digit

	cmp    byte [ebx], '0' ; If less than 0 ignore it
	jl     not_digit

handle_digit:
	sub    al, '0' ; Get the current character's offset from char position '0'
	mov    edx, enc_table ; Have edx register point to enc_table[0]
	add    edx, eax ; Add the character's offset to enc_table[0].
	mov    al, [edx] ; Store the encrypted value from enc_table back in to al
	mov    [ebx], al ; Finally, store the encrypted value in this position of to_encrypt

not_digit:
	inc    ebx ; Increment iterator to next position in to_encrypt
	jmp    short encrypt_loop ; Jump back up to the top of the encryption loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Finished encryption, now print results
end_encrypt_loop:
	
	mov    eax, finished_msg ; 
	call   print_string;

	mov    eax, to_encrypt ; Move our newly-encrypted string into the eax for printing
	call   print_string ; Print it

	mov    eax, LF ; Just print a LF to make it look better
	call   print_char

        popa
        mov     eax, 0            ; return back to C
        leave                     
        ret


