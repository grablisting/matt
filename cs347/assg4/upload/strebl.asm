; Matt Forbes
; CSCI 347
; Helper subprograms for file mstrebl.asm
; Exports procedures getline and remove_spaces
; Internal procedure strcpy to copy one string into another address

;Protoypes
; remove_spaces(char *str);
; get_line(char *str);
; strcpy(char *destination, char *source);

%include "asm_io.inc"

LF       equ     0AH

segment .data

segment .bss
tmp_str		resb	80

segment .text
	global remove_spaces, get_line

; remove_spaces:
; remove leading/trailing/duplicate spaces in string
; uses a tmp_str in .bss segment to store intermediary string
; arguments:
; arg1 - string to clean - [ebp+8]
remove_spaces:
	enter     0,0
	
	; store pointers to input string, and tmp_str
	mov		ebx, [ebp+8] 
	mov		ecx, tmp_str 

skip_spaces:
	; stopping case
	cmp		byte [ebx], ' '
	jne		remove_loop

	inc		ebx 
	jmp		skip_spaces
	
remove_loop: 
	; stopping case
	cmp		byte [ebx], 0 
	je		end_remove_loop
	
	; copy character from input to tmp_string
	mov		al, [ebx] ; 
	mov		[ecx], al

	; increment pointers
	inc		ebx 
	inc     ecx

	; if read character is space, skip the next spaces in string
	cmp		al, ' ' ; skip extra spaces
	je		skip_spaces
	jmp		remove_loop
	
end_remove_loop:
	; making call to strcpy
	push	dword [ebp+8] ; destination
	push	tmp_str ; source
	call	strcpy ; strcpy(input_str, tmp_str)
	pop		ecx 
	pop		ecx
	
	leave
	ret
	

; strcpy:
; copies one string into another
; Arguments:
; arg1: destination - [ebp+12]
; arg2: source - [ebp+8]
strcpy:
	enter   0,0
	
	; setup points to destination and source
	mov 	ebx, [ebp+12] 
	mov 	ecx, [ebp+8]
	
copy_loop:
	; stopping case
	cmp 	byte [ecx], 0 
	je  	end_copy_loop
	
	; copy 
	mov 	al, [ecx] 
	mov 	[ebx], al
	
	inc 	ebx 
	inc 	ecx
	jmp     copy_loop
	
end_copy_loop:
	; store nul terminator
	mov		byte [ebx], 0 
	
	leave
	ret


; get_line:
; reads a string into arg1 up to end of line
; Arguments:
; arg1 - address of destination - [ebp+8]
get_line:
	enter   0,0
	
	; pointer to destination
	mov     ebx, [ebp+8]

read_loop:
	; store next character from stdin
	call    read_char
	mov     [ebx], eax

	; stopping case
	cmp     eax, LF
	je      end_read_loop

	inc     ebx
	jmp     read_loop

end_read_loop:
	; store nul terminator
	mov     byte [ebx], 0

	leave
	ret

