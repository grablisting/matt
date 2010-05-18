%include "asm_io.inc"

LF       equ     0AH

segment .bss
tmp_str		resb	80

segment .text
	global remove_spaces, getline

; remove_spaces - remove leading/trailing/duplicate spaces in string
; arg1 - string to clean - [ebp+4]
remove_spaces:
	push	ebp
	
	mov		ebx, [ebp+4] ; store pointer to arg_str
	mov		ecx, tmp_str ; pointer to tmp string
	
; read through ebx until hitting a non whitespace char
skip_spaces:
	cmp		byte [ebx], ' ' ; stopping case
	jne		remove_loop
	inc		ebx 
	jmp		skip_spaces
	
; remove spaces from ebx, store new string in ecx
remove_loop: 
	cmp		byte [ebx], LF ; stopping case
	je		end_remove_loop
	
	mov		al, [ebx] ; copy instructions
	mov		[ecx], al
	inc		ebx ; byte has been copied, ok to inc
	cmp		al, ' ' ; skip extra spaces
	je		skip_spaces
	jmp		remove_loop
	
end_remove_loop:

	; making call to strcpy
	push	dword [ebp+4] ; destination
	push	tmp_str ; source
	call	strcpy
	pop		ecx ; remove args from stack
	pop		ecx
	
	pop		ebp
	ret
	

; strcpy - copies one string into another
; arg1: destination - [ebp+4]
; arg2: source - [ebp+8]
strcpy:
	push	ebp
	
	mov 	ebx, [ebp+4] ; destination
	mov 	ecx, [ebp+8] ; source
	
copy_loop:
	cmp 	byte [ecx], 0 ; stopping case
	je  	end_copy_loop
	
	mov 	al, [ecx] ; copy instructions
	mov 	[ebx], al
	
	inc 	ebx ; continue reading strings
	inc 	ecx
	
end_copy_loop:
	mov		byte [ebx], 0 ; store nul terminator
	
	pop ebp
	ret


; getline - reads a string into arg1 up to end of line
; arg1 - address of destination - [ebp+4]
getline:
	push     ebp

	mov     ebx, [ebp+4] ; point ebx at our destination

read_loop:
	call    read_char
	cmp     byte al, LF
	je      end_read_loop

	mov     byte [ebx], al
	inc     ebx
	jmp     read_loop

end_read_loop:
	mov     byte [ebx], 0 ; nul terminator

	pop ebp
	ret

