%include "asm_io.inc"

segment .data
segment .bss
segment .text
	global    substr

;substr:
;  Takes 2 strings, returning the index of the first occurrence
;  of the second string in the first, or -1 if not found
;arguments:
;  str1: string to search                    [ebp+12]
;  str2: substring to match                  [ebp+8]
;locals:
;  pos: position of the potential match      [ebp-4] 
;  reading : 1 if last character was a match [ebp-8]
;  loc : current location in the main string [ebp-12]
substr:
	enter     0,12

	; ebx points to main string
	mov	  ebx, [ebp+8]
	mov       ecx, [ebp+12]

	; initialize locals 
	mov	  dword [ebp-4], -1
	mov	  dword [ebp-8], 0
	mov	  dword [ebp-12], 0

substr_loop:
	; stopping cases
	cmp	  byte [ebx], 0
	je	  end_substr_loop

	cmp	  byte [ecx], 0
	je	  end_substr_loop

	mov	  byte al, [ebx]
	cmp	  al, [ecx]
	je        substr_match

substr_not_match:
	mov	  dword [ebp-4], -1
	mov	  dword [ebp-8], 0
	mov	  ecx, [ebp+12]
	jmp	  substr_next_loop

substr_match:
	cmp	  dword [ebp-8], 1
	je	  substr_match_continue
	mov	  eax, [ebp-12]
	mov	  dword [ebp-4], eax
	mov	  dword [ebp-8], 1 
substr_match_continue:
	inc	  ecx
	
substr_next_loop:
	inc	  ebx
	inc	  dword [ebp-12]
	jmp	  substr_loop

end_substr_loop:
	cmp	  byte [ecx], 0
	je	  substr_return
	mov	  dword [ebp-4], -1

substr_return:
	mov	  eax, [ebp-4]
	leave
	ret
	
	
	
		
