; Matt Forbes
; CSCI 347 - Assignment 4
; Provides subprogram substr to find a substring in a string of text
; It takes 2 parameters, and uses 3 local variables on the stack

; Prototype:
; int substr(char *string, char *sub);

%include "asm_io.inc"

segment .data
segment .bss
segment .text
	global    substr

;int substr(char *string, char *sub);
;  Takes 2 strings, returning the index of the first occurrence
;  of the second string in the first, or -1 if not found
;Arguments:
;  str1: string to search                     [ebp+12]
;  str2: substring to match                   [ebp+8]
;Locals:
;  pos: position of the potential match       [ebp-4] 
;  reading : 1 if last character was a match  [ebp-8]
;  loc : current location in the main string  [ebp-12]

substr:
	enter     0,12

	; ebx points to main string
	; ecx points to substring
	mov	  ebx, [ebp+8]
	mov       ecx, [ebp+12]

	; initialize locals 
	mov	  dword [ebp-4], -1
	mov	  dword [ebp-8], 0
	mov	  dword [ebp-12], 0

substr_loop:
	; stopping cases - at end of string or substring
	cmp	  byte [ebx], 0
	je	  end_substr_loop
	cmp	  byte [ecx], 0
	je	  end_substr_loop

	; compare string with substring, handle depending on match
	mov	  byte al, [ebx]
	cmp	  al, [ecx]
	je        substr_match

substr_not_match:
	; set reading and pos because we are not matching
	mov	  dword [ebp-4], -1
	mov	  dword [ebp-8], 0
	mov	  ecx, [ebp+12]
	jmp	  substr_next_loop

substr_match:
	; if we weren't already matching, initialize state
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
	; if we exited, and substring was read to the end, we found a match
	cmp	  byte [ecx], 0
	je	  substr_return
	; otherwise we didn't find the whole substring
	mov	  dword [ebp-4], -1

substr_return:
	; return pos
	mov	  eax, [ebp-4]
	leave
	ret
	
	
	
		
