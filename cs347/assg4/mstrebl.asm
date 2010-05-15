input_prompt db "Enter a string: ",0

input_str resb 80

main:
	mov		eax, input_prompt
	call	print_string
	
	push	input_str
	call	get_str
	call	remove_spaces
	
	mov		eax, input_str
	call	print_string
	
	