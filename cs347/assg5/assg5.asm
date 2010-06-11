; Matt Forbes
; CSCI 347 - Assignment 5
; May 2010
; Matrix procedures, read matrix, write matrix, transpose matrix, max element in matrix

%include "asm_io.inc"

; Prototypes
; void get_matrix_size(int *rows, int *cols);
; void read_matrix(int **matrix);
; void print_matrix(int rows, int cols, int **matrix);
; void transpose_matrix(int **matrix, int **t_matrix);
; void max_matrix(int **matrix, int *max, int *max_row, int *max_col);
; void print_matrix_elem(int elem, int row, int col);

segment .data
	m_rows		dd		0
	m_cols		dd		0

	row_prompt  db "Enter number of rows: ",0
	col_prompt  db "Enter number of columns: ",0
	mat_prompt  db "Enter Matrix data: ",0
	norm_output db "Original Matrix: ",0
	tran_output db "Transposed Matrix: ",0
	max_output db "Maximum Element: ",0
	elem_output db " at matrix",0
	

segment .bss
	matrix		resd	100
	t_matrix	resd	100
	max 		resd	1
	max_r		resd	1
	max_c		resd	1

segment .text
	global   asm_main

	
asm_main:
	enter    0,0
	pusha

	; Get size of matrix
	push     m_rows
	push	 m_cols
	call	 get_matrix_size
	add		 esp, 8

	; Read matrix
	push     dword [m_rows]
	push     dword [m_cols]
	push	 matrix
	call	 read_matrix
	add		 esp, 12

	; Print matrix
	mov		 eax, norm_output
	call     print_string
	call     print_nl
	push     dword [m_rows]
	push     dword [m_cols]
	push	 matrix
	call	 print_matrix
	add	 	 esp, 12

	; Transpose matrix
	push     dword [m_rows]
	push     dword [m_cols]
	push	 matrix
	push	 t_matrix
	call	 transpose_matrix
	add		 esp, 20	

	; Print transposed matrix
	mov		 eax, tran_output
	call     print_string
	call     print_nl
	push     dword [m_cols]
	push     dword [m_rows]
	push	 t_matrix
	call	 print_matrix
	add		 esp, 12

	; Find max element in matrix 
	push	 matrix
	push	 dword [m_rows]
	push	 dword [m_cols]
	push	 max
	push	 max_r
	push	 max_c
	call	 max_matrix
	add		 esp, 24

	; Print max element
	mov		 eax, max_output
	call     print_string
	push	 dword [max]
	push	 dword [max_r]
	push	 dword [max_c]
	call	 print_matrix_elem
	add		 esp, 12

	call	 print_nl

	popa
	mov		 eax, 0
	leave
	ret

; void get_matrix_size(int *rows, int *cols);
; int *rows = [ebp+12]
; int *cols = [ebp+8]
; Prompts and retrieves row size from user

get_matrix_size:
	enter    0,0
	
	mov      eax, row_prompt
	call     print_string

	call     read_int
	mov      ebx, [ebp+12]
	mov      dword [ebx], eax

	mov      eax, col_prompt
	call     print_string

	call     read_int
	mov		 ebx, [ebp+8]
	mov      dword [ebx], eax

	leave
	ret

; void read_matrix(int rows, int cols, int **matrix);
; int rows     = [ebp+16]
; int cols     = [ebp+12]
; int** matrix = [ebp+8]
read_matrix:
	enter    0,0

	; setup pointer to matrix in ebx, and counter for rows in cl
	mov      ebx, [ebp+8]
	mov      ecx, [ebp+16]

read_row:
	mov      edx, [ebp+12]
read_col:
	call	 read_int
	mov		 dword [ebx], eax
	dec		 edx
	add		 ebx, 4
	cmp		 edx, 0
	jne      read_col

	dec      ecx
	cmp      ecx, 0
	jne      read_row

	leave
	ret

; void print_matrix(int rows, int cols, int **matrix)
; int rows     = [ebp+16]
; int cols     = [ebp+12]
; int** matrix = [ebp+8]
print_matrix:
	enter    0,0

	; setup pointer to matrix in ebx, and counter for rows in cl
	mov      ebx, [ebp+8]
	mov      ecx, [ebp+16]

print_row:
	mov      edx, [ebp+12]
print_col:
	mov      eax, [ebx]
	call     print_int
	mov      eax, ' '
	call     print_char
	dec      edx
	add      ebx, 4
	cmp      edx, 0
	jne      print_col

	call	 print_nl
	dec      ecx
	cmp      ecx, 0
	jne      print_row

	leave
	ret

; void transpose_matrix(int rows, int cols, int **matrix, int **t_matrix);
; int rows       = [ebp+20]
; int cols       = [ebp+16]
; int **matrix   = [ebp+12]
; int **t_matrix = [ebp+8]

transpose_matrix:
	enter    0,0

	; setup pointers to matrices
	mov      ebx, [ebp+12]

	; setup counters, ecx = i, esi = j
	mov		 ecx, 0
	mov		 esi, 0

trans_outer:
	mov		 esi, 0
trans_inner:
	;compute index
	mov		 eax, [ebp+8]
	mov		 edi, [ebp+20]
	imul     edi, esi
	add		 edi, ecx
	imul	 edi, 4
	add		 eax, edi

	;store value into t_matrix
	mov		 edi, [ebx]
	mov		 [eax], edi
	
	;next inner loop
	inc		 esi
	add		 ebx, 4
	cmp		 esi, [ebp+16]
	jl		 trans_inner

	;next outer loop
	inc		 ecx
	cmp		 ecx, [ebp+20]
	jl		 trans_outer

	leave
	ret

; void max_matrix(int **matrix, 
;				  int rows,
;				  int cols,
;				  int *max, 
;				  int *max_row,
;				  int *max_col);
; int **matrix = [ebp+28]
; int rows	   = [ebp+24]
; int cols	   = [ebp+20]
; int *max     = [ebp+16]
; int *max_row = [ebp+12]
; int *max_col = [ebp+8]
; locals:
; int max      = [ebp-4]
; int i        = ecx
; int j        = esi
max_matrix:
	enter    0,4
		
	; initialize max and pointer to matrix
	mov		 dword [ebp-4], 0
	mov		 ebx, [ebp+28]

	; setup counters, ecx = i, esi = j
	mov		 ecx, 0
	mov		 esi, 0

max_outer:
	mov		 esi, 0
max_inner:
	; check if this element is the new maximum
	mov		 eax, [ebx]
	cmp		 dword eax, [ebp-4]
	jle		 next_max_inner

	;set new max
	mov		 dword [ebp-4], eax

	; update max_row value
	mov		 eax, [ebp+12]
	mov		 edx, ecx
	mov		 [eax], edx 
	
	; update max_col value
	mov		 eax, [ebp+8]
	mov		 edx, esi
	mov		 [eax], edx 


next_max_inner:
	;next inner loop
	inc		 esi
	add		 ebx, 4
	cmp		 esi, [ebp+20]
	jl		 max_inner

	;next outer loop
	inc		 ecx
	cmp		 ecx, [ebp+24]
	jl		 max_outer

	; update max value (at address passed in)
	mov		 eax, [ebp+16]
	mov		 edx, [ebp-4]
	mov		 [eax], edx

	leave
	ret

; void print_matrix_elem(int elem, int row, int col)
; int elem = [ebp+16]
; int row = [ebp+12]
; int col = [ebp+8]
print_matrix_elem:
	enter    0,0

	mov      eax, [ebp+16]
	call     print_int


	mov      eax, elem_output
	call     print_string

	mov		 eax, '['
	call	 print_char

	mov		 eax, [ebp+12]
	call	 print_int

	mov		 eax, ']'
	call	 print_char

	mov		 eax, '['
	call	 print_char

	mov		 eax, [ebp+8]
	call	 print_int

	mov		 eax, ']'
	call	 print_char

	leave
	ret

