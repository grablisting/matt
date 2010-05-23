; Matt Forbes
; CSCI 347 - Assignment 5
; May 2010
; Matrix procedures, read matrix, write matrix, transpose matrix, max element in matrix

%include "asm_io.inc"

; Prototypes
; void get_matrix_size(int *rows, int *cols);
; void read_matrix(int **matrix);
; void print_matrix(int **matrix);
; void transpose_matrix(int **matrix, int **transposed_matrix);
; void max_matrix(int **matrix, int *max, int *max_row, int *max_col);
; void print_matrix_max(int *max, int *max_row, int *max_col);

segment .data
	m_rows		db		0
	m_cols		db		0

	row_prompt  db "Enter number of rows: ",0
	col_prompt  db "Enter number of columns: ",0
	mat_prompt  db "Enter Matrix data: ",0
	max_prompt1 db "Maximum element is ",0
	max_prompt2 db " at matrix",0
	

segment .bss
	matrix		resb	100
	t_matrix	resb	100
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
	sub		 esp, 8

	; Read matrix
	push	 matrix
	call	 read_matrix
	sub		 esp, 4

	; Print matrix
	push	 matrix
	call	 print_matrix
	sub	 	 esp, 4

	; Transpose matrix
	push	 matrix
	push	 t_matrix
	call	 transpose_matrix
	sub		 esp, 8	

	; Print transposed matrix
	push	 t_matrix
	call	 print_matrix
	sub		 esp, 4

	; Find max element in matrix
	push	 matrix
	call	 max_matrix
	sub		 esp, 4
	
	; Print max element
	push	 dword [max]
	push	 dword [max_r]
	push	 dword [max_c]
	call	 print_matrix_max
	sub		 esp, 12

	popa
	mov		 eax, 0
	leave
	ret
