	.text
	.p2align 4
count:
	test rdi, rdi
	jne .Llir_0_1
	mov rdi, rsi
	jmp .Llir_0_2
.Llir_0_1:
	lea r9, [rdi - 1]
	add rsi, rdi
	mov rdi, r9
	jmp count
.Llir_0_2:
	jmp done
	.section .note.GNU-stack
