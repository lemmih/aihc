	.text
	.p2align 4
sum:
	mov eax, 0x0
.Llir_0_1:
	test rdi, rdi
	je .Llir_0_3
.Llir_0_2:
	lea r9, [rdi - 1]
	add rax, rdi
	mov rdi, r9
	jmp .Llir_0_1
.Llir_0_3:
	ret
	.section .note.GNU-stack
