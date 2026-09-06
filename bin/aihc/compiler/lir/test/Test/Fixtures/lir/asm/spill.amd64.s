	.text
	.p2align 4
wide:
	mov rax, rdi
	add rax, rsi
	add rcx, rdx
	add r8, r9
	imul rax, rcx
	imul r8, rdi
	add rax, r8
	add rax, rsi
	add rax, rdx
	ret
	.section .note.GNU-stack
