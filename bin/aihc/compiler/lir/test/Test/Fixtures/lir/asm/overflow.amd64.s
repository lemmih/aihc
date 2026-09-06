	.text
	.p2align 4
spread:
	mov rax, [rsp + 8]
	mov rbx, [rsp + 16]
	mov r12, [rsp + 24]
	mov r13, [rsp + 32]
	add rdi, r13
	jmp spread
	.section .note.GNU-stack
