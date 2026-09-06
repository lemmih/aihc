	.text
	.p2align 4
twice:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10
	mov [rsp], rbx
	mov [rsp + 8], r12
	mov rbx, rdi
	mov eax, 0x0
	call helper
	mov r12, rax
	mov rdi, r12
	mov eax, 0x0
	call helper
	add rax, r12
	add rax, rbx
	mov rbx, [rsp]
	mov r12, [rsp + 8]
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
