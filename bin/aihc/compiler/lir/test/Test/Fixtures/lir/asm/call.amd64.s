	.text
	.p2align 4
twice:
	push rbp
	mov rbp, rsp
	sub rsp, 0x30
	mov [rsp + 24], rbx
	mov [rsp + 32], r12
	mov rbx, rdi
.Llir_0_0:
	mov rdi, rbx
	mov eax, 0x0
	call helper
	mov r12, rax
	mov rdi, r12
	mov eax, 0x0
	call helper
	mov [rsp], rax
	mov rax, r12
	mov r10, [rsp]
	add rax, r10
	mov [rsp + 8], rax
	mov rax, [rsp + 8]
	mov r10, rbx
	add rax, r10
	mov [rsp + 16], rax
	mov rax, [rsp + 16]
	mov rbx, [rsp + 24]
	mov r12, [rsp + 32]
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
