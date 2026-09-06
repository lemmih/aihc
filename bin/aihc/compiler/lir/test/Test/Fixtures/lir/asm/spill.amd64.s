	.text
	.p2align 4
wide:
	push rbp
	mov rbp, rsp
	sub rsp, 0x70
	mov [rsp + 88], rbx
	mov [rsp + 96], r12
	mov [rsp + 104], r13
	mov rbx, rdi
	mov r12, rsi
	mov r13, rdx
	mov [rsp], rcx
	mov [rsp + 8], r8
	mov [rsp + 16], r9
.Llir_0_0:
	mov rax, rbx
	mov r10, r12
	add rax, r10
	mov [rsp + 24], rax
	mov rax, r13
	mov r10, [rsp]
	add rax, r10
	mov [rsp + 32], rax
	mov rax, [rsp + 8]
	mov r10, [rsp + 16]
	add rax, r10
	mov [rsp + 40], rax
	mov rax, [rsp + 24]
	mov r10, [rsp + 32]
	imul rax, r10
	mov [rsp + 48], rax
	mov rax, [rsp + 40]
	mov r10, rbx
	imul rax, r10
	mov [rsp + 56], rax
	mov rax, [rsp + 48]
	mov r10, [rsp + 56]
	add rax, r10
	mov [rsp + 64], rax
	mov rax, [rsp + 64]
	mov r10, r12
	add rax, r10
	mov [rsp + 72], rax
	mov rax, [rsp + 72]
	mov r10, r13
	add rax, r10
	mov [rsp + 80], rax
	mov rax, [rsp + 80]
	mov rbx, [rsp + 88]
	mov r12, [rsp + 96]
	mov r13, [rsp + 104]
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
