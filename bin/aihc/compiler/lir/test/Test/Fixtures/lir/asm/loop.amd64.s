	.text
	.p2align 4
sum:
	push rbp
	mov rbp, rsp
	sub rsp, 0x40
	mov [rsp + 24], rbx
	mov [rsp + 32], r12
	mov [rsp + 40], r13
	mov [rsp + 48], r14
	mov [rsp + 56], r15
	mov [rsp], rdi
.Llir_0_0:
	mov rax, [rsp]
	mov r10d, 0x0
	mov r12, rax
	mov [rsp + 8], r10
.Llir_0_1:
	mov rax, r12
	mov r10d, 0x0
	cmp rax, r10
	sete al
	movzx rax, al
	mov r15, rax
	mov rax, r15
	test rax, rax
	je .Llir_else_0
	mov rax, [rsp + 8]
	mov [rsp + 16], rax
	jmp .Llir_0_3
.Llir_else_0:
	mov rax, r12
	mov r10, [rsp + 8]
	mov r13, rax
	mov r14, r10
	jmp .Llir_0_2
.Llir_0_2:
	mov rax, r13
	mov r10d, 0x1
	sub rax, r10
	mov r15, rax
	mov rax, r14
	mov r10, r13
	add rax, r10
	mov rbx, rax
	mov rax, r15
	mov r10, rbx
	mov r12, rax
	mov [rsp + 8], r10
	jmp .Llir_0_1
.Llir_0_3:
	mov rax, [rsp + 16]
	mov rbx, [rsp + 24]
	mov r12, [rsp + 32]
	mov r13, [rsp + 40]
	mov r14, [rsp + 48]
	mov r15, [rsp + 56]
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
