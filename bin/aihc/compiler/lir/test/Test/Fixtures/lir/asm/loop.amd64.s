	.text
	.p2align 4
sum:
	push rbp
	mov rbp, rsp
	sub rsp, 0x30
	mov [rsp + 16], rbx
	mov [rsp + 24], r12
	mov [rsp + 32], r13
	mov [rsp + 40], r14
	mov [rsp], rdi
.Llir_0_0:
	mov rax, [rsp]
	mov r10d, 0x0
	mov r12, rax
	mov rbx, r10
.Llir_0_1:
	mov rax, r12
	mov r10d, 0x0
	cmp rax, r10
	sete al
	movzx rax, al
	mov r13, rax
	mov rax, r13
	test rax, rax
	je .Llir_else_0
	mov rax, rbx
	mov [rsp + 8], rax
	jmp .Llir_0_3
.Llir_else_0:
	mov rax, r12
	mov r10, rbx
	mov rbx, rax
	mov r12, r10
	jmp .Llir_0_2
.Llir_0_2:
	mov rax, rbx
	mov r10d, 0x1
	sub rax, r10
	mov r13, rax
	mov rax, r12
	mov r10, rbx
	add rax, r10
	mov r14, rax
	mov rax, r13
	mov r10, r14
	mov r12, rax
	mov rbx, r10
	jmp .Llir_0_1
.Llir_0_3:
	mov rax, [rsp + 8]
	mov rbx, [rsp + 16]
	mov r12, [rsp + 24]
	mov r13, [rsp + 32]
	mov r14, [rsp + 40]
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
