	.text
	.p2align 4
count:
	push rbp
	mov rbp, rsp
	sub rsp, 0x40
	mov [rsp], rdi
	mov [rsp + 8], rsi
.Llir_0_0:
	mov rax, [rsp]
	mov r10d, 0x0
	cmp rax, r10
	sete al
	movzx rax, al
	mov [rsp + 16], rax
	mov rax, [rsp + 16]
	test rax, rax
	je .Llir_else_0
	mov rax, [rsp + 8]
	mov [rsp + 56], rax
	jmp .Llir_0_2
.Llir_else_0:
	mov rax, [rsp]
	mov r10, [rsp + 8]
	mov [rsp + 24], rax
	mov [rsp + 32], r10
	jmp .Llir_0_1
.Llir_0_1:
	mov rax, [rsp + 24]
	mov r10d, 0x1
	sub rax, r10
	mov [rsp + 40], rax
	mov rax, [rsp + 32]
	mov r10, [rsp + 24]
	add rax, r10
	mov [rsp + 48], rax
	mov rdi, [rsp + 40]
	mov rsi, [rsp + 48]
	mov rsp, rbp
	pop rbp
	jmp count
.Llir_0_2:
	mov rdi, [rsp + 56]
	mov rsp, rbp
	pop rbp
	jmp done
	.section .note.GNU-stack
