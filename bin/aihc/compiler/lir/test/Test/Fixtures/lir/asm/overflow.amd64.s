	.text
	.p2align 4
spread:
	push rbp
	mov rbp, rsp
	sub rsp, 0x60
	mov [rsp + 80], rbx
	mov [rsp], rdi
	mov [rsp + 8], rsi
	mov [rsp + 16], rdx
	mov [rsp + 24], rcx
	mov [rsp + 32], r8
	mov [rsp + 40], r9
	mov rax, [rbp + 16]
	mov [rsp + 48], rax
	mov rax, [rbp + 24]
	mov [rsp + 56], rax
	mov rax, [rbp + 32]
	mov [rsp + 64], rax
	mov rax, [rbp + 40]
	mov rbx, rax
.Llir_0_0:
	mov rax, [rsp]
	mov r10, rbx
	add rax, r10
	mov [rsp + 72], rax
	sub rsp, 0x28
	mov rax, [rsp + 88]
	mov [rsp + 8], rax
	mov rax, [rsp + 96]
	mov [rsp + 16], rax
	mov rax, [rsp + 104]
	mov [rsp + 24], rax
	mov rax, rbx
	mov [rsp + 32], rax
	mov rdi, [rsp + 112]
	mov rsi, [rsp + 48]
	mov rdx, [rsp + 56]
	mov rcx, [rsp + 64]
	mov r8, [rsp + 72]
	mov r9, [rsp + 80]
	mov rbx, [rsp + 120]
	mov r10, rbp
	mov rax, [r10 + 8]
	mov [rsp], rax
	mov rbp, [r10]
	mov rax, [rsp + 32]
	mov [r10 + 40], rax
	mov rax, [rsp + 24]
	mov [r10 + 32], rax
	mov rax, [rsp + 16]
	mov [r10 + 24], rax
	mov rax, [rsp + 8]
	mov [r10 + 16], rax
	mov rax, [rsp]
	mov [r10 + 8], rax
	lea rsp, [r10 + 8]
	jmp spread
	.section .note.GNU-stack
