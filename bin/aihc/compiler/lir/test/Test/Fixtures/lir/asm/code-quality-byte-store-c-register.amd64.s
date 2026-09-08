	.text
	.p2align 4
store_byte:
	movzx rsi, sil
	mov [rdi], sil
	movzx rax, [rdi]
	ret
	.text
	.p2align 4
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10
	mov [rsp], 0x0
	lea rdi, [rsp]
	mov esi, 0x25
	mov edx, 0x0
	mov eax, 0x0
	call store_byte
	movzx rax, al
	mov rsp, rbp
	pop rbp
	ret
	.section .note.GNU-stack
