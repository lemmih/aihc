	.text
	.p2align 4
double:
	mov rsi, rdi
	movq r11, xmm0
	mov rdi, r11
	cmp rsi, 0x2a
	sete sil
	movzx rsi, sil
	mov r11d, 0x0
	mov rax, rdi
	test rsi, rsi
	cmove rax, r11
	movq xmm0, rax
	ret
	.text
	.p2align 4
single:
	movq r11, xmm0
	mov r11d, r11d
	mov rsi, r11
	cmp rdi, 0x13
	sete dil
	movzx rdi, dil
	mov r11d, 0x0
	mov rax, rsi
	test rdi, rdi
	cmove rax, r11
	movd xmm0, eax
	ret
	.text
	.p2align 4
forward:
	push rbp
	mov rbp, rsp
	mov rsi, rdi
	movq r11, xmm0
	mov rdi, r11
	movq xmm0, rdi
	mov rdi, rsi
	mov eax, 0x1
	call double
	movq rax, xmm0
	mov r10, 0x4014000000000000
	movq xmm0, rax
	movq xmm1, r10
	addsd xmm0, xmm1
	movq rdi, xmm0
	movq xmm0, rdi
	mov edi, 0x2a
	mov eax, 0x1
	mov rsp, rbp
	pop rbp
	jmp double
	.text
	.p2align 4
indirect:
	mov rdx, rsi
	movq r11, xmm0
	mov r11d, r11d
	mov rsi, r11
	mov r10, rdi
	test r10, r10
	je .Llir_trap_0
	mov r11d, 0x40600000
	movq xmm0, r11
	mov edi, 0x13
	mov eax, 0x1
	jmp r10
	.text
	.p2align 4
main:
	push rbp
	mov rbp, rsp
	mov r11, 0x4000000000000000
	movq xmm0, r11
	mov edi, 0x2a
	mov eax, 0x1
	call forward
	movq rax, xmm0
	mov rbx, rax
	lea r10, [rip + table]
	mov rdi, [r10]
	mov r11d, 0x3f800000
	movq xmm0, r11
	mov esi, 0x8
	mov eax, 0x1
	call indirect
	movd eax, xmm0
	mov eax, eax
	mov rdx, rax
	mov rax, rbx
	mov rsp, rbp
	pop rbp
	ret
	.text
	.p2align 4
.Llir_trap_0:
	lea rsi, [rip + .Llir_trap_message_0]
	mov edx, 0x20
	jmp .Llir_trap
	.p2align 4
.Llir_trap:
	mov edi, 0x2
	and rsp, -0x10
	call write
	mov edi, 0x1
	call _exit
	ud2
	.section .rodata
.Llir_trap_message_0:
	.byte 0x69, 0x6e, 0x64, 0x69, 0x72, 0x65, 0x63, 0x74, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x20, 0x74, 0x6f, 0x20, 0x61, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0xa
	.section .rodata
	.p2align 3
table:
	.quad single
	.section .note.GNU-stack
