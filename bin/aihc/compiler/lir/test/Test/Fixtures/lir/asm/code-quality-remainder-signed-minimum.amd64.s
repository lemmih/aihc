	.text
	.p2align 4
main:
	mov r11d, 0x80
	mov r10d, 0xff
	movsx rax, r11b
	movsx r10, r10b
	test r10, r10
	je .Llir_trap_0
	cmp r10, -0x1
	jne .Llir_div_0
	mov r10d, 0x1
.Llir_div_0:
	cqo
	idiv r10
	mov r9, rdx
	movzx r9, r9b
	mov r11, 0x8000000000000000
	mov r10, 0xffffffffffffffff
	mov rax, r11
	test r10, r10
	je .Llir_trap_0
	cmp r10, -0x1
	jne .Llir_div_1
	mov r10d, 0x1
.Llir_div_1:
	cqo
	idiv r10
	mov r8, rdx
	mov rax, r9
	ret
	.text
	.p2align 4
.Llir_trap_0:
	lea rsi, [rip + .Llir_trap_message_0]
	mov edx, 0x19
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
	.byte 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x20, 0x64, 0x69, 0x76, 0x69, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x79, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0xa
	.section .note.GNU-stack
