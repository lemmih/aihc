	.text
	.p2align 4
main:
	lea r10, [rip + bytes]
	movzx rax, [r10]
	and rax, 0x1
	lea r10, [rip + bytes]
	movzx rdx, [r10 + 1]
	and rdx, 0x1
	ret
	.section .rodata
	.p2align 0
bytes:
	.byte 0x2
	.byte 0x3
	.section .note.GNU-stack
