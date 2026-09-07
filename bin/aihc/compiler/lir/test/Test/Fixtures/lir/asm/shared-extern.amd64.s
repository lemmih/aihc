	.text
	.p2align 4
many:
	push rbp
	mov rbp, rsp
	mov edi, 0x0
	call shared
	mov edi, 0x1
	call shared
	mov edi, 0x2
	call shared
	mov edi, 0x3
	call shared
	mov edi, 0x4
	call shared
	mov edi, 0x5
	call shared
	mov edi, 0x6
	call shared
	mov edi, 0x7
	call shared
	mov edi, 0x8
	call shared
	mov edi, 0x9
	mov rsp, rbp
	pop rbp
	jmp shared
	.text
	.p2align 4
one:
	mov edi, 0x0
	jmp single
	.text
	.p2align 4
boundary:
	push rbp
	mov rbp, rsp
	mov edi, 0x0
	call eight
	mov edi, 0x1
	call eight
	mov edi, 0x2
	call eight
	mov edi, 0x3
	call eight
	mov edi, 0x4
	call eight
	mov edi, 0x5
	call eight
	mov edi, 0x6
	call eight
	mov edi, 0x7
	mov rsp, rbp
	pop rbp
	jmp eight
	.section .note.GNU-stack
