	.text
	.p2align 2
_store_byte:
	and x1, x1, #0xff
	strb w1, [x0]
	ldrb w0, [x0]
	ret
	.text
	.p2align 2
_main:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #16
	str xzr, [sp]
	add x0, sp, #0
	mov x1, #37
	mov x2, #0
	bl _store_byte
	and x0, x0, #0xff
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
