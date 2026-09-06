	.text
	.p2align 2
_spread:
	ldr x8, [sp]
	ldr x9, [sp, #8]
	add x0, x0, x9
	b _spread
