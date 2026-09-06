	.text
	.p2align 2
_wide:
	add x8, x0, x1
	add x9, x2, x3
	add x10, x4, x5
	mul x8, x8, x9
	mul x9, x10, x0
	add x8, x8, x9
	add x8, x8, x1
	add x0, x8, x2
	ret
