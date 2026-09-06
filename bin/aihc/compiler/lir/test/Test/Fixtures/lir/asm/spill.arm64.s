	.text
	.p2align 2
_wide:
	add x8, x0, x1
	add x3, x2, x3
	add x4, x4, x5
	mul x8, x8, x3
	mul x4, x4, x0
	add x8, x8, x4
	add x8, x8, x1
	add x0, x8, x2
	ret
