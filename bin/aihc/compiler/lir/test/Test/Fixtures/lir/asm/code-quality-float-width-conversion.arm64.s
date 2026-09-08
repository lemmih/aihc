	.text
	.p2align 2
_main:
	fmov s16, w0
	fcvt d16, s16
	fmov x0, d16
	fmov d16, x1
	fcvt s16, d16
	fmov w1, s16
	ret
