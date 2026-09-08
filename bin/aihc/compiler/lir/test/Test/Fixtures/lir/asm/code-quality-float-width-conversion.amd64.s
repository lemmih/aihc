	.text
	.p2align 4
main:
	movd xmm0, edi
	cvtss2sd xmm0, xmm0
	movq rax, xmm0
	movq xmm0, rsi
	cvtsd2ss xmm0, xmm0
	movd edx, xmm0
	ret
	.section .note.GNU-stack
