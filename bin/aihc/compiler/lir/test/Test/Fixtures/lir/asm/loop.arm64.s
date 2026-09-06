	.text
	.p2align 2
_sum:
	mov x8, #0
.Llir_0_1:
	cbnz x0, .Llir_0_2
	mov x0, x8
	b .Llir_0_3
.Llir_0_2:
	sub x9, x0, #1
	add x8, x8, x0
	mov x0, x9
	b .Llir_0_1
.Llir_0_3:
	ret
