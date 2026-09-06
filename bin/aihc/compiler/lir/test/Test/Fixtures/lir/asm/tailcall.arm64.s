	.text
	.p2align 2
_count:
	cbnz x0, .Llir_0_1
	mov x0, x1
	b .Llir_0_2
.Llir_0_1:
	sub x8, x0, #1
	add x1, x1, x0
	mov x0, x8
	b _count
.Llir_0_2:
	b _done
