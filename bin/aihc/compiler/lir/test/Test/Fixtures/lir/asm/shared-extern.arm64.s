	.text
	.p2align 2
_many:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x0, #0
	bl .Llir_extern_0
	mov x0, #1
	bl .Llir_extern_0
	mov x0, #2
	bl .Llir_extern_0
	mov x0, #3
	bl .Llir_extern_0
	mov x0, #4
	bl .Llir_extern_0
	mov x0, #5
	bl .Llir_extern_0
	mov x0, #6
	bl .Llir_extern_0
	mov x0, #7
	bl .Llir_extern_0
	mov x0, #8
	bl .Llir_extern_0
	mov x0, #9
	mov sp, x29
	ldp x29, x30, [sp], #16
	b .Llir_extern_0
	.text
	.p2align 2
_one:
	mov x0, #0
	b _single
	.text
	.p2align 2
_boundary:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	mov x0, #0
	bl _eight
	mov x0, #1
	bl _eight
	mov x0, #2
	bl _eight
	mov x0, #3
	bl _eight
	mov x0, #4
	bl _eight
	mov x0, #5
	bl _eight
	mov x0, #6
	bl _eight
	mov x0, #7
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _eight
	.text
.Llir_extern_0:
	b _shared
