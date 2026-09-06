	.text
	.p2align 2
_sum:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #64
	str x19, [sp, #16]
	str x20, [sp, #24]
	str x21, [sp, #32]
	str x22, [sp, #40]
	str x23, [sp, #48]
	str x24, [sp, #56]
	str x0, [sp]
.Llir_0_0:
	ldr x9, [sp]
	mov x10, #0
	mov x20, x9
	mov x19, x10
.Llir_0_1:
	mov x9, x20
	mov x10, #0
	cmp x9, x10
	cset x9, eq
	mov x23, x9
	cbz x9, .Llir_else_0
	mov x9, x19
	str x9, [sp, #8]
	b .Llir_0_3
.Llir_else_0:
	mov x9, x20
	mov x10, x19
	mov x21, x9
	mov x22, x10
	b .Llir_0_2
.Llir_0_2:
	mov x9, x21
	mov x10, #1
	sub x9, x9, x10
	mov x23, x9
	mov x9, x22
	mov x10, x21
	add x9, x9, x10
	mov x24, x9
	mov x9, x23
	mov x10, x24
	mov x20, x9
	mov x19, x10
	b .Llir_0_1
.Llir_0_3:
	ldr x0, [sp, #8]
	ldr x19, [sp, #16]
	ldr x20, [sp, #24]
	ldr x21, [sp, #32]
	ldr x22, [sp, #40]
	ldr x23, [sp, #48]
	ldr x24, [sp, #56]
	mov sp, x29
	ldp x29, x30, [sp], #16
	ret
	.text
	.p2align 2
.Llir_trap:
	mov x2, x1
	mov x1, x0
	mov x0, #2
	bl _write
	mov x0, #1
	bl __exit
	brk #0
