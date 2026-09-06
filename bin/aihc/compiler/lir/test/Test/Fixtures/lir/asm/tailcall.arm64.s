	.text
	.p2align 2
_count:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #64
	str x0, [sp]
	str x1, [sp, #8]
.Llir_0_0:
	ldr x9, [sp]
	mov x10, #0
	cmp x9, x10
	cset x9, eq
	str x9, [sp, #16]
	cbz x9, .Llir_else_0
	ldr x9, [sp, #8]
	str x9, [sp, #56]
	b .Llir_0_2
.Llir_else_0:
	ldr x9, [sp]
	ldr x10, [sp, #8]
	str x9, [sp, #24]
	str x10, [sp, #32]
	b .Llir_0_1
.Llir_0_1:
	ldr x9, [sp, #24]
	mov x10, #1
	sub x9, x9, x10
	str x9, [sp, #40]
	ldr x9, [sp, #32]
	ldr x10, [sp, #24]
	add x9, x9, x10
	str x9, [sp, #48]
	ldr x0, [sp, #40]
	ldr x1, [sp, #48]
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _count
.Llir_0_2:
	ldr x0, [sp, #56]
	mov sp, x29
	ldp x29, x30, [sp], #16
	b _done
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
