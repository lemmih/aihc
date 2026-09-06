	.text
	.p2align 2
_spread:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #96
	str x19, [sp, #80]
	str x0, [sp]
	str x1, [sp, #8]
	str x2, [sp, #16]
	str x3, [sp, #24]
	str x4, [sp, #32]
	str x5, [sp, #40]
	str x6, [sp, #48]
	str x7, [sp, #56]
	ldr x9, [x29, #16]
	str x9, [sp, #64]
	ldr x9, [x29, #24]
	mov x19, x9
.Llir_0_0:
	ldr x9, [sp]
	mov x10, x19
	add x9, x9, x10
	str x9, [sp, #72]
	sub sp, sp, #16
	ldr x9, [sp, #80]
	str x9, [sp]
	mov x9, x19
	str x9, [sp, #8]
	ldr x0, [sp, #88]
	ldr x1, [sp, #24]
	ldr x2, [sp, #32]
	ldr x3, [sp, #40]
	ldr x4, [sp, #48]
	ldr x5, [sp, #56]
	ldr x6, [sp, #64]
	ldr x7, [sp, #72]
	ldr x19, [sp, #96]
	mov x10, x29
	ldp x29, x30, [x10]
	add x11, x10, #16
	ldr x9, [sp, #8]
	str x9, [x11, #8]
	ldr x9, [sp]
	str x9, [x11]
	mov sp, x11
	b _spread
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
