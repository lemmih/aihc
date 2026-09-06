	.text
	.p2align 2
_twice:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #48
	str x19, [sp, #24]
	str x20, [sp, #32]
	mov x19, x0
.Llir_0_0:
	mov x0, x19
	bl _helper
	mov x20, x0
	bl _helper
	str x0, [sp]
	mov x9, x20
	ldr x10, [sp]
	add x9, x9, x10
	str x9, [sp, #8]
	mov x10, x19
	add x9, x9, x10
	str x9, [sp, #16]
	ldr x0, [sp, #16]
	ldr x19, [sp, #24]
	ldr x20, [sp, #32]
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
