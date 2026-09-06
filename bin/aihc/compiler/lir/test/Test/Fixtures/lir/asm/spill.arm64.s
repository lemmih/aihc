	.text
	.p2align 2
_wide:
	stp x29, x30, [sp, #-16]!
	mov x29, sp
	sub sp, sp, #112
	str x19, [sp, #88]
	str x20, [sp, #96]
	str x21, [sp, #104]
	mov x19, x0
	mov x20, x1
	mov x21, x2
	str x3, [sp]
	str x4, [sp, #8]
	str x5, [sp, #16]
.Llir_0_0:
	mov x9, x19
	mov x10, x20
	add x9, x9, x10
	str x9, [sp, #24]
	mov x9, x21
	ldr x10, [sp]
	add x9, x9, x10
	str x9, [sp, #32]
	ldr x9, [sp, #8]
	ldr x10, [sp, #16]
	add x9, x9, x10
	str x9, [sp, #40]
	ldr x9, [sp, #24]
	ldr x10, [sp, #32]
	mul x9, x9, x10
	str x9, [sp, #48]
	ldr x9, [sp, #40]
	mov x10, x19
	mul x9, x9, x10
	str x9, [sp, #56]
	ldr x9, [sp, #48]
	ldr x10, [sp, #56]
	add x9, x9, x10
	str x9, [sp, #64]
	mov x10, x20
	add x9, x9, x10
	str x9, [sp, #72]
	mov x10, x21
	add x9, x9, x10
	str x9, [sp, #80]
	ldr x0, [sp, #80]
	ldr x19, [sp, #88]
	ldr x20, [sp, #96]
	ldr x21, [sp, #104]
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
