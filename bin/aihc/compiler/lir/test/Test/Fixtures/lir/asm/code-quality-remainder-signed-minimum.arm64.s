	.text
	.p2align 2
_main:
	mov x16, #128
	mov x17, #255
	sxtb x16, w16
	sxtb x17, w17
	cbz x17, .Llir_trap_0
	sdiv x15, x16, x17
	msub x0, x15, x17, x16
	and x0, x0, #0xff
	ldr x16, =-0x8000000000000000
	mov x17, #-1
	cbz x17, .Llir_trap_0
	sdiv x15, x16, x17
	msub x1, x15, x17, x16
	ret
	.text
	.p2align 2
.Llir_trap_0:
	adrp x0, .Llir_trap_message_0@PAGE
	add x0, x0, .Llir_trap_message_0@PAGEOFF
	mov x1, #25
	b .Llir_trap
	.p2align 2
.Llir_trap:
	mov x2, x1
	mov x1, x0
	mov x0, #2
	bl _write
	mov x0, #1
	bl __exit
	brk #0
	.section __TEXT,__const
.Llir_trap_message_0:
	.byte 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x20, 0x64, 0x69, 0x76, 0x69, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x79, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0xa
