	.text
	.p2align 2
_main:
	adrp x17, _bytes@PAGE
	add x17, x17, _bytes@PAGEOFF
	ldrb w0, [x17]
	and x0, x0, #0x1
	adrp x17, _bytes@PAGE
	add x17, x17, _bytes@PAGEOFF
	ldrb w1, [x17, #1]
	and x1, x1, #0x1
	ret
	.section __TEXT,__const
	.p2align 0
_bytes:
	.byte 0x2
	.byte 0x3
