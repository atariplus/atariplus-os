;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menudir.asm,v 1.6 2013/06/02 20:41:05 thor Exp $		**
;;; **									**
;;; ** In this module:	 Directory menu for DUP 2.++			**
;;; **********************************************************************

	.include "menujmptab.i"
	.segment "menufunction"

;;; Init-vector
.proc	InitVector
	rts			;not required
.endproc
;;; Entry points
	jmp Dir			;show directory
	jmp Car			;run cartridge
	jmp Dos			;to DOS
;;; Functions
;;; **** Show the contents of the directory
;;; **** Dir
.proc	Dir
	lda #40
	ldx #<DirRequest
	ldy #>DirRequest
	jsr PrintRecord

	jsr GetKey
	cmp #'1'
	bcc normal
	cmp #'9'
	bcs normal
	;; here short directlry
	pha
	jsr LoadInputBufferPtr
	jsr SetZPtr
	ldy #0
	lda #'D'
	sta (ZPtr),y
	iny
	pla
	sta (ZPtr),y		;install the default device name
	iny
	ldx #4
cppat:
	lda DefaultPattern,x
	sta (ZPtr),y
	iny
	dex
	bpl cppat
	jsr PrintEOL
	bpl cont
normal:
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
cont:
	jsr CursorOff
	jsr DisableBREAK
	jsr BlockDisplay
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	bne notempty
	;; no pattern given
	jsr LoadBuf1Ptr
	jsr SetZPtr
	ldy #0
	ldx #3
settdf:
	lda DefaultPattern,x
	sta (ZPtr),y
	iny
	dex
	bpl settdf
notempty:
	jsr LoadBuf1Ptr
	jsr AddDevice
	jsr LoadBuf1Ptr
	jsr SetZPtr
	ldy #1
	lda (ZPtr),y
	cmp #':'		;device here?
	beq fnddevice
	iny			;or here?
fnddevice:
	iny
	lda (ZPtr),y		;is there a valid pattern?
	cmp #$9b
	bne havename
	ldx #3
settdf2:
	lda DefaultPattern,x
	sta (ZPtr),y
	iny
	dex
	bpl settdf2
havename:
	lda #0
	sta ZFlag
	jsr LoadBuf2Ptr
	jsr ReadInputParameter
	beq nooutput		;output spec given?
	dec ZFlag
nooutput:
	ldx #$10
	jsr SetIOCB
	jsr LoadBuf1Ptr
	lda #6
	jsr Open		;open the directory
	jsr CheckIOError
	;; read in units of 256 bytes
	lda #0
	sta ZPtr
	sta ZPtr+1		;read size=zero bytes
	ldx #<256
	ldy #>256
	jsr SetLength
	ldx #<256
	ldy #>256
	jsr Reserve
	stx Z2Ptr
	sty Z2Ptr+1
loop:
	jsr BGet
	jsr CheckIOError
	php			;keep the EOF flag
	jsr GetLength
	clc
	txa
	adc ZPtr
	sta ZPtr
	tya
	adc ZPtr+1
	sta ZPtr+1
	plp
	bcs eof
	;; More data here, extend the memory
	ldx #<256
	ldy #>256
	jsr Reserve
	clc
	bcc loop
eof:
	jsr Close
	;; now print
	bit ZFlag
	bpl toeditor
	jsr LoadBuf2Ptr
	lda #8
	jsr Open		;on the same IOCB
	jsr CheckIOError
	bcc print
toeditor:
	ldx #0			;to IOCB #0 instead of one
	jsr SetIOCB
print:
	lda #0
	sta Z3Ptr
drloop:
	ldy #0
fndeol:	
	lda (Z2Ptr),y
	iny
	cmp #$9b
	bne fndeol
	sty Z3Ptr+1
	bit Z3Ptr
	bmi odd
	lda IOCB
	bne odd
	lda #' '
	dey
	sta (Z2Ptr),y
	iny
	lda (Z2Ptr),y
	pha
	lda #' '
	sta (Z2Ptr),y
	iny
odd:
	tya
	tax
	ldy #0
	jsr SetLength
	ldx Z2Ptr
	ldy Z2Ptr+1
	jsr BPut
	jsr CheckIOError
	;; restore the lost character
	bit Z3Ptr
	bmi odd2
	lda IOCB
	bne odd2
	ldy Z3Ptr+1
	pla
	sta (Z2Ptr),y
odd2:
	;; print with EOL on the next row
	lda Z3Ptr
	eor #$80
	sta Z3Ptr

	clc
	lda Z2Ptr
	adc Z3Ptr+1
	sta Z2Ptr
	bcc noinc1
	inc Z2Ptr+1
noinc1:
	sec
	lda ZPtr
	sbc Z3Ptr+1
	sta ZPtr
	bcs nodec1
	dec ZPtr+1
nodec1:
	lda ZPtr
	ora ZPtr+1
	bne drloop

	bit Z3Ptr
	bpl haveol
	jsr PrintEOL
haveol:	
	;; finally, close down.
	ldx #$10
	jsr SetIOCB
	jsr Close
	rts
DirRequest:
	.byte "directory - search spec., list file.",155
DefaultPattern:
	.byte 155,"*.*:"
.endproc
;;; *** Run the cartridge
;;; *** CAR
.proc	Car
	ldx #<CarRequest
	ldy #>CarRequest
	lda #CarRequestL
	jsr Print
	jsr YesNo
	bcs gocart
	jsr PrintEOL
	rts
gocart:
	jmp RunCartridge
CarRequest:	.byte "O.K. to run cartridge ?"
CarRequestL	=	*-CarRequest
.endproc
;;; *** Return to the DOS command line
;;; *** Dos
.proc	Dos
	ldx #<DosRequest
	ldy #>DosRequest
	lda #DosRequestL
	jsr Print
	jsr YesNo
	bcs godos
	jsr PrintEOL
	rts
godos:
	jmp RunCommandLine
DosRequest:	.byte "O.K. to run DOS ?"
DosRequestL	=	*-DosRequest
.endproc
	
