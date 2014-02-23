;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menubinary.asm,v 1.13 2013/06/02 20:41:05 thor Exp $        **
;;; **                                                                  **
;;; ** In this module:   Binary load,save				**
;;; **********************************************************************

        .include "menujmptab.i"
	.include "kernel.i"
	.include "fms.i"
	.include "dup.i"
	.include "reset.i"
	.include "errors.i"
	.segment "menufunction"

BinStartAddress	=	$f0
BinEndAddress	=	$f2
Flags		=	$f8
	
;;; ***
;;; *** Useful macro definitions
;;; ***
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

;;; *** Init vector
        jmp InitBinary
;;; *** Menu functions
	jmp BinarySave
	jmp BinaryLoad
	jmp RunAtAddress
;;; *** The init vector. Set the RUN vector to something harmless
.proc	InitBinary
	lda #<RTSVector
	sta RunVector
	sta InitVector
	lda #>RTSVector
	sta RunVector+1
	sta InitVector+1
	rts
.endproc
;;; *** BinarySave
;;; *** Construct a binary load file from user given parameters
BinarySaveExit:
	rts
.proc	BinarySave
	ldx #<BinaryReq
	ldy #>BinaryReq
	lda #45
	jsr PrintRecord
	jsr SetRedColor
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr PrintEOL
	jsr DisableBREAK
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq BinarySaveExit
	jsr LoadBuf1Ptr
	jsr AddDevice

	ldx #$10
	jsr SetIOCB

	jsr LoadBuf2Ptr
	jsr ReadInputParameter	;read the start address
	jsr LoadBuf2Ptr
	jsr FromHex		;errors on invalid input
	stx BinStartAddress
	sty BinStartAddress+1
	lda #0
	sta Flags

	jsr LoadBuf2Ptr
	jsr ReadInputParameter	;end address
	jsr LoadBuf2Ptr
	jsr FromHex
	stx BinEndAddress
	sty BinEndAddress+1

	lda BinEndAddress
	cmp BinStartAddress
	lda BinEndAddress+1
	sbc BinStartAddress+1
	bcs isfine
	;; must be larger or equal
	jmp ParameterError
isfine:	
	
	jsr LoadBuf2Ptr
	jsr ReadInputParameter
	beq noinit
	jsr LoadBuf2Ptr
	jsr FromHex
	stx InitAddress
	sty InitAddress+1

	lda Flags
	ora #$40		;indicate to have init
	sta Flags
noinit:
	jsr LoadBuf2Ptr
	jsr ReadInputParameter
	beq norun
	jsr LoadBuf2Ptr
	jsr FromHex
	stx RunAddress
	sty RunAddress+1

	lda Flags
	ora #$80		;we do have a run address
	sta Flags
norun:
	jsr LoadBuf1Ptr
	lda #'N'
	jsr GetArgumentExtension ;is there a /N?
	bcs noheader
	lda Flags
	ora #$01		;write a binary header
	sta Flags
noheader:
	jsr LoadBuf1Ptr
	lda #8
	jsr Open		;open the file now
	jsr CheckIOError

	lda Flags
	lsr a
	bcc noheaderw
	ldx #<2
	ldy #>2
	jsr SetLength
	ldx #<Header
	ldy #>Header
	jsr BPut
	jsr CheckIOError
noheaderw:
	ldx #<4
	ldy #>4
	jsr SetLength
	ldx #<BinStartAddress
	ldy #>BinStartAddress
	jsr BPut
	jsr CheckIOError

	lda BinEndAddress
	sec
	sbc BinStartAddress
	tax
	lda BinEndAddress+1
	sbc BinStartAddress+1
	tay
	inx
	bne nocarry
	iny
nocarry:
	jsr SetLength
	ldx BinStartAddress
	ldy BinStartAddress+1
	jsr BPut
	jsr CheckIOError

	ldx #<6
	ldy #>6
	jsr SetLength
	
	;; is there a init address?
	bit Flags
	bvc noinitw
	;; here an init address

	ldx #<InitBytes
	ldy #>InitBytes
	jsr BPut
	jsr CheckIOError
noinitw:
	bit Flags
	bpl norunw

	ldx #<RunBytes
	ldy #>RunBytes
	jsr BPut
	jsr CheckIOError
norunw:
	jsr Close
	jsr CheckIOError
exit:	
	rts
Header:		.word	$ffff	;the binary load header
InitBytes:	.word	InitVector,InitVector+1
InitAddress:	.word	0
RunBytes:	.word	RunVector,RunVector+1
RunAddress:	.word	0
BinaryReq:	.byte "Binary save - give start,end(,init,run)",155
.endproc
;;; *** Binary load
;;; *** This implements your average binary load without bells and wissles.
.proc	BinaryLoad
	ldx #<BloadReq
	ldy #>BloadReq
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr PrintEOL
	jsr LoadInputBufferPtr
	jsr AddDevice
	beq exit
	jsr LoadInputBufferPtr
	jsr SetZPtr
	;; copy the entire command line to the DUP command line buffer
	ldy #0
cpcmdline:
	lda (ZPtr),y
	sta DupBuffer,y
	iny
	bmi done
	cmp #$9b
	bne cpcmdline
done:
	ldx #$10
	jsr SetIOCB
	jsr LoadInputBufferPtr
	lda #4
	jsr Open
	jsr CheckIOError
	jsr Close
	;; here is the file ok
	jsr RemoveMenu

	lda InstallMenu+1
	sta Restore+1
	lda InstallMenu+2
	sta Restore+2
	
	ldx #$c0		;init and run
	ldy #0
	jsr SetAux
	jsr LoadInputBufferPtr
	lda #CmdBload
	jsr XIO
	sty Flags

	jsr Restore
	
	ldy Flags
	bpl exit
	jsr CheckIOError
exit:	
	rts
Restore:
	jmp RTSVector
BloadReq:	.byte "Which file to load ?",155
.endproc
;;; *** RunAtAddress
;;; *** Run a binary file from a specific address.
.proc	RunAtAddress
	ldx #<RunReq
	ldy #>RunReq
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr PrintEOL
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq noparam
	jsr LoadBuf1Ptr
	jsr FromHex
	stx RunVector
	sty RunVector+1
noparam:
	ldx RunVector
	ldy RunVector+1
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	cmp #$60		;points directly to an RTS?
	beq exit

	jsr RemoveMenu
	jsr RunOverRunVector
	jsr InstallMenu
exit:	
	rts
RunOverRunVector:
	jmp (RunVector)
RunReq:		.byte "Run from which address ?",155
.endproc
