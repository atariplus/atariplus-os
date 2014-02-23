;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menuformat.asm,v 1.3 2013-05-18 19:48:00 thor Exp $		**
;;; **									**
;;; ** In this module:	 Format menu for DUP 2.++			**
;;; **********************************************************************

	.include "menujmptab.i"
	.segment "menufunction"
	.include "fms.i"

;;; *** Init vector
	rts
;;; *** Menu functions
	jmp FormatSingle
	jmp Format
	jmp Clear
;;; *** Format in enhanced density
;;; *** Format
.proc	Format
	lda #FormattingL
	pha
	lda #<Formatting
	pha
	lda #>Formatting
	pha
	lda #FormatActionL
	pha
	lda #<FormatAction
	pha
	lda #>FormatAction
	pha
	lda #<FormatRequest
	pha
	lda #>FormatRequest
	pha
	lda #CmdFormatExtended
	ldx #34
	ldy #0
	jmp HandleFormatActivity
FormatRequest:		.byte "Format - enter (device:)headline",155
FormatAction:		.byte "format "
FormatActionL		=	*-FormatAction
Formatting:		.byte "Formatting "
FormattingL		=	*-Formatting
.endproc
;;; *** Format in single density
;;; *** FormatSingle
.proc	FormatSingle
	lda #FormattingSingleL
	pha
	lda #<FormattingSingle
	pha
	lda #>FormattingSingle
	pha
	lda #FormatSingleActionL
	pha
	lda #<FormatSingleAction
	pha
	lda #>FormatSingleAction
	pha
	lda #<FormatSingleRequest
	pha
	lda #>FormatSingleRequest
	pha
	lda #CmdFormatExtended
	ldx #33
	ldy #0
	jmp HandleFormatActivity
FormatSingleRequest:	.byte "Format single - enter (device:)headline",155
FormatSingleAction:	.byte "format (in SD) "
FormatSingleActionL	=	*-FormatSingleAction
FormattingSingle:	.byte "Formatting "
FormattingSingleL	=	*-FormattingSingle
.endproc	
;;; *** Clear Disk: Erase, without formatting (quick format)
;;; *** Clear
.proc	Clear
	lda #ClearingL
	pha
	lda #<Clearing
	pha
	lda #>Clearing
	pha
	lda #ClearActionL
	pha
	lda #<ClearAction
	pha
	lda #>ClearAction
	pha
	lda #<ClearRequest
	pha
	lda #>ClearRequest
	pha
	lda #CmdInit
	ldx #0
	ldy #0
	jmp HandleFormatActivity
ClearRequest:		.byte "Clear disk - enter (device:)headline",155
ClearAction:		.byte "clear "
ClearActionL		=	*-ClearAction
Clearing:		.byte "Clearing "
ClearingL		=	*-Clearing
.endproc
;;; *** Run the XIO in A with Aux1,Aux2 in x,y, and
;;; *** the first request and second request text on the stack
;;; *** HandleFormatActivity
.proc	HandleFormatActivity
	sta ZFlag
	txa
	pha
	tya
	pha
	ldx #$10		;use IOCB #10
	jsr SetIOCB
	pla
	tay
	pla
	tax
	jsr SetAux
	;; get the request string
	pla
	tay
	pla
	tax
	lda #40
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr LoadInputBufferPtr
	jsr AddDevice
	jsr SetRedColor
	jsr PrintEOL
	ldx #<Question
	ldy #>Question
	lda #QuestionL
	jsr Print
	pla
	tay
	pla
	tax
	pla
	jsr Print
	jsr LoadInputBufferPtr
	jsr SetZPtr
	ldy #1
	lda #':'
	cmp (ZPtr),y
	beq isend
	iny
isend:	iny
	tya
	sta Z2Ptr
	ldx ZPtr
	ldy ZPtr+1
	jsr Print
	jsr CursorOn
	jsr YesNo
	bcc abort
	jsr CursorOff
	jsr DisableBREAK
	jsr PrintEOL
	pla
	tay
	pla
	tax
	pla
	jsr Print
	jsr LoadInputBufferPtr
	lda Z2Ptr
	jsr Print
	ldx #<Dots
	ldy #>Dots
	lda #DotsL
	jsr Print
	jsr LoadInputBufferPtr
	lda ZFlag
	jsr XIO
	jsr CheckIOError
	rts
abort:
	pla
	pla
	pla
	rts
Question:	.byte "Press ",34,"Y",34," to "
QuestionL	=	*-Question
Dots:		.byte "... please wait."
DotsL		=	*-Dots
.endproc
