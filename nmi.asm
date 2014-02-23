;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: nmi.asm,v 1.16 2013/06/02 20:41:06 thor Exp $		**
;;; **									**
;;; ** In this module:	 Support for NMI routines of all kinds		**
;;; **********************************************************************

	.include "nmi.i"
	.include "antic.i"
	.include "gtia.i"
	.include "pokey.i"
	.include "pia.i"
	.include "kernel.i"

	.segment  "OsHi"
	
;;; ** Entry point for all kinds of NMIs. The CPU starts NMI processing
;;; ** right at this place.
	.global NMIEntry
.proc	NMIEntry
	bit NMIStat
	bpl vbi
	jmp (VecDLI)		; jump thru the display list interrupt. We'd better be quick here
vbi:
	cld
	bvc other		; support for A800 like reset signal to be on the safe side
	pha
	txa
	pha
	tya
	pha
	jmp (VecImmediate)	; jump to immediate VBI here
other:
	pha
	lda NMIStat
	and #$20		; occasionally, NMIstat can be zero here if the user erases it just in time
	bne reset
	pla			; just ignore. This will break the acid test, but leave the machine sane.
	rti
reset:	
	jmp (VecNMI)
.endproc

;;; ** Short VBI exit routine, only copy the
;;; ** bare minimum
.proc	ShortVBI
	lda GPriorSet
	beq ExitVBI

	lda ColorBackShadow	; restore registers overwritten by OS DLIs	
	eor ColorMask
	and DarkMask
	sta ColorBack
	
	lda Color1Shadow
	eor ColorMask
	and DarkMask
	sta Color1
	
	lda GPriorShadow
	sta GPrior
	;; runs into the following
.endproc
;;; ** Vertical blank exit routine
	.global ExitVBI
.proc	ExitVBI
	pla
	tay
	pla
	tax
	pla
.endproc
	.global ExitDLI
.proc	ExitDLI
	rti
.endproc
	
;;; ** System vertical blank starts here.
;;; First, for the clock registers.
	.global SystemVBI
.proc	SystemVBI
	inc Clock		; increment clock registers, one after another
	bne nocarry
	inc Attract		; update attract color cycling
	inc Clock-1
	bne nocarry
	inc Clock-2
nocarry:
;;; now handle system timer #0
	ldx #0
	jsr DecrementTimer
	bcc timer0fine
	jsr JmpTimer0		; indirect jump thru timer 0 indirection
timer0fine:
	
	lda CriticIO		; handle interrupts fast?
	bne ShortVBI		; leave the VBI if so
	tsx			; check whether we are within an interrupt by testing the I bit
	lda $104,x		; on the stack
	and #$04
	bne ShortVBI
;; here: long VBI;
	;; attract mode handling
	lda #$fe
	ldx #$00
	ldy Attract
	bpl noattractmode
	sta Attract
	ldx Clock-1
	lda #$f6
noattractmode:
	sta DarkMask
	stx ColorMask
	
	ldy PenV		; copy light pen status over
	lda PenH
	sta PenHShadow
	sty PenVShadow
	
	lda DListShadow		; reload antic program counter
	ldy DListShadow+1
	sta DList
	sty DList+1

	lda DMACtrlShadow	; reload DMA control register
	sta DMACtrl
	
	lda GPriorShadow	; GTIA Priority register
	sta GPrior

;;; Handle fine scrolling

	ldy FineScroll		; enabled?
	beq nofine
	dey
	lda #$08
	sty FineScroll
	sec
	sbc FineScroll
	and #$07
	sta VScroll
nofine:
	ldx #$08
	stx Console		; reset GTIA console registers
	cli			; the remaining part is no longer critical
colorloop:
	lda PColor0Shadow,x	; now load all color registers
	eor ColorMask
	and DarkMask
	sta PColor0,x		; carry over
	dex
	bpl colorloop
	
	lda ChBaseShadow
	ldx ChCtrlShadow
	sta ChBase
	stx ChCtrl
;;; and now for timer #1
	ldx #$02
	jsr DecrementTimer
	bcc timer1fine
	jsr JmpTimer1
timer1fine:
;;; now for all remaining timers. They just set a flag
	ldx #$04
timerloop:
	lda #$00
	jsr DecrementTimer
	bcs underrun		; timer became zero, clear flag
	beq next		; ignore if timer was zero before
	lda #$ff		; set flag to 0xff if not yet zero
underrun:
	sta Timer3Flag-4,x	; insert data
next:	
	inx
	inx
	cpx #$0a
	bne timerloop
;;; keyboard handling follows	
	lda KeyStat
	and #$04		; keyboard key still pressed?
	beq nodec
	lda KeyDelay
	beq nodec
	dec KeyDelay
nodec:	
	lda KeyTimer
	beq keyboardone		; no timing active => out
	lda KeyStat
	and #$04		; keyboard key still pressed?
	beq ispressed
norepeat:
	lda #$00
	sta KeyTimer		; reset the keyboard timer
	beq keyboardone
ispressed:
	dec KeyTimer
	bne ignorekey		; not yet a key repeat
	lda KeyDisable		; if set, ignore as well
	bne ignorekey
	lda KeyRepeat		; get keyboard repeat value
	sta KeyTimer		; now start repeating
	
	lda KeyCode		; get pokey keyboard code (again)
	tay
	cmp #$9f		; is it ^1?
	beq norepeat		; if so, do not repeat it
	and #$3f
	cmp #$11		; is it HELP?
	beq norepeat
	sty KeyCodeShadow	; keep the code for repeating
keyboardone:
ignorekey:
;;; joystick/controller handling follows
	lda PIAPortA
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	ldx #$02
	ldy #$01
	jsr PaddleWrite
	pla
	ldx #$00
	dey
	jsr PaddleWrite

	lda Trigger0
	sta Strig0
	sta Strig2
	lda Trigger1
	sta Strig1
	sta Strig3

	ldx #$03
paddleloop:
	lda Pot0,x
	sta Paddle0,x
	sta Paddle4,x
	dex
	bpl paddleloop
	sta PotGo
	jmp (VecDeferred)
;;; Jump indirectly thru timer 0 vector
JmpTimer0:	
	jmp (VecVBITimer0)	
;;; Jump indirectly thru timer 1 vector
JmpTimer1:
	jmp (VecVBITimer1)
;;; Helper functions:	Decrement the timer register
;;; indirect X, does not touch A
;;; Returns with C=1 and Z set in case the timer became zero
;;; Returns with C=0 and Z set if the timer was zero before
;;; Returns with C=0 and Z clear if the timer is (not yet) zero
DecrementTimer:
	ldy VBITimer0,x		; is the lo-register zero?
	bne keeplo
	ldy VBITimer0+1,x	; is the hi-register zero?
	beq notover		; if so, we're done with it
	dec VBITimer0+1,x	; decrement hi, as lo is zero
keeplo:
	dec VBITimer0,x		; decrement lo now
	bne notover		; we're done with it
	ldy VBITimer0+1,x	; is hi now over?
	bne notover
	sec			; A is zero here, always.
	rts
notover:
	clc
	rts	
;;; Paddle trigger write, offset in X (paddle) Y (Stick), value in A
PaddleWrite:
	and #$0f
	sta Stick0,y
	sta Stick2,y
	lsr a
	lsr a
	lsr a
	sta PTrig1,x
	sta PTrig5,x
	lda #$0
	rol a			; get last bit back
	sta PTrig0,x
	sta PTrig4,x
	rts
.endproc
	
	
