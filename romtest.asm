;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: romtest.asm,v 1.6 2013/06/02 20:41:07 thor Exp $		**
;;; **									**
;;; ** In this module:	 ROM checksum computation			**
;;; **********************************************************************

	.include  "kernel.i"
	.include  "reset.i"
	.include  "nmi.i"
	.include  "irq.i"
	.include  "fms.i"
	.include  "cio.i"
	.include  "pia.i"
	.include  "gtia.i"
	
	.segment  "RomTest"

	jmp RomSumLo
	jmp RomSumHi
	jmp VectorInit
	
	;; imported for internal use only
	.global HaTabsInitTable
	
	;;; *** RomSumLo
;;; *** Checksum the $c000 area of the ROM
.proc	RomSumLo
	ldx #$00
	stx RomSum
	stx RomSum+1		; clear both
sumloop:	
	jsr SumRegion		; compute a checksum over the regions noted here
	cpx #$0c
	bne sumloop		; until done
	;; now check for correctness
	lda RomSumLow
	cmp RomSum
	bne fail
	lda RomSumLow+1
	cmp RomSum+1
fail:
	rts
.endproc
;;; *** RomSumHi
;;; *** Checksum the $e000 area of the ROM
.proc	RomSumHi
	lda #$00
	sta RomSum
	sta RomSum+1		; clear both
	ldx #$0c		; continue summing here
	jsr SumRegion		; ditto
	jsr SumRegion		; two regions to sum here
	lda RomSumHigh
	cmp RomSum
	bne fail
	lda RomSumHigh+1
	cmp RomSum+1
fail:
	rts	
.endproc
;;; *** SumRegion
;;; *** Compute the checksum over the by the X register given region
.proc	SumRegion
	ldy #0
initlp:	
	lda SumTable,x
	sta RomPtr,y		; initialize region
	inx
	iny
	cpy #$04
	bne initlp
	ldy #$00
sumloop:
	clc
	lda (RomPtr),y		; read from the ROM
	adc RomSum
	sta RomSum
	bcc nocarry
	inc RomSum+1
nocarry:
	inc RomPtr
	bne nocarry2
	inc RomPtr+1
nocarry2:
	lda RomPtr
	cmp RomRegionEnd
	lda RomPtr+1
	sbc RomRegionEnd+1
	bne sumloop
	rts
;;; the following table contains the
;;; addresses of all rom regions to be checksummed
SumTable:
	.word $c002,$d000	; low memory region
	.word $5000,$5800	; selftest area
	.word $d800,$e000	; mathpack area
	.word $e000,$fff8	; high area
	.word $fffa,$0000	; cpu vector area
.endproc	
;;; the following initializes the vectors during the
;;; reset.
.proc	VectorInit
	;; Os init starts here.
	;; check whether we've basic enabled.
	;; if so, set the basic flag. We must do
	;; this here because we clear all other
	;; flags and the RAM above.
	ldx #$00		; is enabled
	lda PIAPortB		; get again the PIA
	and #$2			; done
	beq isenabled
	inx			; is disabled
isenabled:
	stx BasicDisabled	
	;; initialize the magic cookies
	lda #$5c
	sta InitMagic0
	lda #$93
	sta InitMagic1
	lda #$25
	sta InitMagic2
	;; timing setup
	lda PalNTSC		; read the PAL/NTSC flag
	and #$0e
	bne ntsc
	lda #5			; keyboard repeat
	ldx #1			; pal/ntsc flag
	ldy #40			; keyboard delay
	bne loadtimer
ntsc:
	lda #6
	ldx #0
	ldy #48
loadtimer:
	sta KeyRepeat
	stx PalNTSCShadow
	sty KeyRepeatDelay
	;; initalize system vectors
	ldx #<(VecDeferred+1)	; all from here
initvec:
	lda VectorInitTable,x
	sta VecDLI,x
	dex
	bpl initvec
	lda #<BreakIRQ
	sta BreakVec
	lda #>BreakIRQ
	sta BreakVec+1
	lda #<WarmStartVector
	sta VecNMI
	lda #>WarmStartVector
	sta VecNMI+1
	;; now hatabs init
	ldx #$0
inithatabs:
	lda HaTabsInitTable,x
	sta HaTabs,x
	beq abort
	inx
	lda HaTabsInitTable,x
	sta HaTabs,x
	inx
	lda HaTabsInitTable,x
	sta HaTabs,x
	inx
	bne inithatabs
abort:
	;; Setup the handlers now
	lda RamChkPtr+1		; get himem, last page
	sta RamSize
	sta MemTop+1		; initialize memtop
	lda #>CustomRamStart	; application RAM starts here
	sta MemLo+1		; init as well
	lda #$0
	sta MemLo		; clear lo-bytes
	sta MemTop
	
	;; now launch all handlers
	jsr EditorTable+$c	; init E:
	jsr ScreenTable+$c	; init S:
	jsr KeyboardTable+$c	; init K:
	jsr PrinterTable+$c	; init P:
	jsr TapeTable+$c	; init C:
	;; misc system resources
	jsr CIOInitVector	; init CIO
	jsr SIOInitVector	; init SIO
	jsr NMIInitVector	; init NMIs
	jsr DiskInitVector	; init resident disk handler		
	rts
	;;; init tables for the vectors
VectorInitTable:
	.word ExitDLI		; DLI:	just to a simple DLI
	.word DummyIRQ		; PIA Proceed interrupt is unused
	.word DummyIRQ		; PIA Interrupt interrupt is unused
	.word DummyIRQ		; 6502 BRK software interrupt is unused
	.word KeyIRQ		; pokey keyboard interrupt
	.word SerInIRQ		; serial input interrupt
	.word SerOutIRQ		; serial output interrupt
	.word SerXmtIRQ		; serial transmission done interrupt
	.word DummyIRQ		; pokey timer 1
	.word DummyIRQ		; pokey timer 2
	.word DummyIRQ		; pokey timer 4
	.word OsIRQEntry	; global IRQ handler entry
	.word 0			; VBI timer 1
	.word 0			; VBI timer 2
	.word 0			; VBI timer 3
	.word 0			; VBI timer 4
	.word 0			; VBI timer 5
	.word SystemVBI		; immediate VBI
	.word ExitVBI		; deferred VBI
.endproc
	
