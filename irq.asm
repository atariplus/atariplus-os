;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: irq.asm,v 1.7 2014/01/01 01:23:40 thor Exp $		**
;;; **									**
;;; ** In this module:	 Support for IRQ routines of all kinds		**
;;; **********************************************************************

	.include "irq.i"
	.include "nmi.i"
	.include "antic.i"
	.include "gtia.i"
	.include "pokey.i"
	.include "pia.i"
	.include "kernel.i"
	.include "errors.i"
	.include "editor.i"
	.include "sio.i"

	.segment  "OsHi"
	
;;; ** Entry point for all kinds of IRQs. The CPU starts IRQ processing
;;; ** right at this place.
	.global IRQEntry
.proc	IRQEntry
	cld
	jmp (ImmediateIRQVec)
.endproc
	
;;; ** The Os IRQ default handler follows here
	.global OsIRQEntry
.proc	OsIRQEntry
	pha
	lda #$20
	bit IRQStat		; check for SerIN IRQ first. This has highest priority and must be handled first
	bne noserin
	lda #$df
	sta IRQStat		; clear this bit
	lda IRQStatShadow	; reload the IRQ status
	sta IRQStat
	jmp (SerInVec)
noserin:
	lsr a
	bit IRQStat		; serOut
	bne noserout
	lda #$ef
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (SerOutVec)
noserout:
	lsr a
	bit IRQStat
	bne noserxmt
	and IRQStatShadow
	beq noserxmt		; serial transfer done
	lda #$f7
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (SerXmtVec)
noserxmt:
	lda #$01
	bit IRQStat		; first pokey timer
	bne notimer1
	lda #$fe
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (PokeyTimer1Vec)
notimer1:
	asl a
	bit IRQStat		; second pokey timer
	bne notimer2
	lda #$fd
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (PokeyTimer2Vec)
notimer2:
	asl a
	bit IRQStat		; fourth pokey timer. The third cannot create IRQs
	bne notimer4
	lda #$fb
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (PokeyTimer4Vec)
notimer4:			; keyboard interrupt?
	bvs nokeyirq
	lda #$bf
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (KeyVec)
nokeyirq:
	bmi nobreakirq
	lda #$7f
	sta IRQStat
	lda IRQStatShadow
	sta IRQStat
	jmp (BreakVec)
nobreakirq:
	bit PIAPortACtrl	; interrupt from PIA port A?
	bpl nopiaairq
	lda PIAPortA		; dummy read to clear the interrupt
	jmp (ProceedVec)	; interrupt on PIA proceed vector
nopiaairq:
	bit PIAPortBCtrl	; interrupt from PIA port B?
	bpl nopiabirq
	lda PIAPortB		; dummy read
	jmp (InterruptVec)	; PIA interrupt vector
nopiabirq:
	txa
	pha
	tsx
	lda $103,x		; load the P register on the stack frame
	and #$10		; is the 6502 break register set?
	beq nobrk
	pla
	tax
	jmp (BRKVec)
nobrk:				; spurious interrupt?
	pla
	tax
	pla
	rti
.endproc
;;; *** SerInIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerInIRQ
.proc	SerInIRQ
	tya
	pha			; keep Y register
	lda #$20		; overrun error?
	bit SkStat		; get serial status
	sta SkReset		; clear status (does not set flags)
	bmi noframeerror	; a framing error?
	ldy #FrameError	
	sty SerialStatus	; signal the error (also: clear Z flag)
noframeerror:
	bne nooverrun
	ldy #OverrunError
	sty SerialStatus	; signal the error
nooverrun:
	lda SerialDataDone	; serial buffer is filled?
	bne datadone
	ldy #$00		; no. Data follows
	lda SerDat
	clc
	sta (SerBufLo),y	; store the data
	adc SerialChkSum	; add up the checksum
	adc #$00		; add up the carry
	sta SerialChkSum	; ditto
	inc SerBufLo		; increment the buffer pointer
	bne nocarry
	inc SerBufHi
nocarry:
	lda SerBufLo
	cmp SerBufEndLo
	lda SerBufHi
	sbc SerBufEndHi		; are we done?
	bcc exit		; done?
	lda SerialNoChkSum	; do we need a checksum?
	beq needchksum
	sty SerialNoChkSum	; clear the checksum flag (does not set Z flag)
	bne xferdone		; the transfer is done
needchksum:
	dec SerialDataDone	; serial data is now done, await checksum
	bne exit		; clears Z flag
datadone:	
	lda SerDat		; here:	 is done. Test the checksum
	cmp SerialChkSum
	beq xferdone
	ldy #ChkSumError	; signal a checksum error
	sty SerialStatus
xferdone:	
	dec SerialXferDone	; serial transfer done
exit:		
	pla
	tay
	pla
	rti
.endproc
;;; *** SerOutRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerOutIRQ
.proc	SerOutIRQ
	inc SerBufLo		; next byte. The first has been sent manually
	bne nocarry
	inc SerBufHi
nocarry:
	lda SerBufLo
	cmp SerBufEndLo
	lda SerBufHi
	sbc SerBufEndHi
	bcc senddata
	lda SerialChkSumDone
	bne sumdone
	lda SerialChkSum	; get the checksum
	sta SerDat		; write it out
	dec SerialChkSumDone	; note that we're done with it
	bne exit		; done with it
sumdone:			; serial checksum has been transfered, we're done
	lda #$08
	jsr AbleIRQ		; disable this IRQ, enable XmtDone
	bne exit
senddata:			; carry is clear here
	tya
	pha	
	ldy #$00		; carry is clear here
	lda (SerBufLo),y	; get data
	sta SerDat		; write out data
	adc SerialChkSum
	adc #$00
	sta SerialChkSum
	pla
	tay
exit:	
	pla
	rti	
.endproc
;;; *** SerXmtIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerXmtIRQ
.proc	SerXmtIRQ
	lda SerialChkSumDone	; checksum done already?
	beq start		; if not so, then why are we here? Could be the start of a transfer
	sta SerialSentDone	; xfer is done here
	lda #$00
	jsr AbleIRQ		; disable all IRQs here
	pla
	rti
;; test whether this starts a serial transfer now
start:
	lda SerialSentDone	; are we done already?
	bne exit
	tya
	pha
	lda #$10		; disable this interrupt, enable SerOut
	jsr AbleIRQ
	ldy #$00
	lda (SerBufLo),y	; get data
	sta SerDat		; write out data
	sta SerialChkSum	; keep the checksum
	pla
	tay
exit:
	;; runs into the following to terminate the IRQ
.endproc
;;; *** Dummy IRQ: Used for interrupts that
;;; *** are otherwise unassigned
	.global DummyIRQ
.proc DummyIRQ
	pla
	rti
.endproc
;;; *** Keyboard IRQ here
;;; *** This is the Os supplied keyboard
;;; *** interrupt
	.global KeyIRQ
.proc KeyIRQ
	txa
	pha
	lda KeyCode		; get the keycode we have here
	cmp LastKey
	bne newkey		; if the same, maybe the last key is repeating?
	ldx KeyDelay		; still debouncing?
	bne debounce
newkey:
	ldx KeyDisable		; is the keyboard disabled?
	bne exit		; if not, bail out
	tax			; keep data
	cmp #$9f		; is it ^1?
	bne nostartstop
	lda StartStopFlag
	eor #$ff
	sta StartStopFlag	; toggle it on/off
	bcs keydone		; jumps always (the above eq condition set c)
nostartstop:
	and #$3f		; check for the HELP key
	cmp #$11
	bne nothelp
	stx HelpFlag		; keep the key code for it
	beq keydone		; jumps always
nothelp:
	stx KeyCodeShadow	; otherwise, keep the code for later
	stx LastKey		; keep for comparison
keydone:
	lda #$03		; debounce value
	sta KeyDelay
	lda #$00
	sta Attract		; reset the attract mode
debounce:
	lda KeyRepeatDelay
	sta KeyTimer		; reload the keyboard repeat timer
exit:
	pla
	tax
	pla
	rti
.endproc
;;; *** Break IRQ here
;;; *** This is the Os supplied Break key
;;; *** interrupt
	.global BreakIRQ
BreakIRQ:
	lda #$00
	sta BreakFlag
	sta StartStopFlag	; continue output
	sta CursorInhibit	; make cursor visible
	sta Attract		; clear the attract mode
	pla
	rti


