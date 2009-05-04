;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: loader.asm,v 1.3 2003/06/02 20:25:43 thor Exp $		**
;;; **									**
;;; ** In this module:	The loader of the main RS-232 handler		**
;;; **********************************************************************

	.include "sio.i"
	.include "reset.i"
	.include "kernel.i"
	
	.segment  "Loader"

	RelocPtr	=	$32 ; misuse the SIO registers, it's then not needed
	Data		=	$600
	
;;; *** Bootstrap code
Boot:
	.byte 0			; boot flag
	.byte 3			; # of sectors to load. This is irrelevant
	.word Boot		; load address
	.word Start		; run address. We won't need it.
;;; *** Program gets run here, ignoring all the init
;;; *** mess. This just loads the necessary data into the
;;; *** SIO vector and bootstraps the main
LoadHandler:
	ldx #3
lp:	lda DCBInit,x
	sta SIODeviceId,x	; install the device
	dex
	bpl lp			; until done
	lda MemLo
	sta SIOBufferLo		; load into MemLo
	lda MemLo+1
	sta SIOBufferHi
	lda #8
	sta SIOTimeout		; store the timeout
	lda Data		; get the size
	sta SIOSizeLo
	lda Data+1
	sta SIOSizeHi		; enter size
	jsr SIOVector		; bootstrap the code
	bmi exit
	;; Now relocate the code
	lda MemLo
	sta RelocPtr
	lda MemLo+1
	sta RelocPtr+1
	ldx #$00		; we have luckely less than a page of relocation data (urgs, I hope)
reloop:
	lda Data+2,x		; get the next relocation data
	beq Start		; abort when done
	clc
	adc RelocPtr		; add up the offset
	sta RelocPtr
	bcc nocarry
	inc RelocPtr+1
nocarry:
	ldy #$00
	clc
	lda (RelocPtr),y	; get the data
	adc MemLo		; add up
	sta (RelocPtr),y
	iny
	lda (RelocPtr),y
	adc MemLo+1		; add up the high-byte
	sec
	sbc #$07		; the code has been compiled for page 7
	sta (RelocPtr),y
	inx			; next relocation entry
	bne reloop
Start:
	;; ensure that the next reset resets the cart, unless we
	;; are still resetting, however.
	lda WarmStartFlag
	beq iscold
	;; we're currently not just booting up. Ensure that the next
	;; reset reloads memlo by signalling a "coldstart" for the
	;; cart.
	lda FmsBootFlag
	ora #$01
	sta FmsBootFlag
iscold:	
	jmp (MemLo)		; now launch 
exit:
	sec			; signal that something's wrong here
	rts
DCBInit:
	.byte $50		; deviceId
	.byte $01		; unit
	.byte '&'		; command
	.byte $40		; direction:	from interface box->computer
;;; The makefile will place the size and the relocation code here
;;; We must have no data at $5a9 since some "fix" codes will try to modify
;;; this (non-standard) code here.
	
		
