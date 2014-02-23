;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: tapeinit.asm,v 1.4 2013/06/01 17:23:21 thor Exp $        **
;;; **                                                                  **
;;; ** In this module:   Tape Init functions that do not stay resident	**
;;; **********************************************************************

	.include "kernel.i"
	.include "fmsreloc.i"
	.include "reset.i"
	.include "cio.i"
	.include "dup.i"
	.segment "tapeinit"

	.global	TapeResidentLength
	
TapeLoPtr	=	$3d	;misused here for initializing the tape
TapeHiPtr	=	$3f	;ditto, resident hi-part
TapeCartPosition=	$ba00	;where the C: goes behind the cart
TapeResidentLoad=	$3440	;where the resident part will be loaded
	
;;; *** This dummy RTS here is just because we don't want to be called
;;; *** by a stupid Os/A+ willing to run us. This program is run through
;;; *** the init vector, the run vector is reserved for RS-232 (R:)
;;; *** initialization.
	rts
;;; *** And here we go.
.proc	Start
	;; first, get the vectors
	jsr CheckForFmsOvl
	;; find the memory and allocate
	jsr FindMemPtrs

	ldx #0
	jsr IsCartPresent
	bne nocart
	inx			;cart is there.
nocart:	
	txa
	pha			;keep the cart flag
	
	jsr SwitchOffVector
	;; copy the resident part to its final position
	lda #<TapeResidentLoad
	sta ZPut
	lda #>TapeResidentLoad
	sta ZPut+1

	lda TapeHiPtr
	sta ZAdr
	lda TapeHiPtr+1
	sta ZAdr+1
	
	lda #<TapeResidentLength
	sta ZLen
	lda #>TapeResidentLength
	sta ZLen+1
	jsr MemCpy

	;; relocate to the final positon
	jsr Relocate

	;; create the low-memory region
	jsr CreateTapeLoRegion
	;;
	;; and finally initialize
	lda TapeHiPtr
	clc
	adc #<(2+2+7*3)
	sta ZAdr
	lda TapeHiPtr+1
	adc #>(2+2+7*3)
	sta ZAdr+1
	jsr CallInit
	;; If this is not a warmstart, ensure that the cart contents
	;; are erased properly.
	lda BootFlag
	ora #1
	sta BootFlag

	;; ensure the cart re-reads memlo
	lda WarmStartFlag
	beq iscold
	lda #0
	sta WarmStartFlag
	lda FmsBootFlag
	ora #1
	sta FmsBootFlag
iscold:	
	;; restore the cart switch flag
	pla
	bne switchon
	jsr SwitchOffVector
	rts
switchon:	
	jsr SwitchOnVector
	rts
.endproc
;;; *** CallInit
;;; *** Call the initialize vector of the resident part of the tape
.proc	CallInit
	jmp (ZAdr)
.endproc
;;; *** CreateTapeLoRegion
;;; *** create the low memory resident part of the tape
;;; *** drive. The pointer to the low-memory region
;;; *** is in TapeLoPtr, the higher one in TapeHiPtr.
.proc	CreateTapeLoRegion
	;; first, create the Hatabs table in
	;; tapeLo. The first entry to jump to start directly
	;; behind Hatabs
	clc
	lda TapeLoPtr
	adc #<(2*6-1)
	sta ZAdr
	lda TapeLoPtr+1
	adc #>(2*6-1)
	sta ZAdr+1
	;;
	ldy #0
inithtabs:
	lda ZAdr
	sta (TapeLoPtr),y
	iny
	lda ZAdr+1
	sta (TapeLoPtr),y
	iny
	;; each entry is nine bytes long
	clc
	lda #9
	adc ZAdr
	sta ZAdr
	bcc noinc
	inc ZAdr+1
noinc:
	cpy #6*2		;we need 6 entries
	bcc inithtabs
	;;
	;; Install now the back-pointer to HaTabs in the hi-resident area
	ldy #0
	lda TapeLoPtr
	sta (TapeHiPtr),y
	iny
	lda TapeLoPtr+1
	sta (TapeHiPtr),y
	iny
	lda TapeEndPtr
	sta (TapeHiPtr),y
	iny
	lda TapeEndPtr+1
	sta (TapeHiPtr),y
	iny
	;; Get the entries in the resident hi table.
	;; they start right behind this vector and the
	;; new MemHi
	clc
	tya
	adc TapeHiPtr
	sta ZAdr
	lda #0
	adc TapeHiPtr+1
	sta ZAdr+1
	;;
	;;  Create now the resident jump table
	ldy #6*2		;restore the offset
	ldx #7			;need seven entries (one for init, which comes last)
createvectors:
	lda #$20		;jsr to switch the card off
	sta (TapeLoPtr),y
	iny
	lda SwitchOffVector+1
	sta (TapeLoPtr),y
	iny
	lda SwitchOffVector+2
	sta (TapeLoPtr),y
	iny
	
	lda #$20		;jsr to the actual entry
	sta (TapeLoPtr),y
	iny
	lda ZAdr
	sta (TapeLoPtr),y
	iny
	lda ZAdr+1
	sta (TapeLoPtr),y
	iny
	;; each entry is three bytes long.
	clc
	lda ZAdr
	adc #3
	sta ZAdr
	bcc noinc2
	inc ZAdr+1
noinc2:
	lda #$4c		;a jmp to turn the cart on again
	sta (TapeLoPtr),y
	iny
	lda SwitchOnVector+1
	sta (TapeLoPtr),y
	iny
	lda SwitchOnVector+2
	sta (TapeLoPtr),y
	iny
	dex
	bne createvectors
	;; create a last vector which is
	;; used to redirect the initialization chain
	lda #$20		;a jmp to turn the cart on again
	sta (TapeLoPtr),y
	iny
	lda SwitchOnVector+1
	sta (TapeLoPtr),y
	iny
	lda SwitchOnVector+2
	sta (TapeLoPtr),y
	iny
	lda #$20
	sta (TapeLoPtr),y
	iny
	lda DosInit
	sta (TapeLoPtr),y
	iny
	lda DosInit+1
	sta (TapeLoPtr),y
	iny
	lda #$4c		;jsr to switch the card off again
	sta (TapeLoPtr),y
	iny
	lda SwitchOffVector+1
	sta (TapeLoPtr),y
	iny
	lda SwitchOffVector+2
	sta (TapeLoPtr),y
	iny
	;; done, now compute
	;; DosInit, which should point to the last vector
	tya
	sec
	sbc #9*2
	clc
	adc TapeLoPtr
	sta DosInit
	lda TapeLoPtr+1
	adc #0
	sta DosInit+1
	rts
.endproc
;;; *** FindMemPtrs
;;; *** Find the target locations of the tape handler
;;; *** given the FmsOvl configuration
.proc	FindMemPtrs
	lda MemLo
	sta TapeLoPtr
	lda MemLo+1
	sta TapeLoPtr+1

	ldx #0			;cart present flag
	ldy #0			;behind cart flag
	jsr IsCartPresent
	bne iscart
	inx			;cart is there.
iscart:
	jsr SwitchOffVector	;try to disable
	jsr IsCartPresent
	bne behindcart		;if cart could be turned off, put behind cart
	;; here: regular case, do not put behind cart
regularcase:
	;; any data from the command line?
	jsr ParseLoadingPosition
	bcc havetapeposnocart
	
	clc
	lda TapeLoPtr
	adc #<(2*6+9*8)		;size of the lo-memory area
	sta TapeHiPtr
	lda TapeLoPtr+1
	adc #>(2*6+9*8)
	sta TapeHiPtr+1
	;; Compute the size of the overall
	clc
	lda TapeHiPtr
	adc #<TapeResidentLength
	sta TapeEndPtr
	lda TapeHiPtr+1
	adc #>TapeResidentLength
	sta TapeEndPtr+1
	bne done

havetapeposnocart:	
	clc
	lda TapeLoPtr
	adc #<(2*6+9*8)
	sta TapeEndPtr
	lda TapeLoPtr+1
	adc #>(2*6+9*8)
	sta TapeEndPtr+1
	bne done
	
behindcart:
	jsr SwitchOnVector	;can the cart be switched on again?
	jsr IsCartPresent
	bne regularcase		;if not, then it wasn't switched off before

	;; any data from the command line?
	jsr ParseLoadingPosition
	bcc havetapepos
	
	lda #<TapeCartPosition
	sta TapeHiPtr
	lda #>TapeCartPosition
	sta TapeHiPtr+1

havetapepos:	
	clc
	lda TapeLoPtr
	adc #<(2*6+9*8)
	sta TapeEndPtr
	lda TapeLoPtr+1
	adc #>(2*6+9*8)
	sta TapeEndPtr+1
	iny			;is behind the cart
	jsr SwitchOffVector
done:
	jsr SwitchOnVector
	txa
	bne keepon		;was turned on
	jsr SwitchOffVector	;turn off again
keepon:
	;; if no switching was installed, restore the jump vectors as they are not needed
	tya
	bne exit
	
	lda #<RTSVector
	sta SwitchOffVector+1
	sta SwitchOnVector+1
	
	lda #>RTSVector
	sta SwitchOffVector+2
	sta SwitchOnVector+2
exit:
	rts
.endproc
;;; *** IsCartPresent
;;; *** Returns Z=0 (ne) if no cart is present, Z=1 (eq) if a cart is present
.proc	IsCartPresent
	lda $a000
	inc $a000
	cmp $a000
	beq exit
	dec $a000
	lda #$ff
exit:
	rts
.endproc
;;; *** CheckForFmsOvl
;;; *** Test whether the FMS overlay manager is active.
;;; *** and initialize the jmp table.
.proc	CheckForFmsOvl
	lda #<RTSVector
	sta SwitchOffVector+1
	sta SwitchOnVector+1
	
	lda #>RTSVector
	sta SwitchOffVector+2
	sta SwitchOnVector+2
	
	lda #$20		;must be a JSR
	cmp RegularEntry
	bne noswitch
	cmp POBEntry
	bne noswitch
	lda #$4c		;must be a JMP
	cmp RegularEntry+6
	bne noswitch
	cmp POBEntry+6
	bne noswitch
	
	lda RegularEntry+1
	cmp POBEntry+1
	bne noswitch
	lda RegularEntry+2
	cmp POBEntry+2
	bne noswitch

	lda RegularEntry+7
	cmp POBEntry+7
	bne noswitch
	lda RegularEntry+8
	cmp POBEntry+8
	bne noswitch

	;; store the vectors
	lda #<SwitchOff
	sta SwitchOffVector+1
	lda #>SwitchOff
	sta SwitchOffVector+2
	lda #<SwitchOn
	sta SwitchOnVector+1
	lda #>SwitchOn
	sta SwitchOnVector+2
noswitch:
	rts
.endproc
;;; MemCpy
;;; *** Copy the memory from ZPut to ZAdr, with ZLen bytes.
;;; *** Return the pointer behind the target in ZAdr, behind the source in ZPut
.proc	MemCpy
	ldy ZLen
	beq zero
copylo:
	dey
	lda (ZPut),y
	sta (ZAdr),y
	tya
	bne copylo
	;; Adjust the lo-mem part of it
	clc
	lda ZPut
	adc ZLen
	sta ZPut
	lda ZPut+1
	adc #0
	sta ZPut+1

	clc
	lda ZAdr
	adc ZLen
	sta ZAdr
	lda ZAdr+1
	adc #0
	sta ZAdr+1
	
	ldy #0
zero:
	lda ZLen+1
	beq exit
zcopy:	
	lda (ZPut),y
	sta (ZAdr),y
	iny
	bne zcopy
	inc ZPut+1
	inc ZAdr+1
	dec ZLen+1
	bne zcopy
exit:	
	rts
.endproc
;;; Relocate
;;; *** Relocate the data pointed to by TapeHiPtr by the data at ZPut
;;; *** Target data is originally assembled to page 7
.proc	Relocate
	lda TapeHiPtr
	sta ZAdr
	sta ZLen
	lda TapeHiPtr+1
	sta ZAdr+1
	sec
	sbc #7			; only the offset adjust, target was page 7 and up
	sta ZLen+1		;relocation offset in ZLen, ZLen+1
	
	inc ZPut
	bne nohi0
	inc ZPut+1
nohi0:
	
loopnext:
	inc ZPut
	bne nohi
	inc ZPut+1
nohi:	
	ldy #0
	lda (ZPut),y		; get the next increment to the relocation data
	beq exit
	
	clc
	adc ZAdr
	sta ZAdr
	lda ZAdr+1
	adc #0
	sta ZAdr+1

	lda (ZAdr),y
	clc
	adc ZLen
	sta (ZAdr),y
	iny
	lda (ZAdr),y
	adc ZLen+1
	sta (ZAdr),y
	bne loopnext
exit:
	rts
.endproc
;;; *** ParseLoadingPosition
;;; *** Parse off the position of the tape handler
;;; *** from the command line. Returns C=1 if
;;; *** no valid position could be found. Returns C=0
;;; *** if the command line position is then in
;;; *** TapeHiPtr
.proc	ParseLoadingPosition
	ldx #0
findspace:	
	lda DupBuffer,x
	beq noarg
	cmp #$9b
	beq noarg
	cmp #' '		;the command separator
	beq continuescan
	inx
	bpl findspace
	bmi noarg
continuescan:
	lda DupBuffer,x
	cmp #' '
	bne check
	inx
	bpl continuescan
	bmi noarg
	;; here: possible hex argument
check:
	lda DupBuffer,x
	cmp #'$'
	bne parsedigit
	inx
parsedigit:	
	lda #0
	sta TapeHiPtr
	sta TapeHiPtr+1
	jsr GetHexDigit
	bcs noarg
	jsr GetHexDigit
	bcs end
	jsr GetHexDigit
	bcs end
	jsr GetHexDigit
end:	
	lda DupBuffer,x
	cmp #$9b
	bne noarg
	;; here everything is fine
	clc
	rts
noarg:	sec
	rts
.endproc
;;; *** GetHexDigit
;;; *** Parse off the next hex digit from DupBuffer+x
;;; *** Return C=0 if ok, C=1 otherwise.
.proc	GetHexDigit
	lda DupBuffer,x
	cmp #'0'
	bcc nodigit
	cmp #'9'+1
	bcc digitten
	cmp #'A'
	bcc nodigit
	cmp #'F'+1
	bcs nodigit
	sbc #'A'-10-1
	bcs insert
digitten:
	sbc #'0'-1
insert:
	pha
	lda TapeHiPtr
	asl a
	rol TapeHiPtr+1
	asl a
	rol TapeHiPtr+1
	asl a
	rol TapeHiPtr+1
	asl a
	rol TapeHiPtr+1
	sta TapeHiPtr
	pla
	ora TapeHiPtr
	sta TapeHiPtr
	inx
	clc
	rts
nodigit:
	sec
	rts
.endproc
;;; *** SwitchOffVector
;;; *** Turn the cart off (if possible)
.proc	SwitchOffVector
	jmp $ffff		;to be patched
.endproc
;;; *** SwitchOnVector
;;; *** Turn the cart on (if possible)
.proc	SwitchOnVector
	jmp $ffff		;to be patched
.endproc
TapeEndPtr:	.word	0 ;will point to the new MemLo
