;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: diskio6init.asm,v 1.4 2013/06/02 20:41:04 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DiskIO helper 					**
;;; **********************************************************************

	.include "diskio6init.i"
	.include "diskio6lo.i"
	.include "diskio6hi.i"
	.include "fmsreloc.i"
	.include "kernel.i"
	.include "cio.i"
	.include "reset.i"
	
	.segment "diskioinit"
	
;;; *** Start of the installation
;;; *** Start
.proc	Start

	jsr CheckCartType
	tay
	bne havecart
	rts
havecart:
	;; check where the resident part should go
	lda MemLo
	sta ResidentLo
	lda MemLo+1
	sta ResidentLo+1

	;; install the low-part of the resident part
	lda ResidentLo
	sta ZAdr
	lda ResidentLo+1
	sta ZAdr+1
	;; source address
	lda #<ResidentLoLoad
	sta ZPut
	lda #>ResidentLoLoad
	sta ZPut+1
	
	lda #<LoLen
	sta ZLen
	lda #>LoLen
	sta ZLen+1
	jsr MemCpy

	lda ZAdr
	sta ResidentEnd
	lda ZAdr+1
	sta ResidentEnd+1
	
	;; keep the end for the time
	lda ZAdr
	sta ResidentHi
	lda ZAdr+1
	sta ResidentHi+1

	;; Relocate the part to ZAdr with data from ZPut
	lda ResidentLo
	sta ZAdr
	lda ResidentLo+1
	sta ZAdr+1
	jsr Relocate
	
	lda ResidentLo
	sta ZAdr
	lda ResidentLo+1
	sta ZAdr+1

	ldy #<(FMSSwitchOffOffset+1)
	lda #<$e4c0		;Os RTS
	sta (ZAdr),y
	sta InitSwitchOff+1
	iny
	lda #>$e4c0
	sta (ZAdr),y
	sta InitSwitchOff+2

	ldy #<(FMSSwitchOnOffset+1)
	lda #<$e4c0
	sta (ZAdr),y
	sta InitSwitchOn+1
	iny
	lda #>$e4c0
	sta (ZAdr),y
	sta InitSwitchOn+2
	
	;; check where the high-part should go
	lda #$20		;must be a JSR
	cmp RegularEntry
	bne notcart
	cmp POBEntry
	bne notcart
	lda #$4c		;must be a JMP
	cmp RegularEntry+6
	bne notcart
	cmp POBEntry+6
	bne notcart
	
	lda RegularEntry+1
	cmp POBEntry+1
	bne notcart
	lda RegularEntry+2
	cmp POBEntry+2
	bne notcart

	lda RegularEntry+7
	cmp POBEntry+7
	bne notcart
	lda RegularEntry+8
	cmp POBEntry+8
	bne notcart

	;; ok, here we have routines to disable the cart
	ldy #<(FMSSwitchOffOffset+1)
	lda POBEntry+1		;offset to switch the cart off
	sta (ZAdr),y
	sta InitSwitchOff+1
	iny
	lda POBEntry+2
	sta (ZAdr),y
	sta InitSwitchOff+2

	ldy #<(FMSSwitchOnOffset+1)
	lda POBEntry+7
	sta (ZAdr),y
	sta InitSwitchOn+1
	iny
	lda POBEntry+8
	sta (ZAdr),y
	sta InitSwitchOn+2

	lda #<ResidentHiTarget
	sta ResidentHi
	lda #>ResidentHiTarget
	sta ResidentHi+1
notcart:
	;; Install the offsets into the lo-memory region
	lda ResidentLo
	sta ZAdr
	lda ResidentLo+1
	sta ZAdr+1
	ldy #<(NewGetOffset+1)
	clc
	lda ResidentHi
	adc #<DiskIOGetOffset
	sta (ZAdr),y
	iny
	lda ResidentHi+1
	adc #>DiskIOGetOffset
	sta (ZAdr),y

	;; ditto for the reset entry
	ldy #<(ResetOffset+1)
	clc
	lda ResidentHi
	adc #<ReInstallOffset
	sta (ZAdr),y
	iny
	lda ResidentHi+1
	adc #>ReInstallOffset
	sta (ZAdr),y
	
	;; copy now the hi-part into its target address
	;; place the hi-part into its target address
	jsr InitSwitchOff
	
	lda ResidentHi
	sta ZAdr
	lda ResidentHi+1
	sta ZAdr+1
	;; source address
	lda #<ResidentHiLoad
	sta ZPut
	lda #>ResidentHiLoad
	sta ZPut+1
	
	lda #<HiLen
	sta ZLen
	lda #>HiLen
	sta ZLen+1
	jsr MemCpy

	;; check whether we could deactive the cart
	;; if not so, adjust MemLo
	lda ResidentHi+1
	cmp #$a0
	bcs ishidden
	lda ZAdr
	sta ResidentEnd
	lda ZAdr+1
	sta ResidentEnd+1
ishidden:
	
	;; Relocate the part to ZAdr with data from ZPut
	lda ResidentHi
	sta ZAdr
	lda ResidentHi+1
	sta ZAdr+1
	jsr Relocate


	
	;; now install the cart type
	jsr InitSwitchOn
	jsr CheckCartType
	pha
	jsr InitSwitchOff

	ldy #<(QuoteTypeOffset)
	lda ResidentHi
	sta ZAdr
	lda ResidentHi+1
	sta ZAdr+1
	pla
	sta (ZAdr),y

	;; Install the pointer to HaTabs into the high-memory area
	ldy #<(HaTabsVectorOffset)
	lda ResidentLo
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	sta (ZAdr),y
	
	;; Install the target value of MemLo
	ldy #<(MemLoOffset)
	lda ResidentEnd
	sta (ZAdr),y
	iny
	lda ResidentEnd+1
	sta (ZAdr),y

	;; Install the cart-switch-on function
	;; in the hi-part
	ldy #<(EnableReturnOffset+1)
	lda ResidentLo
	clc
	adc #<FMSSwitchOnOffset
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	adc #>FMSSwitchOnOffset
	sta (ZAdr),y

	;; install the NewGet offset
	ldy #<(NewGetVectorOffset)
	lda ResidentLo
	clc
	adc #<(NewGetEntryOffset-1)
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	adc #>(NewGetEntryOffset-1)
	sta (ZAdr),y

	;; Install the CIO call-back
	ldy #<(CallCIOOffset+1)
	lda ResidentLo
	clc
	adc #<CIOEntryOffset
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	adc #>CIOEntryOffset
	sta (ZAdr),y

	;; Install the disk buffer pointer
	ldy #<DirBufferVectorOffset
	lda ResidentLo
	clc
	adc #<DirBufferOffset
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	adc #>DirBufferOffset
	sta (ZAdr),y

	ldy #<(OldInitOffset+1)
	lda ResidentLo
	clc
	adc #<NewInitEntryOffset
	sta (ZAdr),y
	iny
	lda ResidentLo+1
	adc #>NewInitEntryOffset
	sta (ZAdr),y
	
	;; Start patching the Low memory region
	lda ResidentLo
	sta ZAdr
	lda ResidentLo+1
	sta ZAdr+1

	;; Install the old get offset into LoMem
	ldy #<(NewGetEntryOffset+1)
	lda EditorTable+4
	clc
	adc #1
	sta (ZAdr),y
	lda EditorTable+5
	iny
	adc #0
	sta (ZAdr),y
	
	;; install the reset-resident logic
	ldy #<(ResetOldOffset+1)
	lda DosInit
	sta (ZAdr),y
	iny
	lda DosInit+1
	sta (ZAdr),y
	clc
	lda ResidentLo
	adc #<ResetNewOffset
	sta DosInit
	lda ResidentLo+1
	adc #>ResetNewOffset
	sta DosInit+1
	
	jsr InitSwitchOn
	
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
	
	;; ok, finally, install it.
	lda #<ResetNewOffset
	clc
	adc ResidentLo
	sta ZAdr
	lda #>ResetNewOffset
	adc ResidentLo+1
	sta ZAdr+1
	;; ok, install me.
	jmp (ZAdr)
	;; this also switches the cart on
exit:
	rts
.endproc
;;; InitSwitchOff
;;; *** Switch off the cartridge so we can copy data behind it
;;; *** This will be installed by the init routine itself and will
;;; *** point to the resident FMS memory area
.proc	InitSwitchOff
	jmp $ffff
.endproc
;;; InitSwitchOn
;;; *** Switch on the cartridge
.proc	InitSwitchOn
	jmp $ffff
.endproc
;;; Check which type of cartridge is currently installed
;;; Returns $22 for basic and $23 for Mac/65
.proc	CheckCartType
	ldy #$22
	lda $a001
	cmp #202		;Basic?
	beq exit
	ldy #$23
	lda $bfff
	cmp #$b0		;Mac/65?
	beq exit
	ldy #$0
exit:
	tya
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
;;; *** Relocate the data pointed to by ZAdr by the data at ZPut
;;; *** Target data is originally assembled to page 7
.proc	Relocate
	lda ZAdr
	sta TargetAddress
	lda ZAdr+1
	sec
	sbc #7			; only the offset adjust, target was page 7 and up
	sta TargetAddress+1
	
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
	adc TargetAddress
	sta (ZAdr),y
	iny
	lda (ZAdr),y
	adc TargetAddress+1
	sta (ZAdr),y
	bne loopnext
exit:
	rts
.endproc
;;; Temporaries
ResidentLo:	.word	0	; start address of the lo-memory resident part
ResidentHi:	.word	0	; start address of the hi-resident part
ResidentEnd:	.word	0	; how MemLo has to be adjusted
TargetAddress:	.word	0	; relocation target offset

