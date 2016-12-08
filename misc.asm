;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: misc.asm,v 1.24 2014/03/14 23:16:43 thor Exp $		**
;;; **									**
;;; ** In this module:	 Miscellaneous helper functions in the kernel	**
;;; **********************************************************************

	.include "misc.i"
	.include "antic.i"
	.include "pokey.i"
	.include "gtia.i"
	.include "cio.i"
	.include "sio.i"
	.include "errors.i"
	.include "irq.i"
	.include "reset.i"
	.include "kernel.i"
	.include "pia.i"
	.include "fms.i"
	.include "nmi.i"
		
	.segment  "OsHi"
	
;;; *** SetIRQ
;;; *** Define an IRQ vector.
;;; *** A defines the vector, Y low and X hi
	.global SetIRQ
.proc	SetIRQ
	php			; keep processor status
	asl a			; for the vector number, multiply by two
	sta IRQTemp		; keep as temporary
	txa
	sei
lp:	
	ldx YPos
	cpx #$7b
	bcc set			; not near the VBI
	cpx #$7c
	bcc lp
	nop
	nop
	nop
	nop
	nop			; not at the beginning of line 248
set:	
	ldx IRQTemp
	sta ImmediateIRQVec+1,x	; set Hi
	tya
	sta ImmediateIRQVec,x	; set Lo
	plp			; restore I flag
	rts
.endproc
;;; *** InitNMI
;;; *** Initialize the NMI control register
;;; *** and setup Trig3
	.global InitNMI
.proc	InitNMI	
	lda Trigger3
	sta Trigger3Shadow		; setup the shadow register (not really required, for compatibility)
	lda #$40		; VBI on, DLI off
	sta NMIEnable
	rts
.endproc
;;; *** ReadTapeBlock
;;; *** Read a block from the tape
;;; *** As we do not support tape IO anymore,
;;; *** Just load Y with an error status
;;; *** and return
	.global ReadTapeBlock
.proc	ReadTapeBlock
	;; runs into the following
.endproc
;;; *** OpenTapeChannel
;;; *** Open a channel for reading data
;;; *** from the tape. As we do not support
;;; *** the tape anymore, just return an error.
	.global OpenTapeChannel
.proc	OpenTapeChannel
	ldy #TimeoutError	; as if the tape didn't receive
	rts
.endproc
;;; *** MountHandler
;;; *** Add a new handler to HATABS.
;;; *** X register = letter of the device to mount
;;; *** A device command table, hi byte
;;; *** Y device command table, lo byte.
;;; *** Returns with C cleared if worked successfully
;;; *** with carry set if the device is already mounted
;;; *** with N set if the table is full.
	.global MountHandler
.proc	MountHandler
	pha			; keep hi
	tya
	pha			; keep lo
	txa			; device name -> X
	ldx #$00
hloop:
	cmp HaTabs,x		; found the name already?
	beq ispresent
	ldy HaTabs,x		; is this slot free?
	beq freeslot
	inx
	inx
	inx			; to the next slot
	cpx #$22		; iterated thru all slots?
	bcc hloop
	;; here:	no free slot found
	pla
	pla
	ldy #$ff		; set N flag, C remains set
	rts
ispresent:
	pla
	tay			; return lo
	pla			; return hi
	inx			; point to handler table directly
	sec			; set indicator flag
	rts
freeslot:			; found a free slot to enter the device
	sta HaTabs,x		; store device name
	pla			; get low
	sta HaTabs+1,x		; store
	pla			; get high
	sta HaTabs+2,x		; store
	clc			; worked fine
	rts
.endproc
;;; *** Selftest, Powerup and Bye
;;; *** vector (memory pad) are all unused
	.global Bye
.proc	Bye
	;; runs in the following
.endproc
;;; *** PowerupDisplay
	.global PowerupDisplay
.proc	PowerupDisplay
	;; runs into the following
.endproc
;;; *** SelfTest
	.global SelfTest
.proc	SelfTest
	sei
	lda #$0
	sta NMIEnable
	sta BootFlag
	sta WarmStartFlag
	lda #$c1		; initialize the fms and run the dup
	jsr InitVectors
	jsr FmsInitVector
	jmp LaunchDosVector
.endproc
	
	
;;; *** CIODirect
;;; *** Dispatch a CIO command with disabling the selftest (service for DUP in the selftest ROM)
.proc	CIODirect
	pha
	jsr HideSelfTest
	pla
	jsr CIOVector
	pha
	jsr MapSelfTest
	pla
	rts
.endproc

;;; *** RunRunVector
;;; *** A helper for the DUP that disables the DUP ROM and jumps indirectly thru the
;;; *** FMS run vector.
	.global RunRunVector
.proc	RunRunVector
	jsr HideSelfTest
	jsr ThruRunVector
	jsr MapSelfTest
	rts
ThruRunVector:
	jmp (RunVector)		; launch by this vector
.endproc
;;; *** Boot850
;;; *** Boot the external 850 interface
;;; *** returns with carry set on error, other
;;; *** wise with carry clear.
	.global Boot850
.proc	Boot850
	ldx #12-1
lp:
	lda Init850Boot,x
	sta SIODeviceId,x
	dex
	bpl lp
	jsr SIOVector
	bmi fail
	ldx #12-1
lp2:
	lda BootSpace,x
	sta SIODeviceId,x
	dex
	bpl lp2
	jsr SIOVector		; boot the handler
	bmi fail
	jmp BootSpace+6		; run the boot code
Init850Boot:
	.byte $50,$01		; RS232-handler, unit 1
	.byte '?',$40		; get boot parameter, read
	.word BootSpace
	.byte $06,$00		; timeout
	.word 12,0		; size, aux1,2
fail:				; runs into the following
.endproc
;;; *** UnlinkParHandler
;;; *** Remove a device from the parallel port
;;; *** extended handler list.
;;; *** As we don't support the parallel port mess,
;;; *** we just bail out.
	.global UnlinkParHandler
.proc	UnlinkParHandler
	;; runs into the following
.endproc
;;; *** LinkParHandler
;;; *** Add a device to the parallel port
;;; *** extended handler list.
;;; *** As we don't support the parallel port mess,
;;; *** we just bail out.
	.global LinkParHandler
.proc	LinkParHandler
	sec
	rts
.endproc
;;; *** MapSelfTest
;;; *** Enable the self test mapping
	.global MapSelfTest
.proc	MapSelfTest
	lda PIAPortB
	and #$7f		; again, this is in the selftest area
	sta PIAPortB
	rts
.endproc
;;; *** HideSelfTest
;;; *** Disable the self test mapping
	.global HideSelfTest
.proc	HideSelfTest
	lda PIAPortB
	ora #$80
	sta PIAPortB
	rts
.endproc
;;; *** LaunchDup
;;; *** Run the built-in dup
.proc	LaunchDup
	jsr MapSelfTest
	jmp RunDup		; is within the selftest
.endproc
;;; AudioCleanup
;;; Clean the Pokey and serial registers after a SIO operation
	.global AudioCleanup
.proc	AudioCleanup
	lda #$00
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl2
	sta AudCtrl3		; clear pokey audio:	 Hmm, less would be possible
	beq AbleIRQ		; disable all serial interrupts
.endproc
;;; InitForSend
;;; Setup pokey such that we can send bytes
;;; This is also available as a kernel vector
	.global InitForSend
.proc	InitForSend
	jsr AudioInit
	lda #$20
	jsr SetSerialMode
	lda #$10		; serout IRQ on
	bne AbleIRQ		; for compatibility, required by misc. Not required by SIO. Jumps always
.endproc
;;; InitForReceive
;;; Setup pokey such that we can receive bytes
.proc	InitForReceive
	jsr AudioInit
	lda #$10
	jsr SetSerialMode
	lda #$20		; serin IRQ on
	;; runs into the following
.endproc
;;; AbleIRQ
;;; Enable the pokey IRQ given in A
	.global	AbleIRQ
.proc	AbleIRQ
	pha
	lda #$c7
	and IRQStatShadow	; first disable all serial IRQ
	sta IRQStatShadow	; keep
	pla
	ora IRQStatShadow	; or this IRQ flag in
	sta IRQStatShadow
	sta IRQStat		; and now in pokey
	rts
.endproc
;;; *** SetSerialMode
;;; *** Change the pokey serial transmission mode by
;;; *** setting the bits in A in SkStat
	.global SetSerialMode
.proc	SetSerialMode
	pha
	lda #$07
	and SkStatShadow
	sta SkStatShadow
	pla
	ora SkStatShadow
	sta SkStatShadow
	sta SkStat
	sta SkReset
	rts
.endproc
;;; AudioInit
;;; Initialize Pokey Audio for
;;; serial transfer
	.global AudioInit
.proc	AudioInit
	lda #$28		; setup pokey
	sta AudFreq2
	lda #$00
	sta AudFreq3		; approximately 19.200 baud, I suppose	
	lda #$28
	sta AudioCtrl		; setup the pokey mode
	lda #$a8		; medium loud?
	ldy SerialSound		; or off?
	bne loud
	lda #$a0		; no sound
loud:
	sta AudCtrl3
	lda #$a0
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl2
	rts
.endproc
;;; ComputeTimeout
;;; Compute the timeout value for the operation
;;; with Lo->Y, Hi->X
	.global ComputeTimeout
.proc	ComputeTimeout
	lda SIOTimeout
	ror a
	ror a			; /4 * 256 = *64
	tay
	and #$3f		; -> Hi
	tax
	tya
	ror a
	and #$c0		; remainder->Lo
	tay
	rts
.endproc	
;;; *** Offsets into spare areas in the directory not used
;;; *** for the directory listing that can be used by the FMS
	 .global FileTmpOffset
FileTmpOffset:		; first sector entry, not required for the dir listing
			; do not use up the first entry, reserved for headline.
	.byte $23,$24	
	.byte $33,$34
	.byte $43,$44
	.byte $53,$54
	.byte $63,$64
	.byte $73,$74
