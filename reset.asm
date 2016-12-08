;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: reset.asm,v 1.40 2015/09/14 16:12:41 thor Exp $		**
;;; **									**
;;; ** In this module:	 Startup and Reset handling			**
;;; **********************************************************************

	.include "reset.i"
	.include "pia.i"
	.include "pokey.i"
	.include "antic.i"
	.include "gtia.i"
	.include "diskinterf.i"
	.include "irq.i"
	.include "rts.i"
	.include "nmi.i"
	.include "cio.i"
	.include "fms.i"
	.include "sio.i"
	.include "kernel.i"
	.include "misc.i"
	
		
	.segment  "OsHi"

;;; *** CPUReset
;;; *** The 6502 reset vector jumps in here
	.global CPUReset
.proc	CPUReset
	sei			; no interrupting
	ldx #$8c		; delay constant
ylp:
	dey
	bne ylp
	dex
	bne ylp			; wait until power is constant
	;; check whether the init magic
	;; cookies are setup correctly
	lda InitMagic0
	cmp #$5c		; still correct?
	bne ResetCold
	lda InitMagic1
	cmp #$93		; still correct?
	bne ResetCold
	lda InitMagic2
	cmp #$25		; still correct?
	bne ResetCold
.endproc
	;; otherwise, warmstart
;;; *** ResetWarm
;;; *** the warmstart vector
	.global ResetWarm
.proc	ResetWarm
	;; check whether a cart has been inserted
	sei			; no interrupting now
	cld			; to be on the safe side
	lda #0
	sta NMIEnable		; really no interrupting now
	lda Trigger3
	cmp Trigger3Shadow	; did this change?
	bne ResetCold		; better do a coldstart
	ror a			; cart inserted?
	bcc nocart
	lda CartTest		; must be zero to identify a cart
	bne nocart		; not run anyhow, do not test the checksum:	is ignored
	jsr CompareCartSum	; check whether the cart ROM sum is fine
	bne ResetCold		; if it doesn't fit, better do the coldstart
nocart:
	lda ColdStartFlag	; enforced a coldstart?
	bne ResetCold
	lda #$ff		; we are coldstarting
	.byte $2c		; skip the next two bytes
.endproc	
;;; *** ResetCold
;;; *** The os controlled coldstart vector
	.global ResetCold
.proc	ResetCold
	lda #$00
.endproc
;;; *** Init
;;; *** Here's the start of the system init
;;; *** expects the coldstart flag in the A register
.proc	Init
	sta WarmStartFlag	; keep it
	cld			; binary mode
	ldx #$ff
	txs			; initialize the stack
	jsr RunBootCart		; run diagnostic cartridge
	jsr CustomChipInit	; initialize custom chips
	jsr BasicInit		; switch on basic RAM
	jsr FindRAMTop
	;; now perform the real ram test
	;; actually, this test is idiotic.
	;; A faulty RAM will loose its contents
	;; or will flip bits randomly, but will most likely
	;; hold its contents for one cycle on the same
	;; address....
	lda WarmStartFlag	; warmstart?
	bne skiptest		; if so, do not check again (do not risk worthful data)
	sta RamTest		; reset the ram test pointer
	sta RamTest+1		; ditto
	ldy #WarmStartFlag	; do not modify above. Everything below must work.
ramtest:			; here:	 coldstart
	lda #$ff
	sta (RamTest),y		; ok, init here
	cmp (RamTest),y		; can we set it to $ff?
	bne ramfailure
	lda #$00		; can we set it to $00
	sta (RamTest),y
	cmp (RamTest),y		; ditto
	bne ramfailure
	iny			; this page done
	bne ramtest
	inc RamTest+1		; to the next page
	lda RamTest+1
	cmp RamChkPtr+1		; until all detected pages have been handled
	bcc ramtest
	lda #$ff
	sta ColdStartFlag	; if we reset now, make this a coldstart
	;; test the ROM checksum here
	jsr MapSelfTest
	jsr RomSumLoVector	; compute the low memory checksum
	bne romfailure
	jsr RomSumHiVector	; compute the high memory checksum
	bne romfailure
	lda #<ByeVector
	sta DosVector
	lda #>ByeVector		; call the bye vector if there is no dos
	sta DosVector+1
	lda #<RtsVector
	sta DosInit
	lda #>RtsVector
	sta DosInit+1
	lda #<LaunchDupVector	; where we get run to launch the DUP
	sta DupVector
	lda #>LaunchDupVector
	sta DupVector+1
	bne init
ramfailure:			
	;; jumped here if the ram test failed
	lda #$34
	sta ColorBack		; color code red
	bne ramfailure
romfailure:
	lda #$46
	sta ColorBack		; color code pink
	bne romfailure
	;;
	;; here handling for the warmstart
skiptest:
	lda #$00
	tax			; initialize the Os vectors
initvecs:
	sta VecDLI,x
	inx
	bne initvecs
	ldx #<ScreenFailure	; and clear here as well, but not including this
initdcb:
	sta SIODeviceId-1,x	; get it
	dex
	bne initdcb		; ditto here.
	ldx #IRQStatShadow	; also reset the Os part of the zero page from here up
initzero:
	sta ZeroBase,x		; reset the zero page
	inx
	bpl initzero
init:
	;; Os init starts here.
	;; check whether we've basic enabled.
	;; if so, set the basic flag. We must do
	;; this here because we clear all other
	;; flags and the RAM above.
	jsr MapSelfTest		; again, this is in the selftest area
	jsr VectorInitVector
	jsr HideSelfTest
	;; check wether the fms wants to be run
	lda FmsBootFlag
	bpl nofms
	jsr FmsInitVector
nofms:	
	cli			; start interrupt management

	ldx #$ff
	lda FmsBootFlag		; have we changed the RAM contents?
	lsr a
	bcc nocartcold
	;; signal that we need the cart to re-initialize
	;; this is actually a Mac65 bug workaround which
	;; checks (incorrectly) the coldstart and not
	;; the warmstart flag
	stx ColdStartFlag
nocartcold:	
	;; check boot method
	inx			; zero X register
	stx CartFlag		; remove traces of the RAM test, re-use as cartridge flag
	lda MemTop+1
	cmp #$b0		; could we have a cart inserted?
	bcs nocart
	lda CartTest		; must be zero to identify a cart
	bne nocart
	inc CartFlag		; we do have one
	jsr CompareCartSum	; compute the cart checksum
	jsr CallCartInit	; and initialize the cart
nocart:
	;; now open the editor
	lda #CmdOpen
	ldx #0			; over IOCB #0
	sta IOCBCmd,x
	lda #<ScreenName
	sta IOCBAdr,x
	lda #>ScreenName
	sta IOCBAdr+1,x
	lda #12			; for read/write
	sta IOCBAux1,x
	jsr CIOVector		; run thru CIO
	bpl screenisopen	;
	;; ooops, editor did not open?
	lda AppMemHi+1		; did this probably happen due to out of memory reasons?
	beq totalfailure	; outch!
	stx AppMemHi		; reset the application himem, we need it for the init screen
	stx AppMemHi+1		; ditto
	stx ColdStartFlag
	lda #$01
	jsr InitVectors		; erase program area.
	jmp ResetWarm		; run thru the warmstart again
totalfailure:
	lda #$00
	sta NMIEnable
	sta DMACtrl
donelp:
	lda #$68		; blue
	sta ColorBack
	bne donelp		; loop forever, wait for the user to reset
	;;
	;; here: start the boot procedure
screenisopen:
	lda Clock
wclock:	
	cmp Clock
	beq wclock		; wait for the VBI to appear and to load the custom chips

	jsr BootTape		; boot from tape if required
	lda CartFlag		; do we have a cart?
	beq performinit
	lda CartType		; shall we boot from disk?
	ror a
	bcc noinit
performinit:
	jsr BootDisk		; boot from disk
noinit:
	;;
	ldx #$00
	lda FmsBootFlag		; have we changed the RAM contents?
	lsr a
	bcc nocold
	;; signal that we need the cart to re-initialize
	stx WarmStartFlag
	and #$20		; about to run DUP? If so, the clear cart flag persists
	bne nocold
	dec FmsBootFlag		; reset this bit again
nocold:	
	stx ColdStartFlag	; booting is now over
	jsr DosInitRun		; need to run the resident dup menu?
	lda CartFlag		; cart available?
	beq runddos		; if not, run the dup vector
	lda CartType		; shall the cart be run?
	and #$04
	beq runddos
	jmp (CartRun)		; run the cart
runddos:
	jmp (DosVector)
CallCartInit:
	jmp (CartInit)		; start thru the init vector
	;; Init table for HATABS
	.global HaTabsInitTable
HaTabsInitTable:
	.byte 'P'		; printer
	.word PrinterTable
	.byte 'C'		; tape
	.word TapeTable
	.byte 'E'		; editor
	.word EditorTable
	.byte 'S'		; screen
	.word ScreenTable
	.byte 'K'		; keyboard
	.word KeyboardTable
	.byte 0,0,0		; terminate
	ScreenName:
	.byte "E:",$9b
.endproc
;;; *** BootTape
;;; *** Boot from tape (or not)
.proc	BootTape
	lda WarmStartFlag
	beq exit		; nothing at coldstart
warmstart:	
	lda BootFlag		; check whether we shall call the tape init vector
	and #$02
	beq exit		; no.
	jmp (CasInit)
exit:
	rts
.endproc
;;; *** Boot
;;; *** Boot from disk or tape or whatever...
.proc	BootDisk
	lda WarmStartFlag
	beq coldstart
	lda BootFlag
	and #$01
	beq exit
CallDosInit:			; run the dos init vector
	jmp (DosInit)		; run the init vector
exit:
	rts
coldstart:	
	lda Console
	and #$01
	bne diskboot		; do regular boot unless the user holds start
fmsinit:	
	lda #$80		; launch FMS on reset
	jsr InitVectors
	jsr FmsInitVector	; initialize the fms
initflag:	
	lda #$01
	ora BootFlag
	sta BootFlag
	rts
diskboot:
	lda #$1			; unit one
	sta SIODeviceUnit
	lda #'S'		; request the status
	sta SIOCommand
	jsr DiskInterfVector	; get the status:	disk here?
	bpl boot
	; no disk, no booting. We now try to boot off the 850 handler
nodisk:		
	lda #$81		; launch FMS on reset, do not try to bootstrap since there is no disk
	jsr InitVectors		; init vectors
	jsr FmsInitVector	; init FMS. Won't boot off since there's no drive
	jsr Init850Vector	; init 850 manually now
	bcc initflag		; if fine, ensure that the handler is run by setting the bootflag
	rts
boot:	
	lda DiskStatus+1
	bpl nodisk		; avoid boot errors if no disk is in drive
reboot:		
	lda #'R'		; from now on:	read sectors
	sta SIOCommand
	ldx #1
	stx SIOAux1		; start at sector #1
	dex
	stx SIOAux2
	stx SIOBufferLo		; at begin of a page
	lda #>BootBuffer
	sta SIOBufferHi		; and offset hi
	jsr DiskInterfVector
	bmi fmsinit		; on error of the first sector, initialize the fms
	ldx #3
cpflags:
	lda BootBuffer,x
	sta DiskBootFlag,x	; copy data over
	dex
	bpl cpflags
	lda DiskBootAddress	; get the address
	sta BootPtr
	lda DiskBootAddress+1
	sta BootPtr+1		; ditto
	ora BootPtr		; in case this address is zero, it is a non-bootable fms disk
	beq fmsinit
	lda BootBuffer+4	; get the init address
	sta DosInit
	lda BootBuffer+5
	sta DosInit+1
bootloop:
	ldy #128-1		; number of bytes in a sector
movelp:
	lda BootBuffer,y	; get the boot data
	sta (BootPtr),y		; move over
	dey
	bpl movelp
	clc
	lda BootPtr
	adc #128		; next sector
	sta BootPtr
	bcc nocarry
	inc BootPtr+1
nocarry:
	dec DiskBootSectors	; anything left?
	beq done		; if so, complete
	inc SIOAux1
	jsr DiskInterfVector	; get the next sector
	bpl bootloop
booterror:			; jumped here if a boot error was detected
	jsr PrintBootError	; the error message
	jmp reboot		; try again
done:
	jsr CallInitVector	; run the init vector of the boot
	bcs gofmsinit		; if this signaled a failure, use the internal FMS
	jsr CallDosInit	
	lda #$01
	ora BootFlag
	sta BootFlag
	rts
gofmsinit:
	jmp fmsinit
.endproc
;;; *** InitVectors
;;; *** set the Dos boot flags in A, initialize the DOS vectors
	.global InitVectors
.proc	InitVectors
	ora FmsBootFlag		; indicate that we want to get run from now on for bootstraping
	sta FmsBootFlag
	rts
.endproc
;;; *** DosInitRun
;;; *** Run the DOS - in case a program uses the now obsolete DosInit vector
	.global DosInitRun
.proc	DosInitRun
	bit FmsBootFlag		; run the resident dup menu?
	bvs rundup
	rts
rundup:
	lda #0
	sta ColdStartFlag
	jmp (DupVector)
.endproc
;;; *** LaunchDos
;;; *** This vector is stored in $a,$b and used to
;;; *** initiate the dos
	.global LaunchDos
.proc	LaunchDos
	lda #0
	sta ColdStartFlag
	lda #$40
	jsr InitVectors
	ldx #$ff
cps:
	cpx KeyStat
	bne cps			; wait until the keyboard goes up again
	stx KeyCodeShadow
	jmp WarmStartVector	; run into the warmstart to initialize the system
.endproc
;;; *** CallInitVector:
;;; *** Call the init vector of a boot
.proc	CallInitVector
	lda DiskBootAddress
	clc
	adc #6			; offset is six bytes from start
	sta BootPtr
	lda DiskBootAddress+1
	adc #0
	sta BootPtr+1		; keep as well
	jmp (BootPtr)		; call me
.endproc
;;; *** PrintBootError
;;; *** Print the boot error message over
;;; *** IOCB #0
.proc	PrintBootError
	ldy #<BootMsg
	lda #>BootMsg
	;; runs into the following
.endproc
;;; *** Print
;;; *** Print the EOL terminated message at Y(lo),A(hi)
.proc	Print
	ldx #0
	sta IOCBAdr+1,x		; store low
	tya
	sta IOCBAdr,x		; store high
	lda #CmdPutRecord
	sta IOCBCmd,x		; put characters up to the EOL
	txa
	sta IOCBLen+1,x		; reset hi
	lda #128		; enough size, is truncated to EOL anyhow
	sta IOCBLen,x
	jmp CIOVector
.endproc
;;; *** The boot error message
BootMsg:
	.byte "BOOT ERROR",$9b
;;; *** CompareCartSum
;;; *** compute and compare the checksum
;;; *** VecDLIover the last cart bytes
.proc	CompareCartSum
	lda #$00		; clear the checksum
	tax			; ditto the index
	clc
chksum:
	adc CartSumRegion,x	; add up
	inx
	bne chksum		; 256 bytes here, including the ROM
	cmp CartSum		; check whether it is fine
	sta CartSum		; store the proper sum, do not change the flags
	rts
.endproc
;;; *** RunBootCart
;;; *** Immediate cartridge initalization and chip reset
.proc	RunBootCart
	lda Trigger3		; check whether we have a cart inserted
	ror a
	bcc nocart		; nothing
	lda CartTest		; is this a valid cartridge?
	bne nocart
	lda CartType		; is this a boot cartridge
	bpl nocart
	jmp (CartInit)
nocart:				; not a boot cartridge
	rts
.endproc	
;;; *** BasicInit
;;; *** Turn the basic ROM on or off.
.proc	BasicInit
	lda WarmStartFlag	; do we have a warmstart?
	beq coldstart
	lda BasicDisabled	; is the basic disabled?
	bne leavedisabled
	beq enablebasic		; enable the basic otherwise
coldstart:			; here check for the user preferences
	lda Console		; check for the console
	and #4			; check whether option is pressed
	beq leavedisabled
enablebasic:			; otherwise, enable the basic cart
	lda PIAPortB
	and #$fd
	sta PIAPortB
leavedisabled:
	rts
.endproc	
;;; *** FindRAMTop
;;; *** Find the last populated RAM test from $2800 up
.proc	FindRAMTop
	lda #<$2800
	sta RamChkPtr
	tay			; also reset the Y register
	lda #>$2800
	sta RamChkPtr+1
	ldx #>($d000-$2800)	; at most that much pages
ramlp:
	lda (RamChkPtr),y
	eor #$ff		; invert this
	sta (RamChkPtr),y
	cmp (RamChkPtr),y	; still equal?
	bne ramend		; if not, then there's no ram here
	eor #$ff		; restore back, we don't do damage
	sta (RamChkPtr),y
	cmp (RamChkPtr),y	; still equal?
	bne ramend		; oops, how that? Ok, something's at least icky here
	inc RamChkPtr+1		; next page
	dex			; do not checksum the chips (no idea what would possibly happen!)
	bne ramlp
ramend:
	;; here $6 is the ramtop page (the first non-valid page)
	rts
.endproc
	
;;; *** CustomChipInit
;;; *** initialize the custom chips here
.proc	CustomChipInit
	lda #$00
	sta PIAPortBCtrl	; port B init:	write data direction (input -> outputs go to one)
	tax
clrloop:
	sta GTIABase,x		; reset GTIA registers
	sta AnticBase,x		; reset antic registers
	sta PokeyBase,x		; reset pokey registers
	inx
	bne clrloop
	;; now initialize PIA. This requires special care since
	;; we could pull the rug under our feed
	sta PIAPortA		; reset port A
	sta PIAPortACtrl	; reset port A
	;; further PIA init
	lda #$3c		; address port B directly
	sta PIAPortBCtrl
	lda #$ff		; write all one's
	sta PIAPortB
	lda #$38
	sta PIAPortACtrl	; address the data direction registers
	sta PIAPortBCtrl	; ditto
	stx PIAPortA		; for port A all lines input
	dex
	stx PIAPortB		; for port B all lines output
	lda #$3c		; switch back to data port
	sta PIAPortACtrl
	sta PIAPortBCtrl	; for both ports
	lda PIAPortB		; dummy read to clear interrupts
	lda PIAPortA		; ditto
	;; pokey init
	lda #$22
	sta SkStat		; initialize pokey for serial communication
	lda #$a0
	sta AudCtrl2
	sta AudCtrl3		; initialize audio
	lda #$28
	sta AudioCtrl		; ditto for audio
	stx SerDat		; keep the serial line high
	rts
.endproc
