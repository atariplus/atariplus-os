;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: fmsrelocinit.asm,v 1.6 2013/06/02 20:41:04 thor Exp $		**
;;; **									**
;;; ** In this module:	 Relocation of the D: handler behind the cart	**
;;; **			 This is the init method 			**
;;; **********************************************************************

	.include "fms.i"
	.include "cio.i"
	.include "fmsreloc.i"
	.include "kernel.i"
	.include "pia.i"
	.include "reset.i"
	
	.segment  "FmsOverlayInit"

;;; *** Initialization
;;; *** Check whether this is actually an OS ROM
.proc	FmsReloc
	ldx #$20
	lda #<DirName
	sta IOCBAdr,x
	lda #>DirName
	sta IOCBAdr+1,x
	lda #6
	sta IOCBAux1,x
	lda #0
	sta IOCBAux2,x
	lda #CmdOpen
	sta IOCBCmd,x
	jsr CIOVector		; open for directory
	bmi nodos		; fail if this is not a DOS entry
	
	lda IOCBPut+1,x		; this must be in the OsLo area
	cmp #$c0
	bcc nodospp
	cmp #$cc
	bcc founddos
	bcs nodospp
nodos:
	lda #<NoDosFound
	ldy #>NoDosFound
	jsr Print
	jmp exit		;no need to close anything
nodospp:
	lda #<NoDosPP
	ldy #>NoDosPP
	jsr Print
	jmp exit
founddos:	
	;; ok, works.
	lda CIOReplacementEntry	; check whether this is ROM
	eor #$ff
	sta CIOReplacementEntry
	cmp CIOReplacementEntry	; if this is RAM, would be equal
	beq nocart

	lda PIAPortB
	and #$02		; is BASIC enabled?
	bne nobasic		; if not so, check for Oss cart

	lda PIAPortB
	ora #$02		; check whether we can disable it
	sta PIAPortB
	
	lda CIOReplacementEntry	; check whether this is ROM
	eor #$ff
	sta CIOReplacementEntry
	cmp CIOReplacementEntry	; if this is RAM, would be equal
	bne nobasic		; if not equal, this is not RAM

	jsr InstallBasicSwitch	; ok, install the basic switch
	jsr InstallFmsSwitch

	lda PIAPortB
	and #$fd		; re-enable basic
	sta PIAPortB
	jmp exit
nobasic:
	;; check whether this is an Oss cart
	lda $afff		; this identifies the Oss bank. Must be 0,1 or 9
	beq couldbeoss
	cmp #1
	beq couldbeoss
	cmp #9
	bne nooss
couldbeoss:
	sta SwitchOn+1		; keep the vector
	sta $d508		; try to disable the cart

	lda PIAPortB
	ora #$02		; disable basic for sure
	sta PIAPortB
	
	lda CIOReplacementEntry	; check whether this is ROM
	eor #$ff
	sta CIOReplacementEntry
	cmp CIOReplacementEntry	; if this is RAM, would be equal
	bne nooss		; if not equal, is not an Oss RAM

	ldx SwitchOn+1		; try to re-enable
	sta $d500,x

	lda $afff
	cmp SwitchOn+1		; if not equal, not an Oss. Unfortunately, no way to restore...
	bne nooss

	sta $d508		; disable again
	
	jsr InstallOssSwitch	; ok, install the basic switch
	jsr InstallFmsSwitch

	lda #$ff
	sta BasicDisabled	; keep basic off
	
	ldx SwitchOn+1
	sta $d500,x		; re-enable the cart
nooss:
nocart:	
exit:
	ldx #$20
	lda #CmdClose
	sta IOCBCmd,x
	jsr CIOVector		; close this
	rts
.endproc
;;; *** Print a string (a,y) to the standard out
;;; *** Print
.proc	Print
	sta IOCBAdr
	sty IOCBAdr+1
	lda #CmdPutRecord
	sta IOCBCmd
	lda #$ff
	sta IOCBLen
	ldx #$00
	stx IOCBLen+1
	jsr CIOVector
	rts
.endproc
;;; *** The error message
NoDosFound:
	.byte "Dos (FMS) is not installed",$9b
NoDosPP:
	.byte "Running DOS is not Dos 2.++",$9b
DirName:
	.byte "D:*.*",$9b
	
;;; *** InstallBasicSwitch
;;; *** Install the switch vectors for basic
.proc	InstallBasicSwitch
	ldx #$c-1		; size of the switch vectors
loop:
	lda SwitchBasicOff,x
	sta SwitchOff,x
	lda SwitchBasicOn,x
	sta SwitchOn,x
	dex
	bpl loop
	rts
.endproc
;;; *** InstallOssSwitch
;;; *** Install the switch vectors for Oss supercarts
.proc	InstallOssSwitch
	ldx #$c-1
loop:
	lda SwitchOssOff,x
	sta SwitchOff,x
	lda SwitchOssOn,x
	sta SwitchOn,x
	dex
	bpl loop
	rts
.endproc
;;; *** InstallFmsSwitch
;;; *** Install the FMS switch
.proc	InstallFmsSwitch
	ldx #0
loop:
	lda #<(RegularEntry-1)
	sta NewDTab,x
	lda #>(RegularEntry-1)
	sta NewDTab+1,x
	inx
	inx
	cpx #12
	bcc loop
	lda #$20			; JSR
	sta RegularEntry
	sta RegularEntry+3
	sta ResetEntry
	sta POBEntry
	sta POBEntry+3
	sta ResetEntry+3
	sta DUPEntry
	lda #$4c			; JMP
	sta DUPEntry+3
	lda #<SwitchOff
	sta RegularEntry+1
	sta POBEntry+1
	sta ResetEntry+1
	lda #>SwitchOff
	sta RegularEntry+2
	sta POBEntry+2
	sta ResetEntry+2
	lda #<CIOReplacementEntry
	sta RegularEntry+4
	lda #>CIOReplacementEntry
	sta RegularEntry+5
	lda #<POBReplacementEntry
	sta POBEntry+4
	lda #>POBReplacementEntry
	sta POBEntry+5
	lda #<ResetReplacementEntry
	sta ResetEntry+4
	lda #>ResetReplacementEntry
	sta ResetEntry+5
	lda #<ResetEntry
	sta DUPEntry+1
	lda #>ResetEntry
	sta DUPEntry+2
	lda #<LaunchDupVector
	sta DUPEntry+4
	lda #>LaunchDupVector
	sta DUPEntry+5
	lda #$4c		; jmp
	sta RegularEntry+6
	sta POBEntry+6
	sta ResetEntry+6
	lda #<SwitchOn
	sta RegularEntry+7
	sta POBEntry+7
	sta ResetEntry+7
	lda #>SwitchOff
	sta RegularEntry+8
	sta POBEntry+8
	sta ResetEntry+8

	;; install the POB vector
	ldx #$20
	lda IOCBPut,x
	sta POBVector
	lda IOCBPut+1,x
	sta POBVector+1

	ldx #$0
cploop:
	lda FmsResident,x
	sta $a000,x
	inx
	bne cploop
	;; install HaTabs
	ldx #0
findht:
	lda HaTabs,x
	beq notfound		; huh?
	cmp #'D'
	beq found
	inx
	inx
	inx
	bne findht
found:
	lda #<NewDTab
	sta HaTabs+1,x
	lda #>NewDTab
	sta HaTabs+2,x
notfound:
	lda #$10		; do not adjust lomem
	ora FmsBootFlag
	sta FmsBootFlag

	lda BootFlag
	ora #$1			; install FMS boot
	sta BootFlag

	lda #<ResetEntry
	sta DosInit
	lda #>ResetEntry
	sta DosInit+1

	lda #<DUPEntry
	sta DupVector
	lda #>DUPEntry
	sta DupVector+1

	;; relocate the disk buffer
	lda #<(CIOReplacementEntry+$100)
	sta DiskBufferBase
	sta DiskBuffer
	lda #>(CIOReplacementEntry+$100)
	sta DiskBufferBase+1
	sta DiskBuffer+1

	;; clear the buffers
	ldy #0
	tya
	ldx #6
clp:
	sta (DiskBuffer),y
	iny
	bne clp
	inc DiskBuffer+1
	dex
	bne clp

	lda #$f
	sta FmsDriveMask	;go for four drives
	lda #8
	sta FmsBuffers		;go for eight buffers
	rts
.endproc	
;;; *** SwitchBasicOff
;;; *** This method switches the BASIC ROM off
.proc	SwitchBasicOff
	pha
	lda PIAPortB
	ora #$02
	sta PIAPortB
	pla
	rts
	nop
.endproc
;;; *** SwitchBasicOn
;;; *** This method switches the BASIC ROM on
.proc	SwitchBasicOn
	pha
	lda PIAPortB
	and #$fd
	sta PIAPortB
	pla
	rts
	nop
.endproc
;;; *** SwitchOssOff
;;; *** Switches a OSS Rom on
.proc	SwitchOssOff
	pha
	lda $afff
	sta SwitchOn+1
	sta $d508
	pla
	rts
.endproc
;;; *** SwitchOssOn
;;; *** Switch the Oss Rom off
.proc	SwitchOssOn
ActiveBank:
	sta $d500
	rts
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
.endproc

	
