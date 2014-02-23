;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: fmsreloc.asm,v 1.7 2013/06/02 20:41:04 thor Exp $		**
;;; **									**
;;; ** In this module:	 Relocation of the D: handler behind the cart	**
;;; **********************************************************************

	.include "fms.i"
	.include "cio.i"
	.include "fmsreloc.i"
	.include "errors.i"
	.include "pia.i"
	.include "kernel.i"
	
	.segment  "FmsOverlay"

;;; This is the start
	jmp CIOReplacement	; called if CIO is to be used
	jmp CIOPutOneByte	; called if the POB is on.
	jmp ResetRestore	; called on Reset - replaces the D: hatabs entry
;;; *** The CIO replacement
;;; *** Called here when entering the FMS via CIO
.proc	CIOReplacement
	sta ZIOByte
	ldy ReducedCmd		; get offset in the handler table
	bne immediate
	jsr immediate		; first call open
	lda #<(POBEntry-1)
	sta ZPut
	lda #>(POBEntry-1)
	sta ZPut+1
	rts
immediate:
	;; bload is tricky since it calls CIO recursively.
	;; re-enable the cart as the disk buffer is not required for this
	;; the second level CIO will switch it off again.
	lda FmsTable+1,y	; keep lo
	pha
	lda FmsTable,y
	pha
	ldy #UnsupportedCmd
	lda ZCmd
	cmp #CmdBload
	bne nobload
	jmp SwitchOn
nobload:	
	lda ZIOByte
	rts
.endproc
;;; *** The PutOneByte Vector
;;; *** Called when a user calls through the POB vector
.proc	CIOPutOneByte
	sta ZIOByte
	lda POBVector+1
	pha
	lda POBVector
	pha
	lda ZIOByte
	rts
.endproc
;;; *** ResetRestore:
;;; *** Installs the new Hatabs entry for the disk vector
.proc	ResetRestore
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

	lda DiskBufferBase
	sta DiskBuffer
	lda DiskBufferBase+1
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
notfound:	
	rts
.endproc

