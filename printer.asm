;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: printer.asm,v 1.4 2003/04/26 20:33:49 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the P: handler		**
;;; **********************************************************************

	.include "printer.i"
	.include "sio.i"
	.include "cio.i"
	.include "diskinterf.i"
	.include "kernel.i"
	        
        .segment  "OsHi"
	
;;; *** PrinterInit
;;; *** Initialize the printer interface
	.global PrinterInit
.proc	PrinterInit
	lda #30			; default timeout:	 30 seconds
	sta PrinterTimeout
	rts
.endproc	
;;; *** PrinterOpen
;;; *** This is the open vector of the printer device
	.global PrinterOpen
.proc	PrinterOpen	
	lda #0			; reset the printer buffer
	sta NextPrinterIdx
	jsr PrepPrinterSize	; setup the size of the printer buffer in bytes
	;; Note that this specifically disallows the usage of several printers
	;; in parallel. However, this wasn't possible before anyhow since there
	;; is only one printer buffer.
	;; runs into the following
.endproc
;;; *** PrinterStatus
;;; *** Run the printer status command
	.global PrinterStatus
.proc	PrinterStatus
	lda #'S'		; status command
	sta SIOCommand
	sta SIOAux1		; for compatibility
	lda #$40		; this is a read command
	sta SIOStatus
	lda #4			; four bytes	
	ldx #<DiskStatus	; read the status into the diskinterf buffer
	ldy #>DiskStatus	; ditto
	jsr SIOPrinterDirect
	bmi error
	lda DiskStatus+2	; get the printer timeout
	sta PrinterTimeout
error:
	rts
.endproc
;;; *** PrepPrinterSize
;;; *** get the size of the printer buffer
;;; *** from the CIO AUX2 open parameter
.proc	PrepPrinterSize
	ldy #'W'		; command:	Write
	ldx #40			; width for normal print
	lda ZAux2		; get the open mode, secondary
	cmp #'N'		; normal?
	beq setsize
	ldx #20			; width for double width printing
	cmp #'D'		; double size
	beq setsize
	ldx #29			; width for sideways printing
	cmp #'S'
	beq setsize
	ldx #40			; default is normal print
	lda #'N'		; signal also the printer
setsize:
	sty SIOCommand
	stx PrinterBufSize	; keep the buffer size
	sta SIOAux1		; insert printing command here
	rts
.endproc
;;; *** PrinterClose
;;; *** This is the printer close vector
	.global PrinterClose
.proc	PrinterClose
	jsr PrepPrinterSize	; get the number of bytes in the buffer
	lda #$9b		; load an EOL
	ldy #$01		; default result code
	ldx NextPrinterIdx	; is there still a character in the buffer
	bne FeedCharacter
empty:
	;; runs into the following
.endproc
;;; *** PrinterGet/Special
;;; *** Read a character from the printer, or not...
;;; *** this just fails with the CIO default result code
	.global PrinterGet
.proc	PrinterGet
.endproc
.proc	PrinterExit
.endproc
	.global	PrinterSpecial
.proc	PrinterSpecial
	rts
.endproc
;;; *** PrinterPut
;;; *** Print a character to the printer
	.global PrinterPut
.proc	PrinterPut	
	ldy IOCBUnit,x		; get the IOCB in case basic forgot to put it here (by the PutOneByte vector)
	sty ZUnit
	;; runs into the following
.endproc
;;; *** FeedCharacter
;;; *** write a character into the
;;; *** printer buffer, possibly empty it.
.proc	FeedCharacter
	ldx NextPrinterIdx
	sta PrinterBuffer,x	; insert it into the buffer
	inx			; next character
	ldy #$01		; default result code is fine
	cpx PrinterBufSize	; check whether the buffer is full
	bcs emptybuffer		; if so, clean it
	stx NextPrinterIdx	; keep the new printer size
	cmp #$9b		; do we print an EOL here?
	bne PrinterExit		; if not, bail out and accept
	lda #' '		; fill the remaining buffer with blanks
fillup:
	sta PrinterBuffer,x
	inx
	cpx PrinterBufSize	; up to its end
	bcc fillup
emptybuffer:			; here:	buffer is full, transmit
	lda #$00
	sta NextPrinterIdx	; cleanup the buffer
	
	lda #$80		; default direction is writing	
	sta SIOStatus		; define the data direction
	jsr PrepPrinterSize	; setup the printer command
	txa			; buffer size->A
	ldx #<PrinterBuffer	; this data
	ldy #>PrinterBuffer	; ditto
	;; runs into the following to write out the buffer
.endproc
;;; *** another jump in that does not set the status command
;;; *** With x = buffer address lo,
;;; *** with y = buffer address hi
.proc	SIOPrinterDirect
	sta SIOSizeLo		; set the size
	stx SIOBufferLo
	sty SIOBufferHi
	lda #$40
	sta SIODeviceId		; the printer device
	lda ZUnit		; get the unit
	sta SIODeviceUnit	; insert the unit
	lda #$00
	sta SIOSizeHi
	lda PrinterTimeout	; insert the timeout
	sta SIOTimeout
	jsr SIOVector		; now run SIO
	rts
.endproc
