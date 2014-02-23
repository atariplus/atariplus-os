;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: tape.asm,v 1.5 2014/01/19 12:19:57 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the C: handler		**
;;; **********************************************************************

	.include "tape.i"
	.include "errors.i"
	
;;; we do not implement the C: handler
;;; this is all too obsolete, and I've nothing
;;; to test this with.

	.segment  "OsHi"
	
;;; *** Open the C: handler (or not)
	.global TapeOpen
.proc	TapeOpen
	ldy #UnknownDevice
	rts
.endproc;
;; *** Tape status handler
	.global TapeStatus
.proc	TapeStatus
	;; runs into the following
.endproc
;;; *** Close the C: handler
	.global TapeClose
.proc	TapeClose
	ldy #$01		; ok, accept this
	;; runs into the following rts
	;; rts
.endproc
;;; *** Read from the C: handler (or not)
	.global TapeGet
.proc	TapeGet
	;; runs into the following
.endproc
;;; *** Write to the C:	handler (or not)
	.global TapePut
.proc	TapePut
	;; runs into the following
.endproc
;;; *** Tape special handler
	.global TapeSpecial
.proc	TapeSpecial
	;; ldy #UnsupportedCmd ; unsupportedCmd is the default CIO handler result
	rts
.endproc
;;; *** Tape init vector. Does nothing
	.global TapeInit
.proc	TapeInit
	lda #<1484
	sta TapeBaud
	lda #>1484
	sta TapeBaud+1		; initialize the baud rate
	rts
.endproc

