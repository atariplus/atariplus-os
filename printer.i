;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: printer.i,v 1.1 2003/03/31 20:57:03 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the P: handler		**
;;; **********************************************************************

NextPrinterIdx			=	$2de	;next character index to print within the printer buffer
PrinterBufSize			=	$2df	;size of the printer buffer in characters
PrinterTimeout			=	$314	;timeout for the printer in secounds
PrinterBuffer			=	$3c0	;printer output buffer


;; Globals
	.global	PrinterOpen
	.global PrinterClose
	.global	PrinterGet
	.global PrinterPut
	.global PrinterStatus
	.global PrinterSpecial
	.global PrinterInit
