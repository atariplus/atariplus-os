;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: diskio6hi.i,v 1.2 2013-04-12 18:47:05 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   High-mem part of DiskIO			**
;;; **********************************************************************
	
	.global HiLen
	.global	ReInstallOffset
	.global DiskIOGetOffset
	.global QuoteTypeOffset
	.global EnableReturnOffset
	.global CallCIOOffset
	.global NewGetVectorOffset
	.global	MemLoOffset
	.global HaTabsVectorOffset
	.global	DirBufferVectorOffset
	.global OldInitOffset
	
	;; Zero-page offsets
ZPage			=	$e0 ;was cb
CmdPtr			=	$e0 ;pointer to commands
ZPageLen		=	2 	; two bytes

	;; other offsets
CmdBuffer		=	$0580 ; where the editor input is expected
