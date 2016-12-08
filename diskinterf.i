;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: diskinterf.i,v 1.3 2015/08/15 14:47:42 thor Exp $		**
;;; **									**
;;; ** In this module:	 Resident disk handler				**
;;; **********************************************************************

DiskBufferLo	=	$15		;; temporary disk buffer pointer
DiskBufferHi	=	$16		;; its high word
DiskTimeOut	=	$246		;; resident disk handler format timeout in seconds
DiskSectorSzLo	=	$2d5		;; disk default sector size
DiskSectorSzHi	=	$2d6		;; disk default sector size hi
DiskStatus	=	$2ea		;; disk status registers

	;; global references
	.global DiskInit
	.global DiskInterf
