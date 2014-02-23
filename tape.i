;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: tape.i,v 1.1 2003-03-31 21:11:30 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the C: handler		**
;;; **********************************************************************

TapeBaud		=	$2ee	; baud rate of the cassette driver

;; globals
	.global TapeOpen
	.global	TapeClose
	.global TapeGet
	.global TapePut
	.global TapeStatus
	.global TapeSpecial
	.global TapeInit
