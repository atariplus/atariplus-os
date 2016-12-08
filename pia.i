;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: pia.i,v 1.4 2015/08/15 14:47:43 thor Exp $							**
;;; **									**
;;; ** In this module:	 Equates for PIA				**
;;; **********************************************************************


PIABase			=	$d300
PIAPortA		=	$d300
PIAPortB		=	$d301
PIAPortACtrl		=	$d302	; a bit unorthogonal, A0 and A1 are swapped in the Atari
PIAPortBCtrl		=	$d303


