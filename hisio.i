;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: hisio.i,v 1.4 2014/03/12 20:24:26 thor Exp $		**
;;; **									**
;;; ** In this module:	 High Speed Serial IO interface			**
;;; ** This code originates from the high-speed SIO patch by		**
;;; ** Matthias Reichl							**
;;; **********************************************************************

	;; possible flags for the high speed SIO variations
Happy1050	=	$0a	; use Happy1050 protocol
Happy810	=	$41	; use Happy810 Warp Speed protocol
XF551		=	$40	; use XF551 protocol
TurboFlag	=	$80	; US protocol
RegularSpeed	=	$28	; regular speed
	
SIOSpeed	=	$30e	; controlls the current speed of SIO
SpeedTab	=	$3ed	; eight bytes defining the speed of the SIO operation, one per drive
SpeedTmpBuf	=	$2c9	; eight bytes buffer for speed detection
SIOTimer	=	$38	; misused as two-byte timer for timeout, required for faster VBI
CurrentSpeed	=	$3a	; currently active speed

	
	
	.global	SIO
	.global	SIOInit
	.global SerInIRQ
	.global SerOutIRQ
	.global SerXmtIRQ

