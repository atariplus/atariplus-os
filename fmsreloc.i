;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: fmsreloc.i,v 1.4 2013-04-28 16:29:14 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the D: handler		**
;;; **********************************************************************

	;; internal variables used by the FMS Relocation
	
ResetEntry	=	$700
NewDTab		=	$740			; where the D: handler is relocated to
RegularEntry	=	NewDTab+12		; regular CIO commands go here
POBEntry	=	RegularEntry+9		; where the PutOneByte entry goes
SwitchOff	=	POBEntry+9		; switching the cart off
SwitchOn	=	SwitchOff+12		; switch the cart on
POBVector	=	SwitchOn+12		; where the original POB is stored
DUPEntry	=	$77a			; the DUP goes here
CIOReplacementEntry	=	$a000		; enters here when doing CIO work
POBReplacementEntry	=	$a003		; enters here for the put-one-byte entry
ResetReplacementEntry	=	$a006		; called on reset
FmsResident	=		$2200		; where the resident part starts
FmsTable	=		$c002		; where the CIO table of the FMS is
	
	
