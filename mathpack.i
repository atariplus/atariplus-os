;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: mathpack.i,v 1.6 2015/08/23 14:05:42 thor Exp $			**
;;; **									**
;;; ** In this module:	 math pack functions				**
;;; **********************************************************************


cix		=	$f2		; offset into the math input buffer
inbuff		=	$f3		; lo,hi of the math pack input buffer
fr0		=	$d4		; six bytes, floating point register #0
fr0ext		=	$da		; extension of fr0 for mul and div
fr1		=	$e0		; six bytes, floating point register #1
fr2		=	$e6		; six bytes, floating point register #2
flptr		=	$fc		; pointer for various purposes
flpt2		=	$fe		; secondary pointer
outbuff		=	$580		; output buffer for ASCII conversion
polybuff	=	$5e0		; another floating point register for poly eval
expbuff		=	$5e6		; another floating point register for exponential

;;; internal registers
frx		=	$ec		; position of exponent in input
eexp		=	$ed		; exponent value
nsign		=	$ee		; sign of the number
esign		=	$ef		; exponent sign
fchflg		=	$f0
digrt		=	$f1		; digits right to floating point
ztemp1		=	$f5		; two bytes
ztemp4		=	$f7		; two bytes

;;; Documented call-ins
AsciiToBCDVector	=	$d800 ; CIX+(INBUFF)->fr0
BCDToAsciiVector	=	$d8e6 ; fr0->outbuff
IntToBCDVector		=	$d9aa ; convert fr0,fr0+1->fr0
BCDToIntVector		=	$d9d2 ; round fr0->fr0,fr0+1 or set C
ZeroFr0Vector		=	$da44 ; 0->fr0
ZeroFRXVector		=	$da46 ; 0->fr0 indexed by X
ZeroRgsVector		=	$da48 ; clear Y bytes at X
LoadOutbuffVector	=	$da51 ; set inbuff to outbuff
TimesTwoVector		=	$da5a ; ztemp4*2 -> ztemp4
BCDSubVector		=	$da60 ; fr0-fr1	-> fr0
BCDAddVector		=	$da66 ; fr0+fr1	-> fr0
BCDMulVector		=	$dadb ; fr0*fr1	-> fr0
BCDDivVector		=	$db28 ; fr0/fr1	-> fr0	or set C
SkipBlanksVector	=	$dba1 ; skip blanks pointed to by CIX+(INBUFF)
TestDigitVector		=	$dbaf ; test whether a digit in CIX+(INBUFF) is valid, return in A, otherwise C=1
Fr0TimesTenVector	=	$dbeb ; multiply fr0 with ten
NormalizeVector		=	$dc00 ; normalize the bcd number in fr0
EvalPolyVector		=	$dd40 ; evaluate the polynomial at (x,y), #a with the argument in fr0
LoadFr0IndXYVector	=	$dd89 ; load fr0 from (x,y)
LoadFr0IndPtrVector	=	$dd8d ; load fr0 from (flptr)
LoadFr1IndXYVector	=	$dd98 ; load fr1 from (x,y)
LoadFr1IndPtrVector	=	$dd9c ; load fr1 from (flptr)
StoreFr0IndXYVector	=	$dda7 ; store fr0 in (x,y)
StoreFr0IndPtrVector	=	$ddab ;	store fr0 in (flptr)
Fr0ToFr1Vector		=	$ddb6 ; fr0->fr1
BCDExpVector		=	$ddc0 ; exp(fr0)->fr0
BCDPow10Vector		=	$ddcc ; 10^fr0 ->fr0
BCDFractVector		=	$de95 ; (fr0-(x,y))/(fr0+(x,y)) -> fr0
BCDLogVector		=	$decd ;	log(fr0)->fr0
BCDLog10Vector		=	$ded1 ; log10(fr0)->fr0
OneHalfVector		=	$df6c ; the constant 0.5
AtnPolyVector		=	$dfae ; the polynomial for arctan
NearOneVector		=	$dfea ; the constant 0.9999999999
PiOver4Vector		=	$dff0 ; the constant Pi/4
