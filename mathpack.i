;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: mathpack.i,v 1.2 2008-11-24 21:22:55 thor Exp $			**
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
ztemp3		=	$f9		; two bytes
degflag		=	$fb		; used by basic: = 0: RADiants, = 6 : Degrees


	.global AsciiToBCD
	.global BCDToAscii
	.global IntToBCD
	.global BCDToInt
	.global ZeroFr0
	.global ZeroFRX			;zero FR register at X
	.global ZeroRgs
	.global LoadOutbuff		;set inbuff to point to OutBuff
	.global	TimesTwo
	.global BCDSub
	.global BCDAdd
	.global BCDMul
	.global BCDDiv
	.global SkipBlanks
	.global TestDigit
	.global Normalize
	.global EvalPoly
	.global LoadFr0IndXY
	.global LoadFr0IndPtr
	.global LoadFr1IndXY
	.global LoadFr1IndPtr
	.global StoreFr0IndXY
	.global StoreFr0IndPtr
	.global Fr0ToFr1
	.global BCDExp
	.global BCDPow10
	.global BCDFract
	.global BCDLog
	.global BCDLog10
	.global	OneHalf
	.global AtnPoly
	.global	NearOne
	.global PiOver4
