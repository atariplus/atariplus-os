;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: kernel.i,v 1.18 2015/08/15 14:47:43 thor Exp $		**
;;; **									**
;;; ** In this module:	 Kernel definitions: The jump vectors of the Os	**
;;; **********************************************************************

;; init addresses of the system handlers
EditorTable		=	$e400
ScreenTable		=	$e410
KeyboardTable		=	$e420
PrinterTable		=	$e430
TapeTable		=	$e440
;; kernel jump-ins 
DiskInitVector		=	$e450
DiskInterfVector	=	$e453
CIOVector		=	$e456
SIOVector		=	$e459
SetIRQVector		=	$e45c
ImmediateVBIVector	=	$e45f
ExitVBIVector		=	$e462
SIOInitVector		=	$e465
NMIInitVector		=	$e46b
CIOInitVector		=	$e46e
ByeVector		=	$e471	;BYE jumps to here
WarmStartVector		=	$e474
ColdStartVector		=	$e477
MountHandlerVector	=	$e486	;install a new handler
Init850Vector		=	$e48f	;850 init 
LaunchDosVector		=	$e492	;DUP entry point
FmsInitVector           =       $e498   ;FMS init
LaunchDupVector		=	$e49b

RTSVector		=	$e4c0   ;a lonely RTS, used by several functions
	
RomSumLoVector		=	$56b0	;in the selftest
RomSumHiVector		=	$56b3
VectorInitVector	=	$56b6
