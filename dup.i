;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: dup.i,v 1.11 2015/08/15 14:47:42 thor Exp $			**
;;; **									**
;;; ** In this module:	 Minimal DUP command line parser		**
;;; **********************************************************************

;;; Definitions for the DUP command line parser

DupBuffer		=		$580		;; dup command line parser enters data here
DupTargetBuffer		=		$5c0		;; secondary buffer

DupError		=		$542		;; also used to keep an error indicator
DupRunHeader		=		$542		;; binary address header for the run/init spec
DupRunAdr		=		$546		;; contains a binary save run address if any
DupInitAdr		=		$548		;; contains a binary save init address if any
DupStartAdr		=		$54a		;; start address for binary save
DupEndAdr		=		$54c		;; end address for binary save
DupTmp			=		$54f		;; temporary

DupSourcePosition	=		$543		;; NOTE position on source
DupTargetPosition	=		$546		;; NOTE position on target
HasTarget		=		$549		;; is set if there is target filespec for COPY
SourceCount		=		$54a		;; counts the source files
BlockSize		=		$54b		;; the amount of data read
SameDevice		=		$54e		;; if ne, target and source are different
;; Dup error codes

InvalidNumber		=		2		;; an invalid numeric has been found
MissingArgument		=		3		;; a required argument is missing
ExtraArgument		=		4		;; found an additional argument that doesn't belong there
Range			=		5		;; numeric out of range
