;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: dup.i,v 1.8 2006/05/06 19:28:42 thor Exp $			**
;;; **									**
;;; ** In this module:	 Minimal DUP command line parser		**
;;; **********************************************************************

;;; Definitions for the DUP command line parser

DupBuffer		=		$580		;; dup command line parser enters data here
DupTargetBuffer		=		$5c0		;; secondary buffer

DupError		=		$742		;; also used to keep an error indicator
DupRunHeader		=		$742		;; binary address header for the run/init spec
DupRunAdr		=		$746		;; contains a binary save run address if any
DupInitAdr		=		$748		;; contains a binary save init address if any
DupStartAdr		=		$74a		;; start address for binary save
DupEndAdr		=		$74c		;; end address for binary save
DupTmp			=		$74f		;; temporary

DupSourcePosition	=		$743		;; NOTE position on source
DupTargetPosition	=		$746		;; NOTE position on target
HasTarget		=		$749		;; is set if there is target filespec for COPY
SourceCount		=		$74a		;; counts the source files
BlockSize		=		$74b		;; the amount of data read

;; Dup error codes

InvalidNumber		=		2		;; an invalid numeric has been found
MissingArgument		=		3		;; a required argument is missing
ExtraArgument		=		4		;; found an additional argument that doesn't belong there
Range			=		5		;; numeric out of range
