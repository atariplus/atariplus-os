;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: taperesident.i,v 1.6 2013/06/05 19:11:21 thor Exp $		**
;;; **									**
;;; ** In this module:	 Resident part of the disk-based tape handler	**
;;; **********************************************************************

TapeBufferPtr	=	$3d	;next free byte in the tape buffer
GapType		=	$3e	;stores the tape inter-record gap type
TapeEOFFlag	=	$3f	;set if an EOF has been detected
RecordSize	=	$40	;size of a tape record in bytes
TapeMode	=	$289	;tape i/o mode (negative for writing)
TapeRecordLen	=	$28a	;size of the current tape record buffered
BaudRate	=	$2ee	;computed baud rate (word)
BaudCnt		=	$30c	;internal 2xtwo-byte register for baud calculation
TapeBload	=	$310	;flags for the bload/booting mechanism of the tape
SerialInBit	=	$316
	;; three bytes ahead of the tape buffer are also used by the tape handler!
TapeBuffer	=	$400	;start of the tape buffer
