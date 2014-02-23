;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: fms.i,v 1.27 2013-04-07 17:18:23 thor Exp $			**
;;; **									**
;;; ** In this module:	 Implementation of the D: handler		**
;;; **********************************************************************

	;; internal variables used by the FMS


FmsTmp			=	$15		;another temporary
DirEntryOffset		=	$16		;used for the directory scanner
ComponentStart		=	$18		;for the file name scanner: Offset to the ":" in the name
FmsStack		=	$19		;stack pointer
FileCounter		=	$1a		;stores the file number of non-unique file names, or -1
FreeDirCode		=	$1b		;next available directory code for a free file
FmsPtr			=	$43		;temporary pointer for miscellaneous use
DiskBuffer		=	$45		;disk buffer currently used
FileBuffer		=	$47		;file buffer currently used
RunVector		=	$2e0		;binary load run address stored here
InitVector		=	$2e2		;binary load init vector stored here

FmsBuffers		=	$709		;number of file buffers we shall allocate
FmsDriveMask		=	$70a		;number of drives we shall supply buffers for
DiskBufferBase		=	$70c		;keeps address of the disk buffers
FileBufferBase		=	$712		;keeps address of the file buffers
;; FileNameBuffer-1 will be overwritten!
FileNameBuffer		=	$715		;buffers the currently used filename, 12 characters.
BloadFlags		=	$729		;for binary load: start/run flags
BloadIOCB		=	$72a		;the IOCB used for BLOAD
StartAddress		=	$72b		;keeps the load address of the file for bload
EndAddress		=	$72d		;keeps the end address
AvailFlags		=	$730		;disk unit available flags. non-zero if available
FileBufferFlags		=	$738		;file buffer available flags. non-zero if allocated
WriteCommand		=	$779		;the command the disk device uses for writing, 'P' or 'W'

FCBBase			=	$780		;floppy control block start (16 bytes each)
FileCode		=	FCBBase+0	;directory entry times *4 as the file code in byte 125 (invalid if odd)
FileAux1		=	FCBBase+1	;copy of Aux1 because Aux1 is overwritten by XIO
SpecialFlag		=	FCBBase+3	;identifies direct ("boot") access if negative
AccessFlags		=	FCBBase+4	;various flags: Bit 6: buffer dirty, Bit 7: create new file links
ByteMax			=	FCBBase+5	;bytes in the currently loaded sector
BytePosition		=	FCBBase+6	;byte pointer within the current sector
OpenBuffer		=	FCBBase+7	;non-negative if open. Then the allocated buffer index
FileSector		=	FCBBase+8	;current sector to access
NextSector		=	FCBBase+10	;next sector to access
FileLength		=	FCBBase+14	;counts the file size

FmsEnd			=	$800		;user buffers start here

RunDup			=	$5000		;is within the selftest

;; additional XIOs provided by the FMS

CmdRename		=	32		; rename a filespec
CmdDelete		=	33		; delete a filespec
CmdValidate		=	34		; validate a filename
CmdLock			=	35		; write protect a file
CmdUnlock		=	36		; remove write protection
CmdPoint		=	37		; set file pointer
CmdNote			=	38		; read file pointer
CmdInit			=	39		; initialize/blank a formatted disk
CmdResolve		=	40		; resolve a wildcard
CmdBload		=	41		; binary load file
CmdFormatExtended	=	42		; format with extended parameters
CmdFormatStandard	=	43		; standard format similar to below
CmdFormat		=	254		; single density format for backwards compatibility

	;; globals

	.global FmsInit				; the usual handler vectors
	.global FmsOpen
	.global FmsClose
	.global FmsGet
	.global FmsPut
	.global FmsStatus
	.global FmsSpecial

