;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: errors.i,v 1.17 2013-03-08 22:11:16 thor Exp $		**
;;; **									**
;;; ** In this module:	 Os Error codes					**
;;; **********************************************************************

NearEOF		=	3	; special return code for near EOF
BreakError	=	$80	; break key has been hit
ChannelInUse	=	$81	; tried to re-open an open channel
UnknownDevice	=	$82	; tried to access a device that is not present
OutputOnly	=	$83	; channel is only open for output
InvalidCmd	=	$84	; command type is invalid
ChannelNotOpen	=	$85	; the selected IOCB is not open
InvalidChannel	=	$86	; the IOCB channel index is invalid
InputOnly	=	$87	; channel is only open for input
EndOfFile	=	$88	; located an EOF
RecordTooLong	=	$89	; record did not fit into the buffer
TimeoutError	=	$8a	; device doesn't reply
DeviceError	=	$8b	; unexpected serial result code
FrameError	=	$8c	; serial framing error
OutOfRange	=	$8d	; cursor out of range error
OverrunError	=	$8e	; serial overrun error
ChkSumError	=	$8f	; serial checksum error
DeviceNak	=	$90	; device negative acknowledge
UnsupportedCmd	=	$92	; command not supported by handler
OutOfMemory	=	$93	; not enough memory to open screen

SerialBusy	=	$96	; R: channel busy
NotOpenForCM	=	$97	; R: channel hasn't been opened for concurrent mode
InvalidBuffer	=	$98	; R: concurrent buffer settings invalid
InConcurrent	=	$99	; R: concurrent mode active
NotConcurrent	=	$9a	; R: is not in concurrent mode

IllegalUnit	=	$a0	; the requested unit is not available
TooManyFiles	=	$a1	; too many files are open, run out of buffers
DiskFull	=	$a2	; failed to allocate a free sector
FileLinkBroken	=	$a4	; FMS file linkage broken
FileNameInvalid	=	$a5	; an invalid filename has been used
InvalidPoint	=	$a6	; point was out of range
FileProtected	=	$a7	; file is write protected
DirectoryFull	=	$a9	; no new slots in the directory
FileNotFound	=	$aa	; as said, not present

NoBinaryFile	=	$af	; requested file is not a binary load file
BadLinkage	=	$b0	; detected a link to sector 0
InvalidMode	=	$b1	; invalid/unsupported open mode
NotADosDisk	=	$b2	; is not a Dos 2.x compatible disk
