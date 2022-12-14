;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: sio.i,v 1.9 2014/03/14 16:02:22 thor Exp $			**
;;; **									**
;;; ** In this module:	 Serial IO interface				**
;;; **********************************************************************


SerialStatus		=	$30		; contains the serial status
SerialChkSum		=	$31		; contains the supposed-to-be checksum
SerBufLo		=	$32		; serial buffer
SerBufHi		=	$33		; serial buffer, hi byte
SerBufEndLo		=	$34		; serial buffer past end
SerBufEndHi		=	$35		; serial buffer past end, hi byte
SerialDataDone		=	$38		; set if serial data transfer done
SerialXferDone		=	$39		; set if serial read transfer done, including checksum
SerialSentDone		=	$3a		; set if serial write transfer done, including checksum
SerialChkSumDone	=	$3b		; set if checksum has been send
SerialNoChkSum		=	$3c		; set if the serial transfer does not require a checksum
SerialSound		=	$41		; enables/disables SIO sound ouput
SkStatShadow		=	$232		; shadow register of SkStat (pokey)
SIOCmdFrame		=	$23a		; contains the SIO command frame to be sent (four bytes)
SIOAck			=	$23e		; received device acknowledge goes here
SIOError		=	$23f		; set if serial transmission protocol error (not a timeout)
SIORetry		=	$2bd		; retry counter for serial device access
SIOCmdRetry		=	$29c		; retry counter to setup a command
SIODeviceId		=	$300		; identifier of the serial device to access
SIODeviceUnit		=	$301		; unit number of the device
SIOCommand		=	$302		; serial command
SIOStatus		=	$303		; SIO status/transfer direction
SIOBufferLo		=	$304		; SIO buffer low
SIOBufferHi		=	$305		; SIO buffer hi address
SIOTimeout		=	$306		; serial timeout
SIOSizeLo		=	$308		; serial transfer size, lo
SIOSizeHi		=	$309		; serial transfer size, hi
SIOAux1			=	$30a		; AUX1 helper (sector)
SIOAux2			=	$30b		; AUX2 helper (sector)
SIOTimerFlag		=	$317		; cleared on timeout
SIOStackPtr		=	$318		; keeps stack pointer on SIO to restore it on error
SIOStatusTmp		=	$319		; keeps a copy of SerialStatus

	;; External vectors
	.global SIOInit
	.global SIO
	.global SerInIRQ
	.global SerOutIRQ
	.global SerXmtIRQ

