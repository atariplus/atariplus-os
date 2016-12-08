;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: cio.i,v 1.4 2015/08/15 14:47:42 thor Exp $			**
;;; **									**
;;; ** In this module:	 Central IO functions				**
;;; **********************************************************************

ReducedCmd		=		$17		;contains the reduced command offset
HaTabs			=		$31a		;Os handler table

IOCBIndex		=		$340		;index into HATABS if open,$ff if closed
IOCBUnit		=		$341		;unit number
IOCBCmd			=		$342		;IOCB command
IOCBStatus		=		$343		;last status code
IOCBAdr			=		$344		;buffer address
IOCBPut			=		$346		;the put-one-byte vector
IOCBLen			=		$348		;length of the buffer
IOCBAux1		=		$34a		;additional data #1
IOCBAux2		=		$34b		;additional data #2
IOCBAux3		=		$34c		;additional data #3
IOCBAux4		=		$34d		;additional data #4
IOCBAux5		=		$34e		;additional data #5
IOCBAux6		=		$34f		;additional data #6
;
; The same for the Z-IOCB
ZIndex			=		$20		;index into HATABS if open,$ff if closed
ZUnit			=		$21		;unit number
ZCmd			=		$22		;Z command
ZStatus			=		$23		;last status code
ZAdr			=		$24		;buffer address
ZPut			=		$26		;the put-one-byte vector
ZLen			=		$28		;length of the buffer
ZAux1			=		$2a		;additional data #1
ZAux2			=		$2b		;additional data #2
ZHandlerVec		=		$2c		;NOT data #3, but handler vector
ZIOCB			=		$2e		;NOT data #5, but the IOCB #
ZIOByte			=		$2f		;NOT data #6, but byte to transmit
;
; Command definitions
CmdOpen			=		3		;open a channel
CmdGetRecord		=		5		;get a newline terminated string
CmdGetBlock		=		7		;get a full block
CmdPutRecord		=		9		;put a newline terminated string
CmdPutBlock		=		11		;put a full block
CmdClose		=		12		;close a channel
CmdStatus		=		13		;query the status information
CmdSpecial		=		14		;from here up: handler specific commands
;
; Structure of a handler table
HandlerOpen		=		0
HandlerClose		=		2
HandlerGet		=		4
HandlerPut		=		6
HandlerStatus		=		8
HandlerSpecial		=		10
HandlerInit		=		12

	;; exports
	.global CIOInit
	.global CIO
