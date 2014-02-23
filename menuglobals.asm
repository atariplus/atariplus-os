;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menuglobals.asm,v 1.3 2013/06/02 20:41:06 thor Exp $		**
;;; **									**
;;; ** In this module:	 Menu driven DUP for Dos 2.++			**
;;; ** Derived from version 1.12 (19-Jan-1990)				**
;;; **********************************************************************

	.include "menuglobals.i"
	
	.segment "menuglobals"

;;; This here stores global variables and buffers
;;; of the menu that are not loaded from disk

	.global	MenuStart
	MenuStart		=	*

	.global DList1
DList1:			.res $40	;first display list
	.global MenuScreenMemory
MenuScreenMemory:	.res $03c0	;screen memory containing the menu
	.global DList2
DList2:			.res $40	;second display list
	.global InputBuffer
InputBuffer:		.res 80		;input buffer for user data
	;; three generic buffers
	.global Buf1
Buf1:			.res 40
	.global Buf2
Buf2:			.res 40
	.global Buf3
Buf3:			.res 40
	;; the information line containing the last error etc..
	.global HeadLine
HeadLine:		.res 40
	;; Entry points for external helpers, will be filled in
	.global MenuEntryPoints
MenuEntryPoints:			.res 27*2
;;; *****
;;; System parameters and entry points
	.global JUMPTAB
JUMPTAB:
	.global DupEnd
DupEnd:			.word	0 ;Last memory address used by DUP
	.global STAX
STAX:			.byte	0 ;keeps the stack pointer
	.global InExtended
InExtended:		.byte	0 ;if negative, currently executing an external command
	.global ResetFlags
ResetFlags:		.byte	0
	.global AllowSwitch
AllowSwitch:		.byte	0 ;switching menu allowed?
	.global DisplaySwitch
DisplaySwitch:		.byte	0 ;dos menu visible or not?
	.global LastMenuPoint
LastMenuPoint:		.byte	0
	.global DefaultDeviceLen
DefaultDeviceLen:	.byte	0
	.global DefaultDevice
DefaultDevice:		.byte 	0,0,0

