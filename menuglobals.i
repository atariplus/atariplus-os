;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menuglobals.i,v 1.2 2013-04-07 18:58:53 thor Exp $		**
;;; **									**
;;; ** In this module:	 Menu driven DUP for Dos 2.++			**
;;; ** Derived from version 1.12 (19-Jan-1990)				**
;;; **********************************************************************

;;; This here stores global variables and buffers
;;; of the menu that are not loaded from disk

	.global	MenuStart
	.global DList1
	.global MenuScreenMemory
	.global DList2
	.global InputBuffer
	;; three generic buffers
	.global Buf1
	.global Buf2
	.global Buf3
	;; the information line containing the last error etc..
	.global HeadLine
	;; Entry points for external helpers, will be filled in
	.global MenuEntryPoints
;;; *****
;;; System parameters and entry points
	.global JUMPTAB
	.global DupEnd
	.global STAX
	.global InExtended
	.global ResetFlags
	.global AllowSwitch
	.global DisplaySwitch
	.global LastMenuPoint
	.global DefaultDeviceLen
	.global DefaultDevice


