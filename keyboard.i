;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: keyboard.i,v 1.3 2003/03/30 12:56:32 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the K: handler		**
;;; **********************************************************************

;; Definitions for the keyboard handler
FuncKeyDef		=	$60	; function key definition table (1200XL only)
KeyDef			=	$79	; keyboard definition table
KeyCodeHold		=	$7c	; keeps the keyboard code after reading
InverseMask		=	$2b6	; if set, then characters are presented in inverse
ShiftLock		=	$2be	; keyboard modifier state
NoClick			=	$2db	; set if keyboard click is disabled
SuperFlag		=	$3e8	; keyboard extended functionality flag

;; special entries in the keyboard table
KeyClear		=	$7d	; clear the screen
KeyBS			=	$7e	; the backspace key
KeyTab			=	$7f	; the TAB key
KeyNop			=	$80	; key is not available
KeyInverse		=	$81	; Atari key = inverse video key
KeyCaps			=	$82	; the Caps key
KeyHiCaps		=	$83	; shift+caps
KeyCtrlCaps		=	$84	; ctrl+caps
KeyEOF			=	$85	; ^3 = signal an EOF
KeyToggleClick		=	$89	; toggle keyboard click on/off (unassigned)
KeyF1			=	$8a	; function key F1
KeyF2			=	$8b	; function key F2
KeyF3			=	$8c	; function key F3
KeyF4			=	$8d	; function key F4 (all unassigned and only available on 1200XL)
KeyCursorHome		=	$8e	; move cursor left/top (unassigned)
KeyCursorEnd		=	$8f	; move cursor left/bottom (unassigned)
KeyCursorLeft		=	$90	; move cursor to the left (unassigned)
KeyCursorRight		=	$91	; move cursor to the right (unassigned)
LastSpecialKey		=	$92	; this is not used in any way and the "end" marker
KeyEOL			=	$9b	; EOL = RETURN
KeyDelLine		=	$9c	; Delete function = Shift BS
KeyInsertLine		=	$9d	; insert a line
KeyCtrlTab		=	$9e	; Ctrl TAB
KeySetTab		=	$9f	; set a TAB stop  = Shift TAB

;; globals of the keyboard
	.global	KeyboardOpen
	.global	KeyboardClose
	.global	KeyboardGet
	.global	KeyboardPut
	.global	KeyboardStatus
	.global	KeyboardSpecial
	.global KeyboardInit
	.global	KeyboardClick
