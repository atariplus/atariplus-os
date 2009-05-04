;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: editor.i,v 1.14 2008-09-23 19:35:24 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the E: handler		**
;;; **********************************************************************

;; Definitions for the screen handler

ScreenError		=	$4c	; status register for the screen and keyboard
LeftMargin		=	$52	; free columns at the left
RightMargin		=	$53	; right editor border
ChrUnderCursor		=	$5d	; keeps the character under the cursor
CursorPtr		=	$5e	; keeps the cursor address
LogicalColumn		=	$63	; position within the logical column
BufferCnt		=	$6b	; number of characters in the editor buffer
StartLogicalRow		=	$6c	; start Y position of the current logical row (above or equal to the cursor row)
StartLogicalColumn	=	$6d	; start X position of the current logical row 
					; (these two give the position where E: read will return data from)
SwapFlag		=	$7b	; flag that indicates whether text window data has been swapped in
InsDat			=	$7d	; temporary flag/character
FineScrollFlag		=	$26e	; finescroll flag. Enabled if this is < 0
WindowRow		=	$290	; cursor row within the text window
WindowColumn		=	$291	; cursor column within the text window (two bytes)
WindowGfxMode		=	$293	; graphics mode of the text window. Always zero.
WindowOrigin		=	$294	; origin of the text window in memory
TabStops		=	$2a3	; this array keeps the position of the TAB stops
TabStopsSize		=	15	; number of bytes in the TAB-table
LineStarts		=	$2b2	; this array keeps the display lines containing logical line starts
LineStartsSize		=	4	; number of bytes in this table
OverrunFlag		=	$2b8	; set by the keyboard handlers if the buzzer should get run
ScrollFlag		=	$2bb	; gets set if scrolling is required
WindowHeight		=	$2bf	; number of lines in the text window
EscFlag			=	$2a2	; if 128, then ESC has been pressed before
DirectFlag		=	$2fe	; if set, then control characters are printed uninterpreted
CursorInhibit		=	$2f0	; cursor visibility flag (=0: visible)

;; Global entries
	.global EditorOpen		; CIO E: open vector
	.global EditorClose		; CIO E: close vector
	.global EditorGet		; CIO E: get vector
	.global EditorPut		; CIO E: put vector
	.global EditorStatus		; CIO E: status vector
	.global EditorSpecial		; CIO E: special vector
	.global EditorInit		; CIO E: init vector

	.global CheckCtrlCode		; check whether a given ATASCII code is a ctrl code. Returns eq if so
	.global	EditorScreenInit	; initializes screen specific editor variables
	.global SwapGfxWindowCursor
	.global BitMasks		; bitmask containing the powers of two
	.global RemoveCursor
