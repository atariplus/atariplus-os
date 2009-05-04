;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: screen.i,v 1.15 2003/04/14 20:59:32 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the S: handler		**
;;; **********************************************************************

;; Definitions for the screen handler


AnticMode		=	$51	; antic mode that fits to the graphics mode, temporary
RowTmp			=	$51	; ditto, another name

CursorRow		=	$54	; cursor Y position
CursorColumn		=	$55	; cursor X position (two bytes)
GfxMode			=	$57	; the active graphics mode
GfxOrigin		=	$58	; origin of the graphics screen in memory
OldCursorRow		=	$5a	; copy of the last cursor position
OldCursorColumn		=	$5b	; copy of the last cursor position
ScreenPtr		=	$64	; temporary helper
STmp1			=	$66	; temporary 1
STmp2			=	$67	; temporary
DLBottom		=	$68	; bottom address of the new display list
ScrollPtr		=	$68	; another name for this temporary
RamTop			=	$6a	; topmost usable page, screen starts underneath (screen handler private)
BitMask			=	$6e	; temporary masking register
ShiftMask		=	$6f	; contains the mask to mask out the current pixel, too
RowAccu			=	$70	; error accumulator of the row for the line drawer
ColumnAccu		=	$72	; error accumulator for the column, line drawer
DeltaMax		=	$74	; maximum of the two delta's
DeltaRow		=	$76	; incrementer register for the line drawer
DeltaColumn		=	$77	; incrementer register for the line drawer
Counter			=	$7e	; counter register for the draw function, and other temporaries
MaxRows			=	$29d	; temporary holder of screen height in lines
ReqBytes		=	$29e	; temporary for the memory allocator
PixelMask		=	$2a0	; contains the mask to mask out the current pixel
FillFlag		=	$2b7	; set if the line drawer is in fact a filler
TmpCursorRow		=	$2b8	; temporary cursor row for area filler
TmpCursorColumn		=	$2b9	; ditto for the column
TmpScreenByte		=	$2bc	; area filler temporary color saver
NewCursorRow		=	$2f5	; for the line drawer: The target position
NewCursorColumn		=	$2f6	; ditto, but the column
DeltaRowSign		=	$2f8	; incrementer in Y direction
DeltaColumnSign		=	$2f9	; incrementer in X direction
ScreenChar		=	$2fa	; the last input or output byte of the screen handler (Antic code)
ScreenByte		=	$2fb	; screen handler primary result code for CIO (Ascii code)
FillColor		=	$2fd	; color to fill areas with
ScreenStack		=	$318	; stack pointer for the screen handler (also used by SIO)
CharacterSet		=	$e000	; the default character set

;; globally defined entry points
	.global ScreenOpen		; CIO S: open vector
	.global ScreenClose		; CIO S: close vector
	.global ScreenGet		; CIO S: get vector
	.global ScreenPut		; CIO S: put vector
	.global ScreenStatus		; CIO S: status vector
	.global ScreenSpecial		; CIO S: special vector
	.global ScreenInit		; initialize the screen handler

	.global OpenScreen		; helper vector for S: and E: open
	.global ComputeCursorAddress	; compute the address of the graphics cursor
	.global ModeWidth		; widths of the graphics modes
	.global ModeHeight		; heights of the graphics modes
	.global PutChar			; write a single character to the screen
	.global	StoreToCursor		; write an antic encoded byte to the cursor position
	.global ReadFromCursor		; read a character at the cursor position
	.global ClearScreen		; clear the screen contents
	.global TranslateToAscii	; translate the screen code to ascii	
	.global LoadScreenPtr		; load the screenPtr with the graphics origin
