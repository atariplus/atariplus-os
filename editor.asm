;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: editor.asm,v 1.28 2014/01/01 18:22:16 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the E: handler		**
;;; **********************************************************************

	.include "editor.i"
	.include "screen.i"
	.include "keyboard.i"
	.include "cio.i"
        .include "errors.i"
	.include "nmi.i"
	.include "irq.i"
	.include "antic.i"
        
        .segment  "OsHi"
	
;;; *** EditorOpen
;;; *** This is the open vector of the editor device
	.global EditorOpen
.proc	EditorOpen
	lda ZAux1	 	; mask out the graphics mode
	and #$0f		; which we don't need
	sta ZAux1
	lda #$00		; reset the target graphics mode
	sta GfxMode		; use a graphics 0 screen
	jsr OpenScreen		; run the screen handler to do this buisiness
	tya
	bmi error	
	lda #24
	sta WindowHeight	
	lda #0
	sta SwapFlag		; text data not swapped
	jsr EditorScreenInit	; initalize editor variables
	jsr RenderCursor	; display the cursor
	ldy #$01
error:
	rts
.endproc
;;; *** EditorScreenInit
;;; *** Initialize the internals of the editor.
;;; *** To be called either from editor open, or from
;;; *** screen open if we have a text window.
	.global EditorScreenInit
.proc	EditorScreenInit
	lda #$00
	sta GfxMode		; set the gfx mode for the text window
	sta CursorPtr
	sta CursorPtr+1		; indicate that there is no cursor to remove currently
	sta CursorInhibit	; make the cursor visible	
	sta BufferCnt		; indicate that the buffer is empty
	
	ldy #TabStopsSize-1
	lda #$01		; now reset all TAB stops: Every eight's stop
tabinit:
	sta TabStops,y
	dey
	bpl tabinit
	jsr ClearLogical	; reset the start of all logical lines
	
	lda #2
	sta LeftMargin		; initialize the left margin
	lda #39
	sta RightMargin		; initialize the right margin	
	jmp CursorHome		; reset the cursor position
.endproc
;;; *** EditorClose
;;; *** This is the close vector for the E: handler
;;; *** interface
	.global EditorClose
.proc	EditorClose
	;; The XL Os disables here finescrolling and re-opens the screen.
	;; I don't think this is overly smart as this may cause close
	;; to fail.
	ldy #$01
	rts
.endproc
;;; *** ExitStatusEditorHandler
;;; *** exit from the editor handler with the status byte
.proc	ExitStatusEditorHandler
.endproc
	;; runs into the following
;;; *** EditorStatus
;;; *** The status vector for the E: handler, used by CIO.
	.global EditorStatus
.proc	EditorStatus
	ldy ScreenError		; since we're using services of the screen handler, we also get the error code here.
	lda #$01
	sta ScreenError
	lda ScreenByte
	;; runs into the following
.endproc
;;; *** EditorSpecial
;;; *** The special handler of the E: handler. This handler
;;; *** doesn't provide any special calls, hence returns
;;; *** immediately with the Y register preloaded error
;;; *** code of CIO.
	.global EditorSpecial
.proc	EditorSpecial
	;; runs into the following
.endproc
;;; *** Editor handler init routine, used for CIO
;;; *** and the reset handler. 
	.global EditorInit
.proc	EditorInit
	rts
.endproc
;;; *** EditorGet
;;; *** This is the get vector ofthe E:	device, used
;;; *** by CIO.
	.global EditorGet
.proc	EditorGet
	tsx
	stx ScreenStack		; keep stack position
	jsr SwapGfxWindowCursor	; swap in editor settings
	jsr CheckEditorCursor	; check whether the E: cursor is in range
	lda BufferCnt		; do we still have some characters to deliver?
	bne providebuffer
	lda CursorRow
	sta StartLogicalRow	; set the position of the logical row and keep the address
	lda CursorColumn
	sta StartLogicalColumn	; ditto
inputloop:
	jsr KeyboardGet		; read a character from the keyboard
	sty ScreenError		; and the secondary result
	sta ScreenByte		; keep the result
	cmp #$9b		; do we have an EOL here?
	beq endinput		; if so, input is done
	ldy #$00
	sty OverrunFlag		; run buzzer?
	jsr EditorOutput	; write this byte out
	lda OverrunFlag		; shall we run the buzzer?
	bne buzzer
	lda #80-7
	clc
	adc RightMargin
	cmp LogicalColumn	; if positioned right, buzzer!
	bne inputloop
buzzer:
	jsr PutBuzzer
	clc
	bcc inputloop		; loop back
endinput:			; here:	User entered an EOL
	jsr RemoveCursor	; remove the cursor, we need to read data back
	jsr ComputeLineSize	; compute the size of the buffer in characters
	lda StartLogicalRow	; adjust the cursor position back
	sta CursorRow
	lda StartLogicalColumn
	sta CursorColumn
providebuffer:			; we do have characters in the buffer here
	lda BufferCnt		; check the buffer
	beq bufferempty		; if the editor is empty, bail out
dropbuffer:
	dec BufferCnt		; one character less
	beq bufferempty		; remove one character from the buffer
	lda ScreenError		; in case any error has been accumulated
	bmi dropbuffer		; cleanup the buffer and deliver the error
	;; the following mirrors the ScreenGet
	;; call, but I'd prefer to decouple S: and E: here
	jsr ReadFromCursor	; read the character under the cursor
	jsr TranslateToAscii	; translate from antic to Ascii set
	lda #1
	sta InsDat
	jsr BumpCursor		; move cursor forwards
exitread:
	jsr SwapGfxWindowCursor	; swap out the editor specials
	jmp ExitStatusEditorHandler
bufferempty:
	jsr AdvanceLineEOL	; advance the cursor
	jsr RenderCursor	; draw the cursor at last
replyeol:
	lda #$9b		; return an EOL
	sta ScreenByte
	bmi exitread
.endproc
;;; *** ComputeLineSize
;;; *** compute the number of characters in the line
;;; *** starting at StartLogicalRow/Column
.proc	ComputeLineSize
	lda CursorRow		; keep the current cursor position
	pha
	lda CursorColumn
	pha
	lda LogicalColumn	; ditto
	pha
	lda StartLogicalRow	; reset the cursor to the start of the logical line
	sta CursorRow
	lda StartLogicalColumn
	sta CursorColumn
	lda #1			; initialize the buffer size
	sta BufferCnt
scanloop:
	ldx CursorRow
	cpx WindowHeight	; at the end of the window?
	bcc notend
	lda CursorColumn
	cmp RightMargin
	bcc notend
	inc BufferCnt		; here we are at the last character of the screen. Prevent scrolling
	bne readloop		; the above should be ne
notend:
	jsr AdvanceCursor	; to the next character
	inc BufferCnt		; one additional character
	lda LogicalColumn	; up to the next line start
	cmp LeftMargin
	bne scanloop
	dec CursorRow		; up again
	jsr PutCursorLeft	; and left:	puts the cursor at the end of the line again.
readloop:
	;; now skip trailing blanks
	jsr ReadFromCursor	; something there?
	cmp #0
	bne exit		; if so, done
	dec BufferCnt
	beq exit		; if all used up, bail out
	lda LogicalColumn
	cmp LeftMargin
	beq exit		; again at start position->bail out
	jsr PutCursorLeft	; move back
	lda CursorColumn
	cmp RightMargin
	bcc readloop		; move backwards
	dec CursorRow		; move up
	bcs readloop
exit:
	pla 
	sta LogicalColumn	; restore position
	pla
	sta CursorColumn
	pla
	sta CursorRow
	rts
.endproc
;;; *** EditorPut
;;; *** This is the put vector of the E: device, used
;;; *** by CIO.
	.global EditorPut
.proc	EditorPut
	sta ScreenByte		; keep the data to be put	
	tsx
	stx ScreenStack		; keep stack position
	jsr SwapGfxWindowCursor	; get possibly editor relevant data into the cursor settings for the textwindow
	jsr CheckEditorCursor	; check whether the E: cursor is in range
	lda #0
	sta SuperFlag		; clear super flag for special keyboard functions
	jsr EditorOutput	; write this character out
	jsr SwapGfxWindowCursor	; swap gfx cursor position back in
	jmp ExitStatusEditorHandler ; and bail out again
.endproc
;;; *** EditorOutput
;;; *** write one byte (including control characters)
;;; *** onto the editor output. Also used by the input
;;; *** to echo characters
.proc	EditorOutput
	jsr RemoveCursor
	jsr GetSpecial		; check for special control codes and get the jump-in address
	beq foundspecial	; found a special control character?
direct:				; here:	print directly
	asl EscFlag		; also disable ESC for the next printing
	lda ScreenByte
	cmp #$9b		; for some wierd sense, we cannot escape EOL
	bne noteol
	jsr AdvanceLineEOL	; done with it
	jmp done
noteol:
	jsr PutChar		; use the screen method for that
	jsr AdvanceCursorEOL	; and advance the cursor
	jmp done
foundspecial:			; here:	 special key
	lda DirectFlag
	ora EscFlag		; print uninterpreted?
	bne direct
	asl EscFlag		; shift out the ESC'ing
	jsr JumpSpecial		; now call the special handler for this control character
done:	
	jsr RenderCursor	; redisplay the cursor now
	rts
JumpSpecial:
	jmp (ScreenPtr)
.endproc
;;; *** CheckCtrlCode:
;;; *** Check whether the ATASCII code in A is a special
;;; *** control code. If so, return with Z flag set, otherwise
;;; *** without this. MUST NOT modify the accumulator
	.global CheckCtrlCode
.proc	CheckCtrlCode
	ldx #CtrlCodeLen-1
cmplp:
	cmp CtrlCodeTable,x
	beq found		; found a control code
	dex
	bpl cmplp
found:
	rts
;;; The following table contains all valid
;;; control codes
CtrlCodeTable:
	.byte $1b		; ESC and
	.byte $1c,$1d,$1e,$1f	; cursor movement
	.byte $7d,$7e,$7f	; clear screen, TABs
	.byte $9b		; EOL
	.byte $9c,$9d,$9e,$9f	; insertion/deletion
	.byte $fd,$fe,$ff	; ditto
CtrlCodeLen = *-CtrlCodeTable
.endproc
;;; *** GetSpecial
;;; *** This function checks whether the ScreenByte is a control
;;; *** character. If so, it returns with carry set and the
;;; *** handler for this character in ScreenPtr, otherwise C is cleared
.proc	GetSpecial
	lda ScreenByte
	jsr CheckCtrlCode	; do we have a control code
	bne exit
	txa
	asl a
	ldy SuperFlag		; super flag set? (=> special editor features)
	beq normal
	adc #SuperTable-SpecialTable ; go to the super function handling table
normal:	
	tax	
	lda SpecialTable,x	; low byte
	sta ScreenPtr
	lda SpecialTable+1,x
	sta ScreenPtr+1
	lda #$00		; set the Z flag
exit:	
	rts
;;; this table contains the entry points for all cursor handling routines
SpecialTable:
	.word PutEsc
	.word PutCursorUp,PutCursorDown,PutCursorLeft,PutCursorRight
	.word PutClearScreen,PutBackspace,PutTab
	.word PutEOL
	.word PutDeleteLine,PutInsertLine,PutDeleteTab,PutInsertTab
	.word PutBuzzer,PutDeleteChar,PutInsertChar
SuperTable:			; for editor "super" functions
	.word PutCursorHome,PutCursorEnd,PutCursorLeftBorder,PutCursorRightBorder
.endproc
;;; Various control character interpretation rules follow from this point
;;; below.

;;; *** PutEsc:	Handle printing of the Escape symbol
.proc	PutEsc
	lda #$80
	sta EscFlag		; signal esc'ing
	rts
.endproc
;;; *** PutUp:	Handle printing of cursor up
.proc	PutCursorUp
	dec CursorRow		; just reduce the cursor position
	bpl isfine
	ldx WindowHeight
	dex
	stx CursorRow		; possibly wrap around
isfine:
	jmp CompleteLogical	; now recompute the logical position
.endproc
;;; *** PutDown: Handle printing of cursor down
.proc	PutCursorDown
	ldx CursorRow
	inx			; increment the cursor position
	cpx WindowHeight	; out of bounds?
	bcc isfine
	ldx #0			; wrap around
isfine:
	stx CursorRow
	jmp CompleteLogical	; recompute logical coordinates
.endproc
;;; *** PutCursorLeft: Handle printing of cursor left
.proc	PutCursorLeft
	ldx CursorColumn
	dex			; to the left
	bmi wraparound
	cpx LeftMargin		; or below the left?
	bcs isfine
wraparound:
	ldx RightMargin		; go to the right (has been checked above)
isfine:
	stx CursorColumn
	jmp GetLogicalColumn	; the logical line itself did not change, so keep it short
.endproc
;;; *** PutCursorRight: Handle printing of cursor right
.proc	PutCursorRight
	ldx CursorColumn
	cpx RightMargin		; already larger than that?
	bcs wraparound
	inx
	cpx #40			; wrap-around?
	bcc isfine
wraparound:
	ldx LeftMargin		; to the left margin
isfine:
	stx CursorColumn
	jmp GetLogicalColumn
.endproc
;;; *** PutClearScreen
;;; *** Clear the screen contents,
;;; *** and reset the editor contents
.proc	PutClearScreen
	jsr ClearScreen		; use the S: handler to do that for us.
	jsr CursorHome		; move the cursor to the home position
	;; now make each line the start of a logical line
	;; runs into the following
.endproc
;;; *** ClearLogical
;;; *** reset the logical line starts for the editor
.proc	ClearLogical
	ldy #LineStartsSize-1
	lda #$ff
slp:
	sta LineStarts,y
	dey
	bpl slp
	rts
.endproc
;;; *** PutBackspace
;;; *** implements the function of the backspace function
.proc	PutBackspace
	jsr GetLogicalColumn	; workaround against programs poking around
	lda LeftMargin		; are we deleting at the beginning of the line
	cmp LogicalColumn
	beq delstartlogical
	cmp CursorColumn	; if so, we need to wrap-around
	bne innerdelete		; keep it simple
	jsr DropLine		; drop the line if empty
innerdelete:
	jsr PutCursorLeft	; run the cursor-left routine
	lda CursorColumn	; did we wrap around?
	cmp RightMargin
	bcc nolinewrap		; if not, regular case and bail out
	lda CursorRow		; if we are in the first row, remain there
	beq nolinewrap
	jsr PutCursorUp		; move the cursor up
nolinewrap:
	lda #$00		; erase with
	jsr StoreToCursor	; blank
	jsr GetLogicalColumn
delstartlogical:
	rts
.endproc
;;; *** PutTab
;;; *** implements the TAB function
.proc	PutTab
tabcnt:
	jsr PutCursorRight	; move at least one character to the right
	lda CursorColumn
	cmp LeftMargin		; if we are at the left margin now
	bne notleft
	;; jump to the next line
	lda #$01
	sta InsDat
	jsr AdvanceLine		; go to the next line
	jsr TestCursorForLogicalLine	; are we now at the start of a logical line
	bcs newline		; if so, bail out
notleft:
	lda LogicalColumn
	jsr TestForTabStop	; check whether we have a TAB stop here
	bcc tabcnt		; continue until we find a tabstop
	jsr GetLogicalColumn	; recompute where we are
newline:
	rts
.endproc
;;; *** PutDeleteLine
;;; *** delete the line under the cursor and
;;; *** scroll the remaining lines up
.proc	PutDeleteLine
	jsr GetLogicalColumn	; compute the line we are in
	sty CursorRow		; store the cursor there
	jmp DeleteLine		; now erase this line and some possibly following lines
.endproc
;;; *** PutDeleteTab
;;; *** remove a TAB stop at the current logical position
.proc	PutDeleteTab
	lda LogicalColumn	; at the current line
	jmp ClearTAB
.endproc
;;; *** PutInsertTab
;;; *** insert a TAB stop at the current logical position
.proc	PutInsertTab
	lda LogicalColumn
	jmp SetTAB
.endproc
;;; *** PutBuzzer
;;; *** generate the buzzer sound ("Bell")
.proc	PutBuzzer
	lda #$20
buzloop:
	jsr KeyboardClick	; does not modify the accumulator
	sec
	sbc #1
	bpl buzloop
	rts
.endproc
;;; *** PutDeleteChar
;;; *** remove a single character at the cursor position
.proc	PutDeleteChar
	lda CursorRow
	pha			; saveback row and column of the cursor
	lda CursorColumn
	pha			; ditto
delloop:
	jsr ComputeCursorAddress ; get pointer to where we are
	lda ScreenPtr
	sta ScrollPtr		; saveback as well
	lda ScreenPtr+1
	sta ScrollPtr+1
	jsr AdvanceCursor	; move the cursor forward one character
	lda LogicalColumn
	cmp LeftMargin		; end of the logical line reached
	beq last
	lda CursorRow
	cmp WindowHeight	; or are we in the last row?
	bcs last
	jsr ReadFromCursor	; if not, read the character under the cursor = the next one (also clears Y)
	ldy #$00		; set Z flag
	sta (ScrollPtr),y	; store in the previous position = drop the character
	beq delloop		; branches always:	advance to the next character
last:				; last character in a line reached
	ldy #$00
	tya
	sta (ScrollPtr),y	; shift in zeros
	jsr DropLineAbove	; erase a line, possibly
	pla
	sta CursorColumn
	pla
	sta CursorRow		; Restore the cursor position
	jmp GetLogicalColumn	; again. 
.endproc
;;; *** PutInsertChar
;;; *** insert a blank character at the cursor position
.proc	PutInsertChar
	lda CursorRow		; keep cursor position for later
	pha
	lda CursorColumn
	pha
	jsr ReadFromCursor	; keep the character under the cursor
	sta InsDat		; here
	lda #$00		; clear the scroll flag
	sta ScrollFlag		; for later testing
	;; insert now a blank
insertloop:
	jsr StoreToCursor	; insert into the cursor position: A blank or the last character
	lda #80-7
	clc
	adc RightMargin
	cmp LogicalColumn	; if positioned right, buzzer!
	bne nowarn
	lda InsDat
	sta OverrunFlag		; warn if non-space here
nowarn:	
	jsr Advance		; advance by one character now:	If blank, do not scroll (implicit!)
	lda LogicalColumn	; end of the line? (If this gets LeftMargin, we got a wraparound)
	cmp LeftMargin
	beq last
	lda InsDat		; keep the old character
	pha
	jsr ReadFromCursor	; read the next
	sta InsDat		; keep it
	pla
	clc
	bcc insertloop		; continue until all characters are done
last:
	pla
	sta CursorColumn	; restore cursor position
	pla
	;; now correct for inserted lines: Adjust cursor position
	; sec			; carry is set
	sbc ScrollFlag		; reduce by scrolled flags (carry is set here)
	sta CursorRow		; and keep new Y position of the cursor
	jmp GetLogicalColumn	; recompute the logical column
.endproc
;;; *** PutCursorLeftBorder
;;; *** move the cursor to the left edge
.proc	PutCursorLeftBorder
	jsr CursorLeft
	jmp GetLogicalColumn	; recompute
.endproc
;;; *** PutCursorRight
;;; *** move the cursor to the right edge
.proc	PutCursorRightBorder
	lda RightMargin
	sta CursorColumn
	jmp GetLogicalColumn
.endproc
;;; *** PutCursorEnd
;;; *** move the cursor to the end position
.proc	PutCursorEnd
	jsr CursorHome		; move to home
	jmp PutCursorUp		; and move upwards with wrap-around
.endproc
;;; *** DropLineAbove:
;;; *** Check whether the line above is empty, if so, drop it.
.proc	DropLineAbove
	lda LogicalColumn
	cmp LeftMargin		; are we left?
	bne notfirst
	dec CursorRow		; move upwards
notfirst:
	jsr GetLogicalColumn
	;; runs into the following
.endproc
;;; *** DropLine:	Check whether the current line is
;;; *** completely empty. If so, remove it.
.proc	DropLine
	lda LogicalColumn
	cmp LeftMargin		; are we at the start of the line?
	beq exit		; perform nothing there
	jsr ComputeCursorAddress ; compute the cursor position
	lda RightMargin
	sec
	sbc LeftMargin		; maximal number of characters in a row
	tay
lineloop:
	lda (ScreenPtr),y	; non-space here?
	bne exit
	dey
	bpl lineloop
	;; here: line is completely blank, remove it
	jsr DeleteLine		; remove the line
exit:
	rts
.endproc
;;; *** DeleteLine
;;; *** delete the line at the cursor position and scroll
;;; *** the remaining lines up.
.proc	DeleteLine
delloop:
	;; First delete the line under the cursor position
	;; by moving the logical line start flags up
	ldy CursorRow
moveuploop:
	iny
	tya
	jsr TestForLogicalLine
	;; keep the result (might scroll nonsense if this is the last line, but I don't care)
	dey			; back to the original
	tya
	jsr DefineLogicalLine	; install the line above
	iny			; up to the window height
	cpy WindowHeight
	bcc moveuploop
	dey			; get the last line
	sec
	tya
	jsr DefineLogicalLine	; mark the last line as logical line start (it will be empty)
	jsr DeleteLineGraphical	 ; scroll now the graphic lines
	jsr TestCursorForLogicalLine	; are we?
	bcc delloop		; if not, delete this line as well
	jmp PutCursorLeftBorder	; reset the cursor
.endproc	
;;; *** CheckEditorCursor
;;; *** Check for validity of the E: cursor
;;; *** and the availibity of a text mode. Reopen
;;; *** if it is not available.
	.global CheckEditorCursor
.proc	CheckEditorCursor
	lda SwapFlag		; are we in the graphics window anyhow?
	bmi havewindow
	lda GfxMode		; check whether we have any editor-friendly mode (namely, zero)
	beq havewindow
	;; here not. Reopen the editor (yuck!)
	jsr EditorOpen
	tya			; error code?
	bmi error
havewindow:
	;; everything is fine so far. Check right and left border
	lda #39
	cmp RightMargin		; must be at most 39
	bcs rightfine
	sta RightMargin
rightfine:
	lda RightMargin
	cmp LeftMargin		; must be smaller
	bcs leftfine
	lda #2			; reset to defauls
	sta LeftMargin
leftfine:
	lda CursorRow		; get cursor Y position
	cmp WindowHeight	; check for out of bounds in this direction
	bcs boundserror
	lda CursorColumn	; must be at most 39
	cmp #40
	bcs boundserror

	lda #$01		; everything fine here
	sta ScreenError
	ldy #BreakError		; reset the break flag
	ldx BreakFlag
	sty BreakFlag		; does not set flags
	beq error
	rts			; everything fine here
boundserror:
	jsr CursorHome
	ldy #OutOfRange		; error code in case the cursor run off
error:
	sty ScreenError
	ldx ScreenStack		; restore stack
	txs			; pointer
	lda SwapFlag		; window<->gfx swapped?
	bpl noswap
	jsr SwapGfxWindowCursor	; swap cursor addressing in graphics/window
noswap:
	jmp ExitStatusEditorHandler
.endproc	
;;; *** SwapGfxWindowCursor
;;; *** Swap cursor location in the text window and the gfx screen
	.global SwapGfxWindowCursor
.proc	SwapGfxWindowCursor
	lda WindowHeight	; get # lines in the text window
	cmp #24			; only if there is a window
	bcs nowindow
	ldx #11			; amount of data to swap around
swaploop:
	lda CursorRow,x
	pha
	lda WindowRow,x
	sta CursorRow,x
	pla
	sta WindowRow,x
	dex
	bpl swaploop
	lda SwapFlag
	eor #$ff		; invert the swap flag
	sta SwapFlag
nowindow:
	rts
.endproc
;;; *** RemoveCursor
;;; *** Remove the cursor by drawing back the character that has
;;; *** been stored there.
	.global RemoveCursor
.proc	RemoveCursor
	lda CursorPtr+1		; do we have a cursor address?
	ora CursorPtr
	beq nocursor
	ldy #0
	lda ChrUnderCursor	; get the old character under the cursor
	sta (CursorPtr),y	; store it
	sty CursorPtr
	sty CursorPtr+1
nocursor:
	rts
.endproc
;;; *** RenderCursor
;;; *** Render the cursor at the cursor position and exit
;;; *** Note that this here only applies for text mode.
.proc	RenderCursor
	ldx CursorInhibit	; is the cursor visible?
	bne nocursor		
	jsr ReadFromCursor	; read the character under the cursor
	sta ChrUnderCursor	; store it
	eor #$80		; invert the character
	ldy #$00
	sta (ScreenPtr),y	; store back inverted
	lda ScreenPtr
	sta CursorPtr
	lda ScreenPtr+1
	sta CursorPtr+1		; keep the address
nocursor:
	rts
.endproc
;;; *** PutCursorHome
;;; *** move the cursor to the home position
.proc	PutCursorHome
	;; runs into the following
.endproc
;;; *** CursorHome
;;; *** place the cursor at its home position
;;; *** in the left top border
	.global CursorHome
.proc	CursorHome
	jsr CursorLeft		; place the cursor now in its home position
	sta LogicalColumn
	sta StartLogicalColumn
	lda #$00
	sta CursorRow		; clear the row as well
	sta StartLogicalRow
	rts
.endproc
;;; *** CursorLeft
;;; *** Place the cursor to the left
.proc	CursorLeft
	lda #$00		; default: zero
	sta CursorColumn+1	; ditto
	lda LeftMargin
	sta CursorColumn	; now store the cursor column
	rts
.endproc	
;;; *** AdvanceCursor
;;; *** Move cursor forwards
;;; *** passively, without inserting lines
.proc	AdvanceCursor
	lda #$00
	beq BumpCursor
.endproc

;;; *** AdvanceEOL
;;; *** Move the cursor forwards,
;;; *** possibly inserting lines because the
;;; *** user is actively editing the line
;;; *** or the screen handler is printing.
.proc	AdvanceCursorEOL
	lda #$9b
	;; runs into the following
.endproc
;;; *** BumpCursor
;;; *** Advance the cursor. Gets a flag in A that indicates
;;; *** the behaivour that shall happen in case we reached
;;; *** the end of a line.
.proc	BumpCursor
	sta InsDat		; keep the scrolling mode here
	;; runs into the following
.endproc
;;; *** Advance
.proc	Advance	
	inc LogicalColumn	; advance the logical position
	
	inc CursorColumn	; advance the cursor position (in text mode: hi only)
	
	;; Check whether the cursor run accross the right border
	lda RightMargin		; check whether the cursor is still in range
	cmp CursorColumn
	bcs nolinewrap		; if right margin is higher or equal, then we are in range
	;; from this point on:	Must advance the line
	lda LogicalColumn	; get the logical column we are in
	cmp #81			; near end of the logical line?
	bcc notnearend
	;; we are about to insert the last line of a logical line block	
	lda InsDat		; check for the advancing flag
	beq AdvanceLine		; plain wraparound if not EOL-mode: Don't scroll.
	lda #$9b		; switch to EOL mode:	Enforce the insertion
	sta OverrunFlag
	sta InsDat		; of a new logical line, even if the caller doesn't want to.
	bmi AdvanceLine
notnearend:
	jsr AdvanceLine		; first, regular insertion "as always" with keeping the mode
	jsr TestCursorForLogicalLine	; at this cursor position
	bcc nolinewrap		; if not, bail out
	lda InsDat		; EOL insertion flag set?
	beq nolinewrap		; if not EOL, don't bother
	clc			; here EOL mode: continue the logical line with a blank line
	jsr InsertLine		; otherwise, insert a line here to make room for the logical line
nolinewrap:
	rts			; bail out, nothing more to do
.endproc
;;; *** PutEOL
;;; *** the same EOL function:	Insert an EOL here
.proc	PutEOL
	;; runs into the following
.endproc
;;; *** AdvanceLineEOL
;;; *** Advance by a line, possibly insert a line.
	.global AdvanceLineEOL
.proc	AdvanceLineEOL
	lda #$9b
	sta InsDat		; set the flag to EOL mode: active line insertion
	;;			; runs into the following
.endproc
;;; *** AdvanceLine
;;; *** Advance by a line. If InsDat is nonzero, then
;;; *** insert a new line and scroll up. If it is EOL, make
;;; *** the new line the beginning of a logical line, else don't.
.proc	AdvanceLine
	jsr CursorLeft		; move the cursor to the left boundary
	inc CursorRow		; increment the vertical cursor position
	;; now check for vertical wrap-around
	lda WindowHeight	; get the height of the text buffer
	cmp CursorRow		; check whether we reached the bottom
	bne exit		; if not in the bottom, keep going
	
	sta MaxRows		; keep the number of rows we have to scroll
				; if we want to put a character over the border
	lda InsDat		; check whether we have EOL insertion
	beq exit		; if not, ditto (also signal errors)
	
	cmp #$9b		; is EOL?
	beq set			; sets C: Insert the start of a new line
	clc			; otherwise, clear carry: no new line
set:
	
scrollloop:
	jsr ScrollUp
	inc ScrollFlag		; scrolling has happened
	dec StartLogicalRow	; start of the logical row is now one on top
	bpl nocarry2
	inc StartLogicalRow
nocarry2:
	dec MaxRows		; keep going, scroll one line less now, remember where to place the cursor
	lda LineStarts		; is the first line now the start of a logical line?
	sec
	bpl scrollloop		; if not, insert new lines and make them logical

	lda MaxRows		; get the line number of where the cursor is now
	sta CursorRow
exit:
	jmp GetLogicalColumn
.endproc
;;; *** PutInsertLine
;;; *** insert a new blank line under the cursor and
;;; *** scroll the remaining lines down.
.proc	PutInsertLine
	sec			; insert a new line that is the start of a new logical line
	;; runs into the following
.endproc	
;;; *** InsertLine
;;; *** Insert a blank line into the line at the cursor
;;; *** position. C set indicates that the new line shall
;;; *** be the start of a logical line, otherwise it's not.
.proc	InsertLine
	;; First care about the logical line start flags
	php			; keep the set/clear flag
	ldy WindowHeight	; last line that gets moved
	dey
moveloglp:
	dey
	bmi lastline
	tya
	jsr TestForLogicalLine	; check whether this line is the start of a logical line
	;; keep the result
	iny			; next line
	tya
	jsr DefineLogicalLine	; install in the line below
	dey
	cpy CursorRow		; until we reach the cursor row
	bcs moveloglp
lastline:
	lda CursorRow
	plp			; restore flags
	jsr DefineLogicalLine	; and set here the logical flag
	;; Now scroll the graphics up
	ldx RamTop
	dex
	stx ScreenPtr+1
	lda #$d8
	sta ScreenPtr		; source of data to move: Last line
	ldx CursorRow		; get Y position
insertionloop:
	inx			; advance to the next line
	cpx WindowHeight	; until we're done
	bcs exit
	lda ScreenPtr
	sta ScrollPtr		; now get the line above as source into screenptr
	sbc #40-1		; carry is cleared
	sta ScreenPtr
	lda ScreenPtr+1
	sta ScrollPtr+1
	sbc #0
	sta ScreenPtr+1
	ldy #40-1
mvlineloop:
	lda (ScreenPtr),y
	sta (ScrollPtr),y	; move one line over
	dey
	bpl mvlineloop
	bmi insertionloop	; continue moving
exit:
	jsr ClearScreenLine
	jmp PutCursorLeftBorder		; move the cursor the left border
.endproc
;;; *** ScrollUp
;;; *** Scroll the screen upwards.
;;; *** C shall be set if the line scrolled in is the start of a logical line
;;; *** clear otherwise.
.proc	ScrollUp
	rol LineStarts+2	; adjust the line starts, scroll in the new flag
	rol LineStarts+1
	rol LineStarts
	
	bit FineScrollFlag	; is finescrolling enabled?
	bpl nofinescroll
waitlp:
	lda FineScroll		; wait until scrolling of last line is over
	bne waitlp
	lda #$08
	sta FineScroll		; re-initialize scrolling
wait2lp:
	lda FineScroll
	cmp #$01
	bne wait2lp		; wait until this is over, too
anticlp:
	lda YPos
	cmp #$40
	bcs anticlp		; wait until this VBI is over, too
	ldx #$0d
	lda WindowHeight	; get amount of lines to scroll
	cmp #24			; small text window?
	bcs tallwindow
	ldx #$70
tallwindow:
anticlp2:
	cpx YPos
	bcs anticlp2		; wait again for syncinc
nofinescroll:
	jsr LoadScreenPtr
	ldx #$00		; at line #0
	jsr DeleteLineX		; remove this line and scroll up
	jmp GetLogicalColumn	; get the logical position within the line
.endproc
;;; *** DeleteLineGraphical
;;; *** delete the line under the cursor row and move
;;; *** the lines below up.
.proc	DeleteLineGraphical
	lda CursorColumn
	pha			; keep me (assume hi is zero, this is true for the editor, surely)
	lda #$00
	sta CursorColumn	; get the cursor address at the begin of the line
	jsr ComputeCursorAddress
	pla
	sta CursorColumn	; restore
	ldx CursorRow		; get Y position
	;; runs into the following
.endproc
;;; *** DeleteLineX
;;; *** Delete the line at screenptr, scroll
;;; *** remaining lines up. ScreenPtr must point
;;; *** to line in the X register
.proc	DeleteLineX
delloop:
	inx			; compute the target position:	is here
	cpx WindowHeight	; abort if done
	bcs done
	lda ScreenPtr		; get the target address
	adc #40			; carry is cleared
	sta ScrollPtr		; the source
	lda ScreenPtr+1
	adc #0
	sta ScrollPtr+1
	ldy #40-1		; bytes to move
movelp:
	lda (ScrollPtr),y	; move this line
	sta (ScreenPtr),y	; up
	dey
	bpl movelp
	lda ScrollPtr		; go to the next lower line
	sta ScreenPtr
	lda ScrollPtr+1
	sta ScreenPtr+1
	bne delloop		; should be ne, hopefully
done:				; screenptr points now to the last line
	;; runs into the following
.endproc
;;; *** ClearScreenLine
;;; *** clear the line at ScreenPtr
.proc	ClearScreenLine
	ldy #40-1	
	lda #$00		; clear me
clrline:
	sta (ScreenPtr),y
	dey
	bpl clrline
	rts
.endproc
;;; *** CompleteLogical
;;; *** get the logical column and keep the start position of the logical
;;; *** line.
.proc	CompleteLogical
	jsr GetLogicalColumn
	sty StartLogicalRow
	lda LeftMargin
	sta StartLogicalColumn
	rts
.endproc
;;; *** GetLogicalColumn
;;; *** Compute the logical column we are in
;;; *** Returns the logical row in the Y register.
.proc	GetLogicalColumn
	lda CursorColumn
	sta LogicalColumn	; guess we're here...
	ldy CursorRow		; get the row position
testloop:
	tya			; get this row
	jsr TestForLogicalLine	; test if this belongs to a logical line
	bcs endloop		; found the start of a logical line?
	lda LogicalColumn
	adc #40			; must be in a second or further row of a long line (C is clear)
	sta LogicalColumn
	dey			; count down
	bne testloop
endloop:
	rts
.endproc
;;; *** DefineLogicalLine
;;; *** Set or clear the indicator for a logical line.
;;; *** line number is in A, set/clear flag is in C
.proc	DefineLogicalLine
	php			; keep the set/clear flag
	clc
	adc #TabStopsSize*8	; addup offset
	plp
	;; runs into the following
.endproc
;;; *** DefineTAB
;;; *** Define a TAB stop at the logical column indicated in A.
;;; *** May also be used to define the start of a logical
;;; *** line if offset with TabStopsSize*8 is used
.proc	DefineTAB
	bcc ClearTAB
.endproc
;;; *** SetTAB
;;; *** Define a TAB stop at the logical column given by the A register
.proc	SetTAB
	jsr SetupTestMask
	lda BitMask
	ora TabStops,x
	sta TabStops,x
	rts
.endproc
;;; *** ClearTAB
;;; *** Clear a TAB stop at the logical column given by A
.proc	ClearTAB
	jsr SetupTestMask
	lda BitMask
	eor #$ff
	and TabStops,x
	sta TabStops,x
	rts
.endproc
;;; *** TestCursorForLogicalLine
;;; *** Test whether the cursor is in a line that
;;; *** starts a logical line.
.proc	TestCursorForLogicalLine
	lda CursorRow		; pre-load with the cursor position
	;; runs into the following
.endproc
;;; *** TestForLogicalLine
;;; *** Test whether the line in the A register is the start of
;;; *** a logical line. Returns with C set if so, otherwise with
;;; *** C cleared. MUST NOT TOUCH Y
.proc	TestForLogicalLine
	clc
	adc #TabStopsSize*8	; offset from TABMap to LineStart
.endproc
	;; ** runs into the following
;;; *** TestForTab
;;; *** Test whether the indicated logical position is a TAB-stop
;;; *** Returns C if so, otherwise C clear.
.proc	TestForTabStop
	jsr SetupTestMask
	clc
	lda TabStops,x
	and BitMask		; is this bit set?
	beq no
	sec
no:
	rts
.endproc
;;; *** Setup a bitmask for the given bit position in the accumulator
;;; *** returns the bitmask in BitMask, and the byte offset in
;;; *** the X register. MUST NOT TOUCH Y
.proc	SetupTestMask
	pha
	and #$07
	tax			; get the bit index
	lda BitMasks,x
	sta BitMask		; setup the bit mask
	pla
	lsr a
	lsr a
	lsr a
	tax
	rts
.endproc
	.global BitMasks
BitMasks:
	.byte $80,$40,$20,$10,$08,$04,$02,$01
