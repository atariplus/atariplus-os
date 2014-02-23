;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: screen.asm,v 1.32 2014/01/19 11:31:43 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the S: handler		**
;;; **********************************************************************

	.include "editor.i"
	.include "screen.i"
	.include "reset.i"
	.include "cio.i"
        .include "errors.i"
	.include "nmi.i"
	.include "irq.i"
	.include "pokey.i"
	.include "antic.i"
	.include "gtia.i"

        .segment  "OsHi"
;;; *** ScreenOpen
;;; *** This is the open vector of the editor device
	.global ScreenOpen
.proc	ScreenOpen
	lda ZAux2		; check the graphics mode
	and #$0f		; if it is graphics 0, permit no text window
	bne nottext
	jmp EditorOpen		; special hack to leave this to the editor then
nottext:
	sta GfxMode		; use a graphics 0 screen
	;; runs into OpenScreen
.endproc
;;; *** OpenScreen
;;; *** This performs the heavy work,
;;; *** namely the screen build-up for
;;; *** the E: and the S: device
;;; *** For the XL os, all modes from 0..15 are valid,
;;; *** all additional bits define flags.
	.global OpenScreen
.proc	OpenScreen
	lda DMACtrlShadow
	and #$dc		; DMA off, playfield width off
	sta DMACtrlShadow	; reset DMA: Normal playfield	
	lda #$80
	sta BreakFlag		; initalize the break flag
	lda #>CharacterSet	; get the default character set
	sta ChBaseShadow	; keep them.
	ldy #$01
	sty ScreenError	; set the status to "fine"
	iny
	sty ChCtrlShadow	; reset character control
	lda #$c0
	ora IRQStatShadow
	sta IRQStatShadow
	sta IRQStat		; allow keyboard and break-key input
	lda #$40
	sta NMIEnable		; enable the VBI, disable the DLI

	ldx #$04
colorinit:
	lda DefaultColors,x
	sta Color0Shadow,x	; install the default colors
	dex
	bpl colorinit

	inx			; back to zero
	ldy RamTop
	stx ScreenPtr		; reset the screen pointer
	stx GPriorSet
	sty ScreenPtr+1		; ditto
	lda #$60		; hard-code the start address
	dey
	sta WindowOrigin	; of the text window
	sty WindowOrigin+1

	
	ldx GfxMode
	lda AnticModes,x	; get the corresponding antic mode
	sta AnticMode

	;; Now check whether there is enough memory for all the modes
	lda ScreenPtr
	sec
	sbc MemRequiredLo,x
	sta ScreenPtr
	sta GfxOrigin
	lda ScreenPtr+1
	sbc MemRequiredHi,x
	sta ScreenPtr+1
	sta GfxOrigin+1
	;; Check whether there is enough memory left
	jsr ReserveMemory

	lda GPriorShadow	; get Prior
	and #$3f
	ora PriorMask,x		; additional GTIA mode depending on GfxMode
	sta GPriorShadow
	cpx #11			; graphics 11:	Set background value to 6
	bne notMode11
	lda #$06
	sta ColorBackShadow
notMode11:
	;; now wait until we get a VSync
waitvbi:	
	lda YPos
	cmp #$7a
	bcc waitvbi

	lda AlignMask,x		; check whether we need to align the DL
	beq noalign
	lda #$ff		; if so, align the screen ptr to the end of the previous page
	dec ScreenPtr+1
	sta ScreenPtr
noalign:
	jsr ReserveMemory	; two additional bytes for the jump address
	jsr ReserveMemory
	lda ScreenPtr
	sta DLBottom
	lda ScreenPtr+1
	sta DLBottom+1
	lda #$41		; Jump back:	 JVB instruction
	jsr WriteDL

	lda PriorMask,x		; check whether we can have a text window.
	beq havewindow
	lda ZAux1		; only a window if Aux1 is set
	and #$40
	beq nowindow
	sta GPriorSet           ; instruct the VBI to copy prior over
havewindow:
	lda ZAux1		; check whether the user wants a text window
	and #$10
	beq nowindow		; not if this bit is not set
	;; here:		; create a text window	
	lda #4			; initialize the text window height
	sta WindowHeight
	lda #0
	sta SwapFlag		; text data not swapped
	jsr SwapGfxWindowCursor	; swap editor variables in
	jsr EditorScreenInit	; initialize the editor variables
	jsr SwapGfxWindowCursor	; swap them into the private editor backup
	ldx #3-1		; number of regular Text#2 instructions to write
	lda #$02		; DL mode
	sta AnticMode		; keep for later
	ldy FineScrollFlag	; fine scrolling enabled?
	bpl nofine
	jsr BottomFineScroll	; write the fine scroll lines, modify the mode type
nofine:
bwldp:
	lda AnticMode
	jsr WriteDL
	dex
	bpl bwldp
	;; Now write the LMS instruction
	lda WindowOrigin+1
	jsr WriteDL
	lda WindowOrigin	; at fixed position relative to RamTop
	jsr WriteDL
	lda AnticMode
	ora #$40		; LMS Text#2 instruction
	jsr WriteDL
	ldy GfxMode
	lda PriorMask,y		; check whether we have a special mode here
	beq nodli
	rol a
	rol a
	rol a			; move to bits 0..1
	tax
	lda DLIVecLo-1,x
	sta VecDLI
	lda DLIVecHi-1,x
	sta VecDLI+1
	lda #$c0
	sta NMIEnable		; allow DLI
	lda #$8f		; write a DLI here
	jsr WriteDL
	lda #$0f		; regular hi-res modes
	sta AnticMode
	ldx #$40		; hard-coded value for graphics 8
	bne insertmodes
nodli:	
	lda AnticModes,y	; restore this now
	sta AnticMode
	ldx ModeLinesWindow,y	; get the number of mode lines in the window
	bne insertmodes		; jump to the mode writer
nowindow:
	ldy GfxMode
	ldx ModeLines,y		; get the mode line again
	tya			; fine scrolling only for mode #0
	bne insertmodes
	lda FineScrollFlag
	bpl insertmodes
	jsr BottomFineScroll	; install fine scrolling at the bottm
insertmodes:
	lda AnticMode
	jsr WriteDL		; write the modelines, bottom up
	dex
	bne insertmodes
	
	lda AnticMode		; Antic modes E,F need to reload the scan pointer
	and #$0f		; mask out finescrolling flag
	cmp #$0e
	bcc singlerun
	;; here: double display run due to 4K boundary cross
	lda RamTop
	sec
	sbc #$10		; 16 pages, hence 4K gives HI
	jsr WriteDL
	lda #$00
	jsr WriteDL		; LO is always blank
	lda AnticMode
	ora #$40		; set LMS run
	jsr WriteDL
	ldx #$5d		; remaining runs: All the same for further modes
inserttop:
	lda AnticMode
	jsr WriteDL
	dex
	bne inserttop
singlerun:
	;; now write the LMS instruction at the top of the screen.
	lda GfxOrigin+1
	jsr WriteDL
	lda GfxOrigin
	jsr WriteDL
	lda AnticMode
	ora #$40
	jsr WriteDL
	;; write three blank lines
	lda #$70
	jsr WriteDL
	lda #$70
	jsr WriteDL
	;; keep the address of the last DL as the start address of the display

	;; Check whether this worked fine so far
	ldy ScreenError		; did this work?
	bpl screenisopen	; if so, then we're open here
	ldx GfxMode             ; check whether we are in mode #0
        beq lost                ; if so, then we are hopelessy lost here!
        ;; fall back to mode #0
	tya
        pha
	jsr EditorOpen		; re-open this screen in graphics #0
        pla
lost:
        tya                     ; return with the error code
        rts
screenisopen:	
	ldy #$01
	lda ScreenPtr
	sta DListShadow
	sta (DLBottom),y	; write return address for JVB instruction
	lda ScreenPtr+1
	sta DListShadow+1
	iny
	sta (DLBottom),y
	lda #$70
	jsr WriteDL		; write the last instruction
	
	lda ScreenPtr
	sta MemTop
	lda ScreenPtr+1
	sta MemTop+1		; keep the memory top
	;; Now check whether we need to clean the screen
	lda ZAux1		; get it
	and #$20
	bne noclear		; do not clear
	jsr ClearScreen		; erase the screen contents
noclear:
	lda #$22
	ora DMACtrlShadow	; now enable the playfield in normal mode
	sta DMACtrlShadow
	ldy ScreenError		; now return the status
	rts
;;; *** BottomFineScroll:
;;; *** Install the bottom lines of the fine
;;; *** scrolling, and the DLI.
BottomFineScroll:
	lda #<FineDLI		; this requires DLI operation
	sta VecDLI
	lda #>FineDLI
	sta VecDLI+1
	lda #$c0
	sta NMIEnable		; and turn on the DLI operation
	sta GPriorSet
	lda #$02
	jsr WriteDL
	lda #$a2		; DLI, last fine scrolled line
	jsr WriteDL
	dex			; reduce mode line counter
	lda #$22		; DL instruction from now: VScroll enabled
	sta AnticMode
	rts
;;; *** Write a display list instruction into ScreenPtr
;;; *** backwards, then reserve this memory
WriteDL:	
	bit ScreenError	; in case of error, no need to continue
	bmi exit
	ldy #$00
	sta (ScreenPtr),y
	;; runs into the following
;;; *** Reserve a byte of screen memory
;;; *** and check whether we have still enough memory here
ReserveMemory:
	lda ScreenPtr
	bne nocarry
	dec ScreenPtr+1
nocarry:
	dec ScreenPtr
	lda AppMemHi
	cmp ScreenPtr
	lda AppMemHi+1
	sbc ScreenPtr+1
	bcc exit
	lda #OutOfMemory
	sta ScreenError	; outch!
exit:		
	rts
	
;;; Default colors
;;; This table contains the default playfield
;;; coloring.
DefaultColors:
	.byte $28,$ca,$94,$46,$00 ; red/orange,green,blue,magenta,black
;;; Memory requirements of the individual
;;; display modes in bytes
;;; The +16 does not cover the really used bytes, but is a requirement
;;; to guarantee proper alignment on the 4K boundary crossing at LMS
MemRequiredLo:
	.byte <(960),<(640),<(400),<(400),<(640),<(1120),<(2080),<(4000)
	.byte <(7840+16),<(7840+16),<(7840+16),<(7840+16),<(1120),<(640),<(4000),<(7840+16)
MemRequiredHi:
	.byte >(960),>(640),>(400),>(400),>(640),>(1120),>(2080),>(4000)
	.byte >(7840+16),>(7840+16),>(7840+16),>(7840+16),>(1120),>(640),>(4000),>(7840+16)
;;; Antic mode indexed by graphics mode
AnticModes:
	.byte $02,$06,$07,$08,$09,$0a,$0b,$0d
	.byte $0f,$0f,$0f,$0f,$04,$05,$0c,$0e
;;; Modifies GPRIOR according to the graphics mode
PriorMask:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$40,$80,$c0,$00,$00,$00,$00
;;; Checks whether the display list requires alignment due to oversize
AlignMask:
	.byte $00,$00,$00,$00,$00,$00,$00,$01
	.byte $01,$01,$01,$01,$00,$00,$01,$01
;;; Number of mode lines for non-window'd modes
ModeLines:
	.byte $17,$17,$0b,$17,$2f,$2f,$5f,$5f
	.byte $61,$61,$61,$61,$17,$0b,$bf,$61
	;; for window'd modes
ModeLinesWindow:
	.byte $13,$13,$09,$13,$27,$27,$4f,$4f
	.byte $41,$41,$41,$41,$13,$09,$9f,$41
	;; DLI vectors for text modes in gr.9,10,11
DLIVecLo:
	.byte <Gr9DLI,<Gr10DLI,<Gr11DLI
DLIVecHi:
	.byte >Gr9DLI,>Gr10DLI,>Gr11DLI
.endproc
;;; *** Gr11DLI
;;; *** This is the DLI routine entered for
;;; *** graphics 11
.proc	Gr11DLI
	pha
	lda ColorBackShadow	; background color
	eor ColorMask
	and DarkMask
	and #$f0		; darken
	jmp CommonColorDLI
.endproc
;;; *** Gr10DLI
;;; *** This is the DLI routine entered for
;;; *** graphics 10
.proc	Gr10DLI
	pha
	lda PColor0Shadow
	eor ColorMask
	and DarkMask
	jmp CommonColorDLI
.endproc
;;; *** Gr9DLI
;;; *** This is the DLI routine entered for
;;; *** graphics 9 text window.
.proc	Gr9DLI
	pha
	lda ColorBackShadow	; background color
	eor ColorMask
	and DarkMask
	;; runs into the following
.endproc
;;; *** CommonColorDLI
;;; *** resets the graphics mode of GTIA,
;;; *** then installs the background color
.proc	CommonColorDLI
	pha
	lda YPos
	cmp #$68		; too low?
	bcs finearea		; we are in the fine scroll area
	lda GPriorShadow	; get the Prior shadow register
	and #$3f		; mask out GTIA specialities
	sta GPrior		; reset the GTIA
	pla			; get the background color
	sta ColorBack
	pla
	rti
finearea:			; fine scroll area
	pla			; pop background color
	.byte $24		; skip the next pha
	;; runs into the following
.endproc
;;; *** FineDLI
;;; *** This is a custom OS DLI handler
;;; *** that is installed on fine scrolling to
;;; *** hide scrolled-in nonsense.
.proc	FineDLI
	pha
	lda Color2Shadow	; get text background color
	eor ColorMask
	and DarkMask
	sta WSync
	sta Color1		; and hide the text away. Yuck.
	pla
	rti
.endproc
;;; *** ClearScreen
;;; *** Clear the screen contents
	.global ClearScreen
.proc	ClearScreen
	jsr LoadScreenPtr	; load the screen pointer with the GfxOrigin
	ldy ScreenPtr		; start here, keep lower offset consistently zero
	ldx ScreenPtr+1
	lda #$00
	sta ScreenPtr
lp:
	sta (ScreenPtr),y	; erase the screen upwards
	iny
	bne lp
	inx
	stx ScreenPtr+1		; reload me
	cpx RamTop		; up to the end of it
	bcc lp
	;; runs into the following
.endproc
;;; *** Home: Move the cursor to the home position
;;; *** of the screen editor
.proc	Home
	lda #$00
	sta CursorRow
	sta CursorColumn
	sta CursorColumn+1	; reset the cursor position (no margins here, intentionally!)
	rts
.endproc	
;;; *** LoadScreenPtr
;;; *** Load the screen pointer with the GfxOrigin
	.global LoadScreenPtr
.proc	LoadScreenPtr
	lda GfxOrigin
	sta ScreenPtr
	lda GfxOrigin+1
	sta ScreenPtr+1
	rts
.endproc
;;; *** ComputeCursorAddress
;;; *** Compute the address and pixelmasks of the cursor address
	.global ComputeCursorAddress
.proc	ComputeCursorAddress
	ldx #$01
	stx STmp1
	dex
	stx ScreenPtr+1		; reset the screen pointer (Hi)
	
	lda CursorRow		; get the Y position
	
	asl a			; offset low is now in A
	rol ScreenPtr+1		; *2
	asl a
	rol ScreenPtr+1		; *4
	adc CursorRow		; +1 = *5
	bcc nocarry1
	inc ScreenPtr+1		; increment hi
nocarry1:
	ldy GfxMode		; now get the multiplier, depends on the mode
	ldx Multiplier,y	; get the multiplier->x
powerloop:
	asl a
	rol ScreenPtr+1		; increment lo,hi
	dex
	bne powerloop
	sta ScreenPtr		; is low

	lda CursorColumn+1	; get high of the cursor position->Hi
	lsr a			; move into carry
	lda CursorColumn
	ldx Shifter,y		; get the downshifter for the X position
	beq norshift
rshiftloop:
	ror a			; downshift the cursor X position
	asl STmp1		; count the upshift
	dex
	bne rshiftloop
norshift:
	adc ScreenPtr		; add up this to the screen offset
	bcc nocarry2
	inc ScreenPtr+1
nocarry2:
	clc
	adc GfxOrigin		; add the screen offset here
	sta ScreenPtr
	lda ScreenPtr+1
	adc GfxOrigin+1
	sta ScreenPtr+1

	lda ColMasks,y		; get the corresponding column mask value
	and CursorColumn	; mask out the relevant bits from the cursor column
	adc STmp1		; add the offset into the PxMask table
	tay			; from that, get the offset
	lda PxMasks-1,y		; get the pixel mask from that
	sta PixelMask
	sta ShiftMask		; keep the pixel mask here.
	ldy #$00		; reset the offset
	rts
.endproc
;;; row multiplier power = screen modulo divided by five.
;;; 5*2^table entry is the modulo, index by graphics mode
Multiplier:
	.byte 3,2,2,1,1,2,2,3,3,3,3,3,3,3,2,3
;;; rightshift for the X position before it forms a 
;;; screen offset, indexed by graphics mode
Shifter:
	.byte 0,0,0,2,3,2,3,2,3,1,1,1,0,0,3,2
;;; ColMask contains the relevant bits in the cursor column
;;; that are required to get the pixel shift,
;;; also indexed by graphics mode
ColMasks:
	.byte 0,0,0,3,7,3,7,3,7,1,1,1,0,0,7,3
;;; Pixel mask indexed by horizontal cursor position
;;; from zero to seven plus 2^shifter
PxMasks:
	.byte $ff				; shifter zero:	 All bits
	.byte $f0,$0f				; shifter one:	 Select left or right nibble
	.byte $c0,$30,$0c,$03			; shifter two:	 Select groups of two bits
	.byte $80,$40,$20,$10,$08,$04,$02,$01	; shifter three: Select individual bits.
;;; *** NextChar
;;; *** Advance the cursor in the line, wrap around possibly
.proc	NextChar
	inc CursorColumn
	bne nocarry
	inc CursorColumn+1
nocarry:
	jsr TestCursorHorizontal	; is the cursor in range horizontally?
	bcc NextLine			; if not, advance by one line
	rts
.endproc
;;; *** NextLine
;;; *** Advance the cursor by one line, do not scroll
.proc	NextLine
	lda #$00
	sta CursorColumn	; back to position zero
	sta CursorColumn+1
	inc CursorRow
	rts
.endproc
;;; *** ReadFromCursor
;;; *** Read the character under the cursor and return in
;;; *** in the Accu.
	.global ReadFromCursor
.proc	ReadFromCursor
	jsr ComputeCursorAddress ; compute the address of the cursor and return it in ScreenPtr, Set PixelMask, etc...
	lda (ScreenPtr),y	; read the data
	and PixelMask		; mask out the relevant bits
shiftloop:
	lsr ShiftMask		; now shift the accu into place
	bcs isinplace
	lsr a			; shift down
	bne shiftloop		; if equal, then more shifting doesn't generate anything different
isinplace:
	sta ScreenChar		; store this byte
	rts
.endproc
;;; *** ScreenStatus
;;; *** The CIO Status vector of the S:	handler, also
;;; *** ExitStatusScreenHandler
;;; *** Often jumped into:	Load the error code and return
.proc	ExitStatusScreenHandler
.endproc
	.global ScreenStatus
.proc	ScreenStatus
	ldy ScreenError
	lda #$01
	sta ScreenError
	lda ScreenByte
	rts
.endproc
;;; *** ScreenClose
;;; *** This is the close vector for the S: handler
;;; *** interface
	.global ScreenClose
.proc	ScreenClose
	;; The XL Os disables here finescrolling and re-opens the screen.
	;; I don't think this is overly smart as this may cause close
	;; to fail.
	ldy #$01
	rts
.endproc
;;; *** ScreenGet
;;; *** This is the get vector for the S: handler
;;; *** interface
	.global ScreenGet
.proc	ScreenGet	
	tsx
	stx ScreenStack		; keep stack position
	jsr TestCursor		; check for out of bounds conditions
	jsr ReadFromCursor	; read the character under the cursor
	jsr TranslateToAscii	; translate from antic to Ascii set
	jsr NextChar		; move cursor forwards
	jmp ExitStatusScreenHandler ; bail out
.endproc
;;; *** ScreenPut
;;; *** Put a character onto the screen handler
;;; *** This is the put vector for the S: handler
;;; *** interface
	.global ScreenPut
.proc	ScreenPut
	sta ScreenByte		; keep the input character
	tsx
	stx ScreenStack		; keep stack position
	jsr ShieldCursor
	lda ScreenByte
	cmp #$7d		; clear screen control?
	bne notclearscreen
	jsr ClearScreen		; erase the screen if so
	jmp ExitStatusScreenHandler
notclearscreen:
	jsr TestCursor		; test whether we are in range
	lda ScreenByte		; get the character to print
	cmp #$9b		; is it EOL?
	bne noteol
	jsr NextLine		; advance the cursor by one line, perform EOL
	jmp ExitStatusScreenHandler
noteol:
	jsr PutChar		; now place this character in the output
	jsr NextChar		; and now advance by one line
	jmp ExitStatusScreenHandler
.endproc
;;; *** TestCursorHorizontal
;;; *** Test whether the cursor is in range horizontally
;;; *** returns carry clear if not so
.proc	TestCursorHorizontal	
	ldx GfxMode
	lda #0			; hi-res border case
	cpx #8			; not graphics 8
	bne nothires
	lda #1			; otherwise, the border is at one
nothires:
	cmp CursorColumn+1	; if here not equal, then either A>column or A<column. Carry clear if A<column, i.e. error
	bne exit
	lda ModeWidth,x		; here eq and hence carry set
	sbc #1			; get maximum position
	cmp CursorColumn	; if carry set, the cursor column still in range.
exit:
	rts
.endproc
;;; *** TestCursor
;;; *** Test for out of bounds conditions of the cursor
.proc	TestCursor
	jsr TestCursorHorizontal
	bcc bounds
	;; graphics mode still in X register
	lda CursorRow
	cmp ModeHeight,x	; get the height of the corresponding graphics mode
	bcs bounds		; must scroll up the screen
	
	lda #$01		; everything fine here
	sta ScreenError
	ldy #BreakError		; reset the break flag
	ldx BreakFlag
	sty BreakFlag		; does not set flags
	beq seterror
	rts			; done here
bounds:				; out of bounds condition detected
	jsr Home		; place the cursor again at the home position
	lda #OutOfRange		; set the screen
seterror:
	sty ScreenError		; error code
	ldx ScreenStack		; restore stack
	txs			; pointer
	jmp ExitStatusScreenHandler
.endproc	
;;; ModeWidth:	This table contains the
;;; widths in characters of the indexed graphics mode,
;;; at least the low-byte of it.
	.global ModeWidth
ModeWidth:
	.byte <40,<20,<20,<40,<80,<80,<160,<160
	.byte <320,<80,<80,<80,<40,<40,<160,<160
;;; ModeHeight:	This table contains the
;;; heights in characters of the indexed graphics mode
	.global ModeHeight
ModeHeight:	
	.byte 24,24,12,24,48,48,96,96
	.byte 192,192,192,192,24,12,192,192
;;; *** TranslateToAscii
;;; *** Translate the character in the screen handler output buffer
;;; *** from antic to Ascii if we have a test mode
	.global TranslateToAscii
.proc	TranslateToAscii
	lda ScreenChar
	ldy GfxMode
	ldx Shifter,y		; check whether the indicated mode is a text mode and requires translation
	bne gfxmode
	rol a
	rol a
	rol a
	rol a
	and #$03		; relevant bits->A
	tax
	lda ScreenChar		; get the character back
	eor ReadMask,x		; translate it
gfxmode:
	sta ScreenByte		; directly the character
	rts
ReadMask:
	.byte $20,$60,$40,$00
.endproc
XlateMask:
	.byte $40,$20,$60,$00
;;; *** PutChar
;;; *** Place the single character in ScreenByte into the screen memory
	.global	PutChar
.proc	PutChar
wt:	
	lda StartStopFlag
	bne wt			; wait possibly for a ^1
	lda CursorRow
	sta OldCursorRow	; carry the cursor position over
	lda CursorColumn
	sta OldCursorColumn
	lda CursorColumn+1
	sta OldCursorColumn+1	; keep me for fill,drawto operations
	;; now translate the character from ASCII into the internal representation
	lda ScreenByte
	ldy GfxMode
	ldx Shifter,y		; check whether the indicated mode is a text mode and requires translation
	bne gfxmode
	rol a
	rol a
	rol a
	rol a
	and #$03		; relevant bits->A
	tax
	lda ScreenByte		; get the character back
	eor XlateMask,x		; translate it
gfxmode:
	;; runs into the following
.endproc
;;; *** StoreToCursor
;;; *** Store the contents of the accumulator
;;; *** onto the cursor address
	.global StoreToCursor
.proc	StoreToCursor
	sta ScreenChar		; keep the screen data
	jsr ComputeCursorAddress
	lda ScreenChar		; get it again
shift:				; shift the data now in place
	lsr ShiftMask
	bcs shiftdone
	asl a
	bne shift		; if it is equal then, further shifting doesn't improve it either...
shiftdone:
	eor (ScreenPtr),y	; now mask the data in
	and PixelMask		; mask out the relevant pixels
	eor (ScreenPtr),y	; zero'd bits from above are now the original data, 
				; remaining bits have been filtered thru a double exor and are hence the data to insert 
	sta (ScreenPtr),y
	rts
.endproc
;;; *** ScreenSpecial
;;; *** The special handler of the S: handler
;;; *** line drawing and filling.
	.global ScreenSpecial
.proc	ScreenSpecial
	tsx
	stx ScreenStack		; keep stack position
	ldx #$00		; fill flag
	lda ZCmd		; check which command we have here.
	cmp #$11		; draw line?
	beq godrawer
	inx			; possibly the filler?
	cmp #$12
	beq godrawer		; this is the drawer
	ldy #InvalidCmd		; an error: This command does not exist
	rts
godrawer:
	jsr ShieldCursor
	stx FillFlag		; store the flag here
	lda CursorRow
	sta NewCursorRow	; keep it
	lda CursorColumn
	sta NewCursorColumn
	lda CursorColumn+1
	sta NewCursorColumn+1	; ditto

	lda #$01		; reset the signs to +1
	sta DeltaRowSign
	sta DeltaColumnSign

	;; Now compute deltas and signs
	sec
	lda NewCursorRow
	sbc OldCursorRow
	sta DeltaRow
	bcs rowpositive
	lda #$ff		; row sign is negative
	sta DeltaRowSign
	eor DeltaRow		; invert this
	adc #1			; carry is cleared:	two's complement
	sta DeltaRow
rowpositive:
	sec
	lda NewCursorColumn
	sbc OldCursorColumn
	sta DeltaColumn
	lda NewCursorColumn+1
	sbc OldCursorColumn+1
	sta DeltaColumn+1
	bcs columnpositive
	lda #$ff		; column sign is negative
	sta DeltaColumnSign
	eor DeltaColumn+1
	sta DeltaColumn+1
	lda #$ff
	eor DeltaColumn
	adc #1			; carry is clear
	sta DeltaColumn		; one's complement
	bcc columnpositive
	inc DeltaColumn+1
columnpositive:
	;; initialize registers now
	ldy #0
	ldx #2			; three registers total
initlp:
	lda OldCursorRow,x	; start here
	sta CursorRow,x		; add up
	sty RowAccu,x		; clear the accumulators
	sty ColumnAccu,x	; ditto (clears a bit more in deltamax, no matter)
	;.byte $94,RowAccu	; ca65 bug
	dex
	bpl initlp
	;; now compute whether DeltaX or DeltaY is larger
	inx			; reset X back to zero
	ldy DeltaColumn
	lda DeltaColumn+1	; assume deltaX is larger
	sta Counter+1		; the larger of the two
	sta DeltaMax+1
	bne islarger
	cpy DeltaRow		; ditto
	bcs islarger
	ldy DeltaRow		; this is the larger of the two
	ldx #$02		; offset to the larger
	;; no need to clear the high byte here, it is already clear by the above condition
islarger:
	sty DeltaMax		; keep as counter register: Length of the line
	sty Counter		; is also the length of the line
	tya			; keep low
	pha
	lda Counter+1		; get high again
	lsr a			; set the carry
	pla
	ror a			; half this register
	sta RowAccu,x		; make this non-zero
drawloop:
	;; check whether the line is over
	lda Counter
	ora Counter+1
	bne continue
	jmp ExitStatusScreenHandler	; bail out if done
continue:	
	;; Row incrementer now
	clc
	lda RowAccu
	adc DeltaRow
	sta RowAccu
	bcc nocarry1
	inc RowAccu+1
nocarry1:
	lda RowAccu
	cmp DeltaMax
	lda RowAccu+1
	sbc DeltaMax+1
	bcc noyincrement	
	;; here: advance Y
	clc
	lda CursorRow
	adc DeltaRowSign
	sta CursorRow
	ldx #$00
	jsr SubtractMax		; compute rowaccu -= max
noyincrement:
	;; column incrementer now
	clc
	lda ColumnAccu
	adc DeltaColumn
	sta ColumnAccu
	lda ColumnAccu+1
	adc DeltaColumn+1
	sta ColumnAccu+1
	
	lda ColumnAccu
	cmp DeltaMax
	lda ColumnAccu+1
	sbc DeltaMax+1
	bcc noxincrement
	;; here: advance X
	bit DeltaColumnSign	; positive or negative?
	bpl incrementx
	lda CursorColumn
	bne nocarry2
	dec CursorColumn+1
nocarry2:
	dec CursorColumn
	bcs xdone
incrementx:
	inc CursorColumn
	bne xdone
	inc CursorColumn+1
xdone:
	ldx #$02
	jsr SubtractMax		; compute columnaccu -= max
noxincrement:
	jsr TestCursor		; are we still in range?
	jsr PutChar		; plot the pixel
	lda FillFlag		; are we filling?
	beq nofill
	jsr FillRight		; fill to the right of this line
nofill:				; here:	continue with the line drawer
	lda Counter		; carry?
	bne nocarry3
	dec Counter+1
nocarry3:
	dec Counter
	jmp drawloop		; continue until done
	
;;; private helper of the line drawer:
;;; subtract the max from the accu indexed by x
SubtractMax:	
	sec
	lda RowAccu,x
	sbc DeltaMax
	sta RowAccu,x
	lda RowAccu+1,x
	sbc DeltaMax+1
	sta RowAccu+1,x
	rts
;;; private helper of the line drawer/area filler:
;;; fill from this position to the right
FillRight:
	lda CursorRow
	pha
	lda CursorColumn
	pha
	lda CursorColumn+1
	pha			; saveback the drawing registers
	lda ScreenByte		; get fill color
	pha
fillloop:
	lda CursorRow
	pha
	jsr NextChar		; to the next cursor position
	pla
	sta CursorRow
	jsr TestCursor		; FIXME: stack is not organized
	jsr ReadFromCursor	; get the color under the cursor
	cmp #$00		; do we have the background color?
	bne exitfill		; abort filling at the end of the row
	lda FillColor
	sta ScreenByte		; set the fill color
	jsr PutChar		; and write out the color
	clc
	bcc fillloop
exitfill:
	;; now restore the settings
	pla
	sta ScreenByte
	pla
	sta CursorColumn+1	; restore the drawing registers
	pla
	sta CursorColumn
	pla
	sta CursorRow
	rts
.endproc
;;; *** Screen handler init routine, used for CIO
;;; *** and the reset handler.
	.global ScreenInit
.proc	ScreenInit
	lda RamSize
	sta RamTop		; initialize ramtop
	rts
.endproc
;;; *** ShieldCursor:	Remove the cursor so we can savely draw on it
.proc	ShieldCursor
	lda WindowHeight	; get # lines in the text window
	cmp #24			; only if there is a window
	bcc windowed		; if there is a window, do not bother
	jsr RemoveCursor
windowed:
	rts
.endproc
	
