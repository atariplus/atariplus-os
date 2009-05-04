;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: keyboard.asm,v 1.12 2008-12-29 23:37:23 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the E: handler		**
;;; **********************************************************************

	.include "keyboard.i"
	.include "editor.i"
	.include "screen.i"
	.include "errors.i"
	.include "nmi.i"
	.include "irq.i"
	.include "gtia.i"
	.include "antic.i"
	.include "cio.i"
	
        .segment  "OsHi"
;;; *** Keyboard open vector for CIO
;;; *** This is called by CIO
	.global KeyboardOpen
.proc	KeyboardOpen
.endproc
	;; runs into the following
;;; *** Keyboard close vector for CIO
;;; *** ditto
	.global KeyboardClose
.proc	KeyboardClose
.endproc
	;; runs into the following
;;; *** KeyboardStatus:	CIO status vector of the keyboard
;;; *** provides status information about us (which is fine)
	.global KeyboardStatus
.proc	KeyboardStatus
.endproc
	ldy #$01		; worked
;;; *** KeyboardPut:	CIO vector for writing
;;; *** into the keyboard which is unsupported (clearly)
;;; *** returns with the error code in Y CIO placed here
	.global KeyboardPut
.proc	KeyboardPut
.endproc	
;;; *** KeyboardSpecial:	CIO vector for extended commands
;;; *** which are not present here. Return with the CIO
;;; *** supplied error code
	.global KeyboardSpecial
.proc	KeyboardSpecial
	rts
.endproc
;;; *** Keyboard get vector of the CIO
;;; *** read one character from the keyboard
;;; *** deliver this in the accumulator.
	.global KeyboardGet
.proc	KeyboardGet
	lda #$00
	sta SuperFlag		; not yet an extended vector found
	lda ZAux1		; check the open mode
	lsr a			; "auto return mode" selected? (Actually, service for E:)
	lda #$9b		; default return code:	EOL for auto-return
	bcs exit		; if so, then just run "return" all over.
WaitLoop:	
	ldy #BreakError		; if BRK has been hit, so deliver this
	ldx BreakFlag
	beq break
	lda KeyCodeShadow	; get the currently active keyboard code
	cmp #$ff		; no key pressed?
	beq WaitLoop
	;; sta KeyCodeHold	; not required
	ldx #$ff		; clear this flag
	stx KeyCodeShadow
	ldx NoClick		; clicking enabled or disabled?
	bne noclick
	jsr KeyboardClick	; generate the click
noclick:
ReDecode:
	tay
	cmp #$c0		; with shift and control? If so, click, but ignore (wierd!)
	bcs WaitLoop
	lda (KeyDef),y		; get the keyboard definition for that key now
	bmi Specials		; branch for special abilities
RegularKeys:
	;; here: regular keys
	cpy #$40		; with any modifiers pressed?
	bcs hasmodifier
	;; here: unmodified
	cmp #'a'		; small print?
	bcc hasmodifier
	cmp #'z'+1
	bcs hasmodifier
	ldx ShiftLock		; is it shifted?
	beq hasmodifier
	tya			; here shiftlock active: Or this modifier in
	ora ShiftLock
	bne ReDecode		; and try again
hasmodifier:	
	jsr CheckCtrlCode	; check whether this key is a control code and hence not invertible
	beq exit		; if so, we must bail out
	eor InverseMask		; otherwise, invert the key
exit:
	ldy #1			; worked fine
bailout:
	sta ScreenByte		; some programs expect this here
	rts
break:
	lda #$9b
	sty BreakFlag
	bne bailout
;;; handling of special keyboard codes
Specials:
	cmp #KeyNop		; this key does nothing
	beq branchback		; get the next key, ignore this (also has now C=1)
	cmp #KeyInverse		; inverse (Atari) key pressed?
	bne notatari
	lda InverseMask
	eor #$80		; invert characters possibly
	sta InverseMask
	bcs branchback
notatari:
	cmp #KeyCaps		; caps key pressed?
	bne notcaps
	lda ShiftLock		; are we locked?
	beq noshiftlock
	lda #$00		; if so, then clear locking
	beq setshiftlock
notcaps:
	cmp #KeyHiCaps		; Caps + Shift?
	bne nothicaps
noshiftlock:
	lda #$40		; enable shifting
	bne setshiftlock
nothicaps:
	cmp #KeyCtrlCaps	; Caps + Ctrl?
	bne noctrlcaps
	lda #$80		; enable Ctrl'ng
setshiftlock:
	sta ShiftLock
	jmp WaitLoop
noctrlcaps:
	cmp #KeyEOF		; signal EOF?
	bne noEOF
	lda #$9b		; return EOL, but with failure
	ldy #EndOfFile
	bmi bailout		; and leave us
noEOF:
	cmp #KeyToggleClick	; keyboard click toggle
	bne notoggleclick
	lda #$ff
	eor NoClick
	sta NoClick
	bne branchback		; branch immediately if disabled (then clicked already)
	jsr KeyboardClick	; generate the click now (didn't happen before)
branchback:	
	jmp WaitLoop
notoggleclick:
	cmp #KeyCursorHome	; extended keys of the 1200? (not supported)
	bcc branchback
notfunctionkeys:
	cmp #LastSpecialKey	; anything special left?
	bcs regular		; if not, this is a regular key
	sbc #KeyCursorHome-$1c-1 ; carry cleared. Map this to the cursor movement, but...
	inc SuperFlag		; remember that there's something special about it.
	bne exit
regular:
	jmp RegularKeys
.endproc
;;; *** Keyboard click routine
;;; *** This subroutine generates the keyboard clicks
;;; *** by means of the console speaker.
;;; *** MUST NOT modify the accumulator
	.global KeyboardClick
.proc	KeyboardClick
	ldx #$7e		; delay counter
clicklp:
	stx Console		; into the console speaker
	ldy YPos		; delay loop without using WSync (do not break DLI's!)
waity:	
	cpy YPos
	beq waity
	dex
	dex			; these were actually two y positions, but keep the frequency!
	bpl clicklp
	rts
.endproc
;;; *** Keyboard handler init routine, used for CIO
;;; *** and the reset handler
	.global KeyboardInit
.proc	KeyboardInit
	lda #$ff
	sta KeyCodeShadow	; reset the keyboard code
	sta LastKey
	lda #$40
	sta ShiftLock		; reset the keyboard modifier
	lda #<KeyboardTable
	sta KeyDef		; store the keyboard definition table
	lda #>KeyboardTable
	sta KeyDef+1
	rts
;;; *** KeyboardTable
;;; *** The os supplied keyboard table
KeyboardTable:
	;; unshifted
	.byte 'l','j',';',KeyNop,KeyNop,'k','+','*'
	.byte 'o',KeyNop,'p','u',KeyEOL,'i','-','='
	.byte 'v',KeyNop,'c',KeyNop,KeyNop,'b','x','z'
	.byte '4',KeyNop,'3','6',$1b,'5','2','1'
	.byte ',',' ','.','n',KeyNop,'m','/',KeyInverse
	.byte 'r',KeyNop,'e','y',KeyTab,'t','w','q'
	.byte '9',KeyNop,'0','7',KeyBS,'8','<','>'
	.byte 'f','h','d',KeyNop,KeyCaps,'g','s','a'
	;; shifted
	.byte 'L','J',':',KeyNop,KeyNop,'K',$5c,'^'
	.byte 'O',KeyNop,'P','U',KeyEOL,'I','_','|'
	.byte 'V',KeyNop,'C',KeyNop,KeyNop,'B','X','Z'
	.byte '$',KeyNop,'#','&',$1b,'%',$22,'!'
	.byte '[',' ',']','N',KeyNop,'M','?',KeyInverse
	.byte 'R',KeyNop,'E','Y',KeySetTab,'T','W','Q'
	.byte '(',KeyNop,')',$27,KeyDelLine,'@',KeyClear,KeyInsertLine
	.byte 'F','H','D',KeyNop,KeyHiCaps,'G','S','A'
	;; with control
	.byte $0c,$0a,$7b,KeyNop,KeyNop,$0b,$1e,$1f
	.byte $0f,KeyNop,$10,$15,KeyEOL,$09,$1c,$1d
	.byte $16,KeyNop,$03,KeyNop,KeyNop,$02,$18,$1a
	.byte KeyNop,KeyNop,KeyEOF,KeyNop,$1b,KeyNop,$fd,KeyNop ; $fd is the buzzer
	.byte $00,$20,$60,$0e,KeyNop,$0d,KeyNop,KeyInverse
	.byte $12,KeyNop,$05,$19,KeyCtrlTab,$14,$17,$11
	.byte KeyNop,KeyNop,KeyNop,KeyNop,$fe,KeyNop,KeyClear,$ff ; insert,delete char are regular ATASCII codes
	.byte $06,$08,$04,KeyNop,KeyCtrlCaps,$07,$13,$01
.endproc
