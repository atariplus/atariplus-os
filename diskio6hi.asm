;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: diskio6hi.asm,v 1.10 2013/06/02 20:41:04 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DiskIO Resident Part - High memory area	**
;;; **********************************************************************

	.include "kernel.i"
	.include "cio.i"
	.include "reset.i"
	.include "editor.i"
	.include "errors.i"
	.include "screen.i"
	.include "nmi.i"
	.include "irq.i"
	.include "fms.i"
	.include "diskio6hi.i"
	

;;; *** The resident part that hides behind the
;;; *** cartridge starts here. goes to $ac00 and above

	.segment "diskiohi"
	
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

;;; *** Entry points for the low-part
;;; *** These are used by the installer and the low-part,
;;; *** and the installer patches the lo-part to jump to
;;; *** the right spot
HiStart		=	*

ReInstallOffset	=	*-HiStart
	jmp ReInstall		; the reset entry

DiskIOGetOffset	=	*-HiStart
	jmp DiskIOGetFunction	; the new editor GET routine

EnableReturnOffset	=	*-HiStart
EnableReturn:	
	jmp $ffff	       ; will be patched by installer

CallCIOOffset		=	*-HiStart
CallCIO:
	jmp $ffff		;will be patched by installer

OldInitOffset		=	*-HiStart
OldInit:
	jmp $ffff		;calls the old Init vector
	
	;; Type of the cartridge found, $22 for Basic, $23 for Mac/65
QuoteTypeOffset	=	*-HiStart
QuoteType:	.byte 0
	
	;; The new MemLo address
MemLoOffset	=	*-HiStart
MemLoAdjust:	.word 0

	;; The new HaTabs value to be patched in
HaTabsVectorOffset	=	*-HiStart
HaTabsVector:	.word 0

	;; The new Editor GET address
NewGetVectorOffset	=	*-HiStart
NewGetVector:	.word 0

DirBufferVectorOffset	=	*-HiStart
DirBufferVector:.word 0

;;; *** The reset routine of DiskIO - re-run it here
;;; ReInstall
.proc	ReInstall
	jsr ReserveMemory
	jsr ResetGlobals

	bit TrueReset
	bpl isinit    		; called for installation?
	
	jsr OldInit		;call the old Init vector - potentially runs DUP

isinit:
	lda #$ff
	sta TrueReset

	bit FmsBootFlag
	bvs notfound		;do not install if running to dos
	
	ldy TitleVector
	lda TitleVector+1
	ldx #0
	jsr CIOSetBuffer
	lda #CmdPutBlock
	sta IOCBCmd,x
	lda #<TitleLen
	sta IOCBLen,x
	lda #>TitleLen
	sta IOCBLen+1,x
	jsr CIOVector	;leave cart disabled!
	;; Find the editor vector
	ldy #0
findeditor:
	lda HaTabs,y
	beq notfound
	cmp #'E'
	beq found
	iny
	iny
	iny
	bne findeditor
	beq notfound
found:
	;; install the new HaTabs vector
	lda HaTabsVector
	sta ZAdr
	sta HaTabs+1,y
	lda HaTabsVector+1
	sta ZAdr+1
	sta HaTabs+2,y
	;; copy the editor vector over
	ldy #11
copyedevice:
	lda EditorTable,y
	sta (ZAdr),y
	dey
	bpl copyedevice
	;; install the new get vector, this will go to our device
	ldy #4
	lda NewGetVector
	sta (ZAdr),y
	lda NewGetVector+1
	iny
	sta (ZAdr),y
notfound:
	;; Update MemLo
	jsr ReserveMemory
	;; reset statics
	rts
.endproc
;;; *** Adjust MemLo such that DiskIO is safe.
;;; ReserveMemory
.proc ReserveMemory
	lda MemLo
	cmp MemLoAdjust
	lda MemLo+1
	sbc MemLoAdjust+1
	bcs nomemlo
	lda MemLoAdjust
	sta MemLo
	lda MemLoAdjust+1
	sta MemLo+1
nomemlo:
	rts
.endproc
;;; *** Reset the global vectors of the resident part
;;; ResetGlobals
.proc	ResetGlobals
	lda #0
	sta InputCounter
	rts
.endproc
;;; *** Read data from the editor, parse off as DiskIO input
;;; DiskIOGetFunction
.proc	DiskIOGetFunction
	sty ScreenError
	ldy ZIOCB
	bne notzero
	ldy IOCBAdr
	cpy #<CmdBuffer
	bne notzero
	ldy IOCBAdr+1
	cpy #>CmdBuffer
	bne notzero
	cmp #$9b		;EOL?
	beq eol
	inc InputCounter
notzero:
	ldy ScreenError
	jmp EnableReturn
eol:
	txa
	pha
	;; store zero-page variables
	ldx #ZPageLen-1
saveloop:
	lda ZPage,x
	pha
	dex
	bpl saveloop

	;; Start processing. Did the user press the help key?
	jsr CheckHelp
	bne clear
	
	lda InputCounter
	beq exit
	sta LineLen
	lda #0
	sta InputCounter
	sta AcceptOK		;accept without asking.
	sta DirectFlag		;no ESCape

	jsr FindCommand
	beq exit		;if not a known command, exit

	jsr CallCommand		;call the command
	bcc return		
clear:				;C=1: Do not forward the input to Basic
	jsr ClearInput
return:
	;; restore the ZIOCB
	ldx #$b
restio:
	lda IOCBIndex,x
	sta ZIndex,x
	dex
	bpl restio
	
	lda #<CmdBuffer
	clc
	adc LineLen
	sta ZAdr
	lda #>CmdBuffer
	adc #0
	sta ZAdr+1
	lda #CmdGetRecord
	sta ZCmd
	lda LineLen
	sta ZLen
	lda #0
	sta ZIOCB
	sta ZLen+1
exit:
	ldx #-ZPageLen
restore:
	pla
	sta ZPage+ZPageLen,x
	inx
	bmi restore
	pla
	tax
	ldy ScreenError
	lda #$9b		;returns an EOF
	jmp EnableReturn
.endproc
;;; *** 
;;; *** Find the next command in the command table
;;; *** FindNext
.proc FindNext
	dey
loop:	iny
	lda (CmdPtr),y
	bpl loop
	iny
	iny
	iny			; also include the pointer
	tya
	jsr IncrementCmdPtr
	rts
.endproc
;;; *** FindCommand
;;; *** Find the command in the command buffer
;;; *** Returns with Z = 1 if not found
.proc	FindCommand
	lda CommandVector
	sta CmdPtr
	lda CommandVector+1
	sta CmdPtr+1
	bne checkcmd
cmdloop:
	jsr FindNext
checkcmd:
	ldy #$ff
findloop:
	iny
	cpy LineLen
	bcs cmdloop		;cannot be this command
	lda (CmdPtr),y		;end of the command list?
	cmp #$ff
	beq exit
	;; compare with the input, which goes to $580 (need to check!)
	and #$7f		; ignore the end of command
	cmp CmdBuffer,y
	bne cmdloop		; not this command
	lda (CmdPtr),y		; end of the command?
	bpl findloop
	iny
	sty CmdLength
exit:	
	rts
.endproc
;;; *** Call the command from the command table
;;; *** CallCommand
;;; *** Must return with C=1 if the command buffer shall be cleared
.proc	CallCommand
	iny
	lda (CmdPtr),y		; get the command handle: lo-byte
	pha
	dey
	lda (CmdPtr),y
	pha
	rts
.endproc
;;; *** ClearInput
;;; *** Erase the input, delivering either an error or an empty line to
;;; *** the cartridge
.proc ClearInput
	ldy #0
	lda #'?'
	sta CmdBuffer,y
	lda #';'
	sta CmdBuffer+1,y
	iny
	iny
	sty LineLen
	sty CmdLength
	lda #$22
	cmp QuoteType		;basic?
	beq exit
	lda #EndOfFile		;signal an EOF for Mac/65, which is then silently ignored instead
	sta ScreenError
	lda #0
	sta LineLen
exit:
	rts
.endproc
;;; *** Command vectors: If they return with C=1, then the command is not forwarded
;;; *** to BASIC or MAC/65. If they return with C=0, then the command buffer is
;;; *** forwarded.
;;; 
;;; *** DiskIO Commands
Commands:
	.byte "DI",'R'+$80
	.word DirCmd-1

	.byte ".",'D'+$80
	.word Delete1Cmd-1
	.byte "DEL",'.'+$80
	.word Delete2Cmd-1
	.byte "DELET",'E'+$80
	.word Delete3Cmd-1

	.byte ".",'K'+$80
	.word Lock1Cmd-1
	.byte "LOC",'K'+$80
	.word Lock2Cmd-1

	.byte ".",'U'+$80
	.word Unlock1Cmd-1
	.byte "UN",'.'+$80
	.word Unlock2Cmd-1
	.byte "UNLOC",'K'+$80
	.word Unlock3Cmd-1

	.byte ".",'N'+$80
	.word Rename1Cmd-1
	.byte "RE",'.'+$80
	.word Rename2Cmd-1
	.byte "RENAM",'E'+$80
	.word Rename3Cmd-1

	.byte ".",'E'+$80
	.word Enter1Cmd-1
	.byte "E",'.'+$80
	.word Enter2Cmd-1
	.byte "ENTE",'R'+$80
	.word Enter3Cmd-1

	.byte ".",'L'+$80
	.word Load1Cmd-1
	.byte "LO",'.'+$80
	.word Load2Cmd-1
	.byte "LOA",'D'+$80
	.word Load3Cmd-1

	.byte ".",'R'+$80
	.word Run1Cmd-1
	.byte "RUN",'D'+$80
	.word Run2Cmd-1

	.byte ".",'S'+$80
	.word Save1Cmd-1
	.byte "S",'.'+$80
	.word Save2Cmd-1
	.byte "SAV",'E'+$80
	.word Save3Cmd-1

	.byte ".",'X'+$80
	.word List1Cmd-1
	.byte "LI",'.'+$80
	.word List2Cmd-1
	.byte "LIST",'D'+$80
	.word List3Cmd-1

	.byte "L",'.'+$80
	.word ListProgram1Cmd-1
	.byte "LIS",'T'+$80
	.word ListProgram2Cmd-1
	
	.byte ".",'B'+$80
	.word Bload1Cmd-1
	.byte "BL",'.'+$80
	.word Bload2Cmd-1
	.byte "BLOA",'D'+$80
	.word Bload3Cmd-1

	.byte "HEL",'P'+$80
	.word Help1Cmd-1
	
	.byte ".",'C'+$80
	.word Move1Cmd-1
	.byte "CO",'.'+$80
	.word Move2Cmd-1
	.byte "COP",'Y'+$80
	.word Move3Cmd-1

	.byte $ff
CommandVector:	.word Commands
;;; *** Close IOCB #5
;;; Close5
.proc	Close5
	ldx #$50
	lda #CmdClose
	sta IOCBCmd,x
	jsr CallCIO
	rts
.endproc
;;; *** List the directory contents on the screen
;;; DirCmd
.proc	DirCmd
	jsr Close5
	lda #$02
	sta LeftMargin
	sta HeadLine		;ensure the headline buffer is empty
	lda #'D'
	sta CmdBuffer
	lda #':'
	sta CmdBuffer+1
	lda #'-'
	sta CmdBuffer+2
	lda #$9b
	sta CmdBuffer+3		;use a buffer that is visible to CIO
	ldy #<CmdBuffer
	lda #>CmdBuffer
	jsr CIOSetBuffer
	lda #CmdOpen
	ldy #6
	jsr CIOExec
	tya
	bpl ok
	jmp Error
ok:
nextscreen:	
	jsr PrepSort
	lda #$00
	sta FreeSecsFlag
	sta DirEnumerator

	jsr DirBufferToCmdPtr
	ldy #0
	
	lda #$7d		; clear screen
	sta (CmdPtr),y
	iny
	lda #$1d		; cursor down
	sta (CmdPtr),y
	iny
	lda #$9b		; EOL
	sta (CmdPtr),y
dirloop:
	;; print the above control characters, or the
	;; pair of entries just loaded.
	ldy DirBufferVector
	lda DirBufferVector+1
	jsr PrintAY
	lda FreeSecsFlag
	bne notfirst

	jsr DirBufferToCmdPtr
	ldy #0
	
	lda #$1c		;cursor up
	sta (CmdPtr),y
	iny
	sta (CmdPtr),y		;twice
	iny
	lda #$9b
	sta (CmdPtr),y		;goes up one line in total
	jsr CIOVector
notfirst:
	jsr SortDirectory
	jsr IfContinue
	bcs nextscreen		;start the next directory
loop2:
	;; insert the entry number in front of the directory entry
	jsr DirBufferToCmdPtr
	
	sed
	clc
	lda DirEnumerator
	adc #$01
	sta DirEnumerator
	cld
	and #$01		;odd entry?
	bne odd
	lda #20			;go to the right side of the screen.
	jsr IncrementCmdPtr
odd:
readdirentry:
	;; read the directory
	ldx #$50
	ldy CmdPtr
	lda CmdPtr+1
	jsr CIOSetBuffer
	ldy #20
	lda #CmdGetRecord
	jsr CIOExec
	tya
	bpl entryok
	cpy #EndOfFile
	beq dirend
	tya
	jmp Error
dirend:	
	jsr Close5
	jsr PrintHeadline
	;; ignore the command from the BASIC/Assembler side and return
	sec
	rts
entryok:
	jsr FilterEntry		;is this the headline?
	bcs readdirentry
	
	inc FreeSecsFlag	;counts file entries
	ldy #13
	lda (CmdPtr),y
	cmp #' '		;end of directory-free sectors?
	bne dirloop

	dec FreeSecsFlag

	;; Insert the file index
	jsr InsertDirEnumerator

	lda DirEnumerator
	lsr a
	bcc jdirloop
	;; odd entry
	jsr DirBufferToCmdPtr
	ldy #$12
	lda #' '
	sta (CmdPtr),y
	iny
	sta (CmdPtr),y
	jmp loop2
jdirloop:
	jmp dirloop
.endproc
;;; *** InsertDirEnumerator
;;; *** Insert the current file index in decimal to CmdPtr
.proc	InsertDirEnumerator
	ldy #18
moveup:
	dey
	lda (CmdPtr),y
	iny
	sta (CmdPtr),y
	dey
	bne moveup
	iny
	lda (CmdPtr),y
	iny
	sta (CmdPtr),y		; move protection state to entry 2, leaving two for the index
	
	ldy #$00
	lda DirEnumerator
	and #$f0
	bne noblank
	lda #' '		;first digit is zero, replace by blank
	sta (CmdPtr),y
	bne cont
noblank:
	lsr a
	lsr a
	lsr a
	lsr a
	ora #'0'
	sta (CmdPtr),y
cont:
	;; insert the low digit
	iny
	lda DirEnumerator
	and #$0f
	ora #'0'
	sta (CmdPtr),y
	rts
.endproc
;;; *** FilterEntry
;;; *** Find out whether a directory entry is a headline.
;;; *** Filter it out if so. Returns C=1 if this is a headline.
.proc	FilterEntry
	ldy #2
	lda (CmdPtr),y
	clc
	bpl exit
loop:
	lda (CmdPtr),y
	cmp #$9b
	beq eol
	and #$7f
	;; convert to screen code
	cmp #$60
	bcs inverse
	sbc #$1f
	bcs inverse
	adc #$60
inverse:
	ora #$80
eol:
	sta HeadLine-2,y	;y starts with 2
	beq abort		;Z still set for EOL
	iny
	bne loop
abort:
	sec
exit:
	rts
.endproc
;;; *** Execute a special command in A
;;; *** If this returns with C=1, the user aborted
;;; SpecialXIO
.proc 	SpecialXIO
	sta XIOCmd
	ldy CmdLength
	jsr BuildCartCmd
	jsr AskUser
	bcs exit
accepted:
	inc CmdLength
	ldx #$20
	;; terminate the file name at its end.
	lda #$9b
	ldy LineLen
	sta CmdBuffer,y
	sec
	tya
	sbc CmdLength		;what's part of the command
	clc
	lda #<CmdBuffer
	adc CmdLength

	ldx #$50
	tay
	lda #>CmdBuffer
	adc #$00
	jsr CIOSetBuffer
	;; the IOCB
	ldy XIOCmd
	lda #$00
	cpy #CmdBload
	bne nobload
	lda #$c0
nobload:
	sta IOCBAux1,x
	tya
	cmp #CmdOpen
	bne notopen
	ldy #4			; open for read
notopen:
	jsr CIOExec
	tya
	bpl ok
	tya
	jmp Error
ok:
	clc
exit:	
	rts
.endproc
;;; *** PrintHeadline
;;; *** Insert the headline into the screen
.proc 	PrintHeadline
	bit HeadLine
	bpl exit
	ldx #$00
	ldy #$02
	lda #$80
clrloop:
	sta (GfxOrigin),y
	iny
	cpy #40
	bne clrloop
	ldy #$0d
insloop:
	lda HeadLine,x
	cmp #$9b
	beq exit
	sta (GfxOrigin),y
	inx
	iny
	bne insloop
exit:
	rts
.endproc
;;; Wait for a keypress, return the key code in A, C=1 on abort
;;; WaitForKey
.proc	WaitForKey
	ldx #$ff
	stx KeyCodeShadow
	stx BreakFlag
wt:	bit BreakFlag
	bpl aborted
	cpx KeyCodeShadow
	beq wt
	lda KeyCodeShadow
	stx KeyCodeShadow
	clc
	rts
aborted:
	ldy AbortedVector
	lda AbortedVector+1
	jsr PrintAY
	lda AbortedVector
	clc
	adc #1
	tay
	lda AbortedVector+1
	adc #0
	jsr PrintAY
	sec
	rts
.endproc
;;; Check whether the screen is full, returns with C=1 if the screen is full.
;;; IfContinue
.proc	IfContinue
	lda DirEnumerator
	cmp #$36		; this is BCD, thus 36 (decimal) entries (18 rows full)
	bcc exit
	jsr PrintHeadline	;insert what we have has headline
	ldy ContinueTextVector
	lda ContinueTextVector+1
	jsr PrintAY
	jsr WaitForKey
	bcs other
	pha

	ldy ClearLineTextVector
	lda ClearLineTextVector+1
	jsr PrintAY

	pla
	cmp #12			;Return?
	beq cont
	cmp #28			;escape?
	bne other
other:
	pla			;drop the return address from the directory caller
	pla
cont:
	sec
exit:
	rts
.endproc
;;; *** ListProgram2Cmd
;;; *** Implement the list command, append the last line number if not given
.proc	ListProgram2Cmd
	Ldy LineLen
	lda CmdBuffer-1,y
	cmp #','		;ends with a ,?
	bne notincomplete
	;; yes
	ldx #0
insloop:
	lda ListEnd,x
	beq end
	sta CmdBuffer,y
	inx
	iny
	inc LineLen
	bne insloop
end:
notincomplete:
	clc			;insert the output
	rts
.endproc
;;; *** Commands taking one argument - in the complete form
.proc	Delete3Cmd
	lda #CmdDelete
	Skip2
.endproc
.proc	Lock2Cmd
	lda #CmdLock
	Skip2
.endproc
.proc	Unlock3Cmd
	lda #CmdUnlock
	Skip2
.endproc
.proc	Rename3Cmd
	lda #CmdRename
	Skip2
.endproc
.proc	Bload3Cmd
	lda #CmdBload
	jsr SpecialXIO
	sec			; ignore the input
	rts
.endproc
;;; *** Shorter versions of commands taking an unquoted argument
;;; ***
Delete2Cmd:	
Unlock2Cmd:	
Rename2Cmd:	
Enter2Cmd:	
Load2Cmd:	
Save2Cmd:	
List2Cmd:	
ListProgram1Cmd:	
Bload2Cmd:
Move2Cmd:
	;; runs all in here
;;; *** HandleShortCommand
;;; *** Handle a command in the abbreviated version
;;; *** The point here is that this eventually inserts the full version
;;; *** into the command buffer so BASIC/Mac/65 can read it.
.proc	HandleShortCommand
	ldy #$0
	jsr FindNext		;advance to the full version
	ldy CmdLength
	dey			;at the DOT to be replaced
loop:	
	lda (CmdPtr),y
	and #$7f		;remove the end marker
	sta CmdBuffer,y		;insert the full command
	lda (CmdPtr),y
	bmi end
	iny
	jsr Insert		;make room for the command
	inc CmdLength
	bpl loop
end:
	;; call the vector for the full command
	iny
	iny
	lda (CmdPtr),y
	pha
	dey
	lda (CmdPtr),y
	pha
	rts
.endproc
;;; Enlarge the output buffer at offset Y by one character
;;; Insert
.proc	Insert
	sty CmdLengthStore
	inc LineLen
	ldy LineLen
insertloop:
	dey
	lda CmdBuffer,y
	sta CmdBuffer+1,y
	cpy CmdLengthStore
	bne insertloop
	lda #' '
	sta CmdBuffer,y
	rts
.endproc
;;; *** Short commands taking one additional character
;;; *** That needs to be removed
.proc	Run2Cmd
.endproc
.proc	List3Cmd
	ldy CmdLength
	lda #' '
	sta CmdBuffer-1,y
	;; runs into the following
.endproc
;;; *** Additional full commands that do not work via XIO,
;;; *** but insert a user command
Save3Cmd:	
Load3Cmd:	
Enter3Cmd:
;;; *** Cartridge based commands
;;; *** These commands insert cartridge commands into the CIO buffer
;;; *** instead of executing the commands themselves
;;; *** InsertFullCartCmd
.proc	InsertFullCartCmd
	ldy CmdLength
	jsr BuildCartCmd
	jsr AskUser		; returns C=0 if ok -> command remains in the buffer.
	rts
.endproc
;;; *** Insert the cartridge command into the command buffer.
;;; *** This requires an additional " or #, depending on the cart type
;;; *** BuildCartCmd
.proc	BuildCartCmd
	jsr Remove
	lda QuoteType		; the IOCB command, a quote for basic or # for Mac.
	cmp CmdBuffer,y		; if the user did not provide it...
	beq isquoted
	jsr Insert
	lda QuoteType
	sta CmdBuffer,y
isquoted:
	iny
	jsr InsertDefaultDevice
	rts
.endproc
;;; *** Remove
;;; *** Remove blanks at the command buffer+y
.proc	Remove
removeagain:	
	lda CmdBuffer,y
	cmp #' '
	bne exit
	cpy LineLen
	beq exit
	dec LineLen
	sty CmdLengthStore
remloop:
	lda CmdBuffer+1,y
	sta CmdBuffer,y
	cpy LineLen
	beq insertnow
	iny
	bpl remloop
insertnow:
	;; insert blanks at the end.
	lda #' '
	sta CmdBuffer+1,y
	ldy CmdLengthStore
	bpl removeagain
exit:
	rts
.endproc
;;; *** Dot-extended commands that take a number to index
;;; *** into the directory listing
Save1Cmd:	
Bload1Cmd:	
Move1Cmd:	
Rename1Cmd:	
List1Cmd:	
Delete1Cmd:	
	inc AcceptOK		;since all the above overwrite data, ask for acceptance
Enter1Cmd:	
Unlock1Cmd:	
Lock1Cmd:	
Run1Cmd:	
Load1Cmd:
	;; runs all into the following
;;; *** Handle short commands
;;; HandleDotCommand
.proc	HandleDotCommand
	lda LineLen
	cmp #2
	bcc exit		;no argument -> stop

	;; read the index of the file to operate on
	lda CmdBuffer+2
	cmp #'0'
	bcc exit		; not valid
	cmp #'9'+1
	bcs exit		; ditto
	sec
	sbc #$20		;convert to ANTIC representation
	cmp #$10
	bne notzero
	lda #0			;first digit will be a blank if zero.
notzero:
	sta EnumerationPat
	lda CmdBuffer+3		;get the next index
	sec
	sbc #$20
	sta EnumerationPat+1
	cmp #$10
	bcc notvalid
	cmp #$1a
	bcs notvalid
	lda LineLen
	cmp #3
	bne nottwodigit
notvalid:
	lda EnumerationPat
	sta EnumerationPat+1
	lda #$80		;do not care
	sta EnumerationPat
nottwodigit:
	lda CmdPtr
	pha
	lda CmdPtr+1
	pha
	;; find the file on the screen
	sec
	lda GfxOrigin
	sbc #18			;before the screen
	sta CmdPtr
	lda GfxOrigin+1
	sbc #0
	sta CmdPtr+1
	;; FIXME: Why this check here? Headline?
	ldy #18+3		;offset to find the enumerator at
	lda (CmdPtr),y
	bne scanloop
	;; next line
	lda #40
	jsr IncrementCmdPtr
scanloop:
	lda #20			;increment that by half a line
	jsr IncrementCmdPtr
	ldy #1
	lda (CmdPtr),y
	bne scanloop2
	;; here: nothing found
	pla
	pla			;dispose the command pointer, remove from stack
exit:
	sec			;do not forward to the user
	rts
scanloop2:
	cmp EnumerationPat+1
	bne scanloop		;fits?
	dey
	lda EnumerationPat
	and #$7f
	cmp (CmdPtr),y
	bne scanloop
	iny
	iny
	lda (CmdPtr),y
	beq found		;a blank
	cmp #$0a		;or a star?
	bne scanloop		;if not so, search again
found:
	lda #' '
	sta CmdBuffer+1
	sta CmdBuffer+2
	bit EnumerationPat	;two-digit?
	bmi short
	sta CmdBuffer+3
short:
	ldy #1
	jsr Remove		;remove all the blanks-arguments
	
	lda #11			;forward to the file name on the screen.
	jsr IncrementCmdPtr
	ldy #3-1
	jsr ReadScreen
	lda #'.'
	jsr InsertChar
	lda #-8
	jsr DecrementCmdPtr
	ldy #8-1
	jsr ReadScreen
	;; restore the command 
	pla
	sta CmdPtr+1
	pla
	sta CmdPtr
	lda #$01
	sta CmdLength
	;; FIXME: Is this correct?
	;; The CmdPtr should actually point to the shortened and not the dot version
	jmp HandleShortCommand
.endproc
;;; *** ReadScreen
;;; *** Read elements from the screen at Y and descending, pointed to by the command pointer
.proc	ReadScreen
loop:	
	lda (CmdPtr),y
	beq isblank
	and #$7f		;remove inversevid
	cmp #$40
	bcs f1
	adc #$20
	bcc f2
f1:	
	cmp #$60
	bcs f2
	sbc #$3f
f2:
	jsr InsertChar
isblank:
	dey
	bpl loop
	rts
.endproc
;;; *** Decrement the command pointer (pointing to the screen)
;;; *** by the given (negative) number
;;; *** DecrementCmdPtr
.proc	DecrementCmdPtr
	dec CmdPtr+1
	;; runs into here
.endproc
;;; *** Increment the command pointer (pointing to the screen buffer)
;;; *** by the given amount
;;; *** IncrementCmdPtr
.proc	IncrementCmdPtr
	clc
	adc CmdPtr
	sta CmdPtr
	bcc exit
	inc CmdPtr+1
exit:
	rts
.endproc
;;; *** Insert the character in A to the command buffer
;;; *** InsertChar
.proc	InsertChar
	sta CmdBuffer
	tya
	pha
	ldy #0
	jsr Insert
	pla
	tay
	rts
.endproc
;;; *** Prepare the sorting and place the pointer to the screen entry
;;; *** to be sorted: First screen address->SortPtr, SortCount=-1
;;; PrepSort
.proc	PrepSort
	clc
	lda GfxOrigin
	adc #22
	sta SortPtr
	lda GfxOrigin+1
	adc #0
	sta SortPtr+1
	lda #$ff
	sta SortCount
	rts
.endproc
;;; *** Sort the directory on the screen
;;; *** SortDirectory
.proc	SortDirectory
	jsr LoadSortPtr
	inc SortCount
	ldx SortCount
	beq exit
	stx SortIndex
	dex
	bne sort
	lda FreeSecsFlag	;free sectors is not sorted in.
	beq sort		;jump if regular file
exit:
	rts
sort:
	;; SortIndex = number of entries to sort-1, is at least 1 if the code
	;; comes here.
	;; the first screen line remains free as buffer
	ldy #40
	jsr CopyScreenToBuffer1
moveloop:
	dec SortIndex
	beq done
	;; move the sorted entries up by one line on
	;; the right side of the screen.
	ldy #40
	jsr CopyScreenToBuffer0
	ldy #0
	jsr CopyBuffer0ToScreen
	lda #40
	jsr IncrementCmdPtr

	clc
	bcc moveloop
done:
	;; entry on the left side to buffer0
	;; left-right column
	ldy #20			;left column, containing the next number
	jsr CopyScreenToBuffer0
	ldy #0			;to right, above, into the free slot
	jsr CopyBuffer0ToScreen
	ldy #20			;insert left entry in its place
	jsr CopyBuffer1ToScreen	;move from top left into its place
	;; free sectors?
	ldy #3
	lda (CmdPtr),y
	beq last

	lda #2
	jsr IncrementCmdPtr	;do not move the enumerations any more
	sec
	lda CmdPtr		;the other column
	sbc #20
	sta InsertPtr
	lda CmdPtr+1
	sbc #0
	sta InsertPtr+1

	;; This entry and the entry below to buffers 1 and 2
	;; these are the two new entries that are not yet sorted
	;; in correctly.
	ldy #0
	jsr CopyScreenToBuffer1	;get the element to insert into buffer1
	ldy #40
	jsr CopyScreenToBuffer2	;save element below as well, not sorted either
	
	jsr InsertBuffer1	;insert the first element
	
	jsr InsertPtrToCmdPtr	;get the left hand side pointer again
	ldy #40			;get element below
	jsr CopyScreenToBuffer0	;to buffer 0
	
	jsr LoadSortPtr
	ldy #2			;copy to the other side
	jsr CopyBuffer0ToScreen	;to duplicate this entry again
	
	jsr InsertPtrToCmdPtr	;restore
	lda #20
	jsr IncrementCmdPtr	;restore the command ptr, again on the right hand side
	
	ldy #40
	jsr CopyBuffer2ToScreen	;into the free position down right
	ldy #40
	jsr CopyScreenToBuffer1	;move to buffer 1
	
	ldy #19
	lda CmdBuffer,y		;is this "free sectors"?
	beq last		;if so, do nothing.
	
	jsr InsertBuffer1	;otherwise, insert me
last:
	;; cleanup the first display line again
	ldy #40-1
	lda #0
clrloop:
	sta (GfxOrigin),y
	dey
	bpl clrloop
	ldy #$2b
	lda #$11
	sta (GfxOrigin),y
	rts
.endproc
;;; *** InsertBuffer1
;;; *** Insert the buffer1 into either the left or the right column
.proc	InsertBuffer1
	jsr InsertIntoSingleColumn
	bcs exit		;already done?
	;; if not so, go for the other column
	jsr InsertPtrToCmdPtr
	jsr InsertIntoSingleColumn
exit:
	rts
.endproc
;;; *** InsertIntoSingleColumn
;;; *** Insert buffer 1 into the column in the CmdPtr, alphabetically sorted
;;; *** Uses buffer#0 as temporary. Returns with C=1 if a suitable insertion
;;; *** position was found.
.proc 	InsertIntoSingleColumn
	lda SortCount
	sta SortIndex		;copy over, number of rows to check
scanloop:
	ldy #0
	jsr CopyScreenToBuffer0	;next position to compare
	ldx #0
cmploop:			;note that the file name starts at offset 1
	inx
	cpx #12			;file name plus extender
	bcs smaller
	lda CmdBuffer+$12,x	;buffer1
	cmp CmdBuffer,x		;buffer0
	bcc smaller
	beq cmploop
	;; contents of buffer1>screen: copy buffer1 into free position *below*
	ldy #40
	jsr CopyBuffer1ToScreen
	sec			;position found
	rts
smaller:
	ldy #40			;move buffer0 into free screen position
	jsr CopyBuffer0ToScreen
	lda #-40		;and move upwards by one
	jsr DecrementCmdPtr
	dec SortIndex		;until no free slots there
	bne scanloop

	clc			;not found
	rts
.endproc
;;; *** Copy the screen data to the ternay command buffer
.proc	CopyScreenToBuffer2
	ldx #$24
	Skip2
.endproc
;;; *** Copy the data from the CmdPtr+y pointing to the screen
;;; *** to the secondary command buffer
.proc	CopyScreenToBuffer1
	ldx #$12
	Skip2
.endproc
;;; *** Copy the data from the CmdPtr+y pointing to the screen
;;; *** to the command buffer
.proc	CopyScreenToBuffer0
	ldx #$00
	lda #$12
	sta CopyTmp
loop:
	lda (CmdPtr),y
	sta CmdBuffer,x
	inx
	iny
	dec CopyTmp
	bne loop
	rts
.endproc
;;; *** Copy the the command buffer 2 to the screen pointer
;;; *** CopyBuffer2ToScreen
.proc	CopyBuffer2ToScreen
	ldx #$24
	Skip2
.endproc
;;; *** Copy the command buffer 1 to the screen pointer
;;; *** CopyBuffer1ToScreen
.proc	CopyBuffer1ToScreen
	ldx #$12
	Skip2
.endproc
;;; *** Copy the command buffer 0 to the screen pointer at CmdPtr+Y
;;; *** CopyBuffer0ToScreen
.proc	CopyBuffer0ToScreen
	ldx #$00
	lda #$12
	sta CopyTmp
loop:
	lda CmdBuffer,x
	sta (CmdPtr),y
	inx
	iny
	dec CopyTmp
	bne loop
	rts
.endproc
;;; *** Move the SortPointer to the CmdPointer
;;; *** LoadSortPtr
.proc	LoadSortPtr
	lda SortPtr
	sta CmdPtr
	lda SortPtr+1
	sta CmdPtr+1
	rts
.endproc
;;; *** Move the InsertPtr to the CmdPtr
;;; *** InsertPtrToCmdPtr
.proc	InsertPtrToCmdPtr
	lda InsertPtr
	sta CmdPtr
	lda InsertPtr+1
	sta CmdPtr+1
	rts
.endproc
;;; *** Move the Dir Buffer to the CmdPtr
;;; *** DirBufferToCmdPtr
.proc	DirBufferToCmdPtr
	lda DirBufferVector
	sta CmdPtr
	lda DirBufferVector+1
	sta CmdPtr+1
	rts
.endproc
;;; *** Error: Handle an error and return to the caller, not
;;; *** including the data to the user
.proc	Error
	pha
	jsr Close5
	pla
	sta ScreenError
	sec			; do not insert
	rts
.endproc
;;; *** InsertDefaultDevice
;;; *** Check the file specification in the command buffer at offset Y
;;; *** where a device specification is present. If not, insert one.
.proc	InsertDefaultDevice
	lda #':'
	cmp CmdBuffer+1,y
	beq hasdevice
	cmp CmdBuffer+2,y
	beq hasdevice
	jsr Insert
	jsr Insert
	lda #'D'
	sta CmdBuffer,y
	lda #':'
	sta CmdBuffer+1,y
hasdevice:
	rts
.endproc
;;; *** Copy a file from A to B
;;; *** Move3Cmd
.proc	Move3Cmd
	jsr FindMemoryBase
	lda #CmdOpen
	jsr SpecialXIO
	bcc continue
	;; user aborted, return with C=1 to clear the command buffer
	rts
continue:
	ldx #$50
	ldy CopyBase
	lda CopyBase+1
	jsr CIOSetBuffer
	lda #CmdGetBlock
	sta IOCBCmd,x
	sec
	lda MemTop
	sbc CopyBase
	sta CopyLen
	tay
	lda MemTop+1
	sbc CopyBase+1
	sta CopyLen+1
	jsr ExecuteBlockCmd
	tya
	bpl noram
	cmp #EndOfFile
goerror:	
	bne Error		; error if it does not fit
	lda IOCBLen+1,x
	sta CopyLen+1
	lda IOCBLen,x
	sta CopyLen
	jsr Close5
	ldy InsertDestinationVector
	lda InsertDestinationVector+1
	jsr PrintAY

	jsr WaitForKey
	bcs abort

	ldx #$50
	cmp #$2a
	beq toscreen
	cmp #$3e
	bne notscreen
	;; here: copy to screen
toscreen:	
	lda #'E'
	sta CmdBuffer
	lda #':'
	sta CmdBuffer+1
	lda #$9b
	sta CmdBuffer+2
	bne docopy
notscreen:
	jsr FindTarget
	bne setbuffer
docopy:
	ldy #<CmdBuffer
	lda #>CmdBuffer
setbuffer:	
	jsr CIOSetBuffer

	ldy #$08		; open for write
	lda #CmdOpen
	jsr CIOExec
	tya
	bmi goerror
	ldy CopyBase
	lda CopyBase+1
	jsr CIOSetBuffer
	lda #CmdPutBlock
	sta IOCBCmd,x
	ldy CopyLen
	lda CopyLen+1
	jsr ExecuteBlockCmd
	tya
	bmi goerror
	bpl done
noram:
	ldy NoRamVector
	lda NoRamVector+1
	jsr PrintAY
done:
	jsr Close5
	lda #0
	sta DirectFlag
abort:	
	sec			;do not deliver to the user
	rts
.endproc
;;; *** Find the target specification, return in Y,A
;;; *** FindTarget
.proc	FindTarget
	ldy CmdLength
findend:			;is there a second argument behind a comma to indicate the destination?
	iny
	cpy LineLen
	bcs notarget
	lda CmdBuffer,y
	cmp #','
	bne findend
	iny			;position behind the separator
	jsr InsertDefaultDevice	;insert a D: if it is missing
	;; y is now the offset to the second device
	tya
	pha
	txa
	pha

	iny
	lda #':'
	cmp CmdBuffer,y
	beq hasdevice
	iny
	cmp CmdBuffer,y
	bne nodevice		;oops?
hasdevice:
	iny
	lda #$9b		;EOL at the position where the file name should be?
	cmp CmdBuffer,y
	bne nodevice
	;; find the source file name
	ldx CmdLength
	inx
	inx
	lda #':'
	cmp CmdBuffer-1,x
	beq foundsrc
	inx
	cmp CmdBuffer-1,x
	beq foundsrc
	ldx CmdLength
foundsrc:
	lda CmdBuffer,x
	cmp #','
	beq terminate
	cmp #$9b
	beq terminate
	sta CmdBuffer,y
	inx
	iny
	bne foundsrc
terminate:
	lda #$9b
	sta CmdBuffer,y
nodevice:
	pla
	tax
	pla
	tay
	bne add
notarget:
	lda CmdLength
add:
	clc
	adc #<CmdBuffer
	tay
	lda #>CmdBuffer
	rts
.endproc
;;; *** Ask the user whether it is ok to execute the given command
;;; *** Returns with C=1 if the execution shall be aborted, C=0 if continue
;;; *** AskUser
.proc	AskUser
	clc
	lda AcceptOK
	beq isok		;nothing to do, is always ok
	
	ldx #$0			;to the editor
	lda #>CmdBuffer
	ldy #<CmdBuffer
	jsr CIOSetBuffer
	ldy LineLen
	lda #'?'
	sta CmdBuffer,y
	iny
	lda #CmdPutRecord
	jsr CIOExec

	jsr WaitForKey
	bcs isnot

	cmp #43			;is ok?
	sec			;prepare for error
	bne isnot
	lda #0
	sta AcceptOK
	clc			;do not abort
isnot:
	ldy LineLen
	lda #' '		; remove the '?'
	sta CmdBuffer,y
isok:	
	rts
.endproc
;;; *** Install the IOCB buffer with Lo in Y and Hi in A
;;; *** CIOSetBuffer
.proc	CIOSetBuffer
	sta IOCBAdr+1,x
	tya
	sta IOCBAdr,x
	rts
.endproc
;;; *** Execute a command via CIO.
;;; *** Command in A. If command is CmdOpen - AUX1 in Y, otherwise len is in Y
;;; *** IOCB is in X. Address is on the stack
.proc	CIOExec
	sta IOCBCmd,x
	cmp #CmdOpen
	bne notopen
	tya
	sta IOCBAux1,x
	lda #0
	sta IOCBAux2,x
	tay
notopen:
	lda #0
	;; runs into the following
.endproc
;;; *** Execute a block command via CIO with the size in A,Y:
;;; *** hi in A, lo in Y
.proc	ExecuteBlockCmd
	sta IOCBLen+1,x
	tya
	sta IOCBLen,x
	jsr CallCIO
	rts
.endproc
;;; *** Print the text pointed to by A in Hi, Y in Lo
;;; *** PrintAY
.proc	PrintAY
	ldx #$0
	jsr CIOSetBuffer
	lda #CmdPutRecord
	ldy #255
	sta IOCBCmd
	lda #0
	sta IOCBLen
	tya
	sta IOCBLen+1
	jsr CIOVector		;leave cart disabled!
	rts
.endproc
;;; *** The Help command: print the help text
;;; *** Help
Help1Cmd:	
.proc	GiveHelp
	ldy HelpVector
	lda HelpVector+1
	ldx #0
	jsr CIOSetBuffer
	lda #CmdPutBlock
	sta IOCBCmd,x
	lda #<HelpLen
	sta IOCBLen,x
	lda #>HelpLen
	sta IOCBLen+1,x
	jsr CIOVector		;leave cart disabled!
	rts
.endproc
;;; *** Check whether the HELP key was pressed. If so,
;;; *** print the help text and return with Z=0. Otherwise,
;;; *** Return with Z=1.
.proc	CheckHelp
	lda HelpFlag
	beq nohelp
	jsr GiveHelp
	ldx #0
	stx HelpFlag
	inx			;not equal
nohelp:
	rts
.endproc
;;; *** Find the base memory - the first free memory address
;;; *** copy can potentially use
.proc	FindMemoryBase
	lda AppMemHi
	sta CopyBase
	lda AppMemHi+1
	sta CopyBase+1
	lda QuoteType
	cmp #'#'
	bne isbasic		;for basic, this is all.
	;; Mac/65 has a bug - AppMemHi is not initialized correctly.
	;; instead, $88/$89 contains the first free byte
	lda $88
	cmp AppMemHi
	lda $89
	sbc AppMemHi+1
	bcc isbasic		;take the larger of the two
	lda $88
	sta CopyBase
	lda $89
	sta CopyBase+1
isbasic:
	rts
.endproc
;;; *** Strings for the vector
;;; Strings

InsertDestinationVector:	.word InsertDestination
NoRamVector:			.word NoRam
TitleVector:			.word Title
HelpVector:			.word HelpText
ContinueTextVector:		.word ContinueText
ClearLineTextVector:		.word ClearLineText
AbortedVector:			.word Aborted
	
InsertDestination:
	.byte "Insert disk 2,press any key(S=to E:).",155	
NoRam:
	.byte "Out of RAM !",155
Aborted:
	.byte 155,"BREAK",155
Title:
	.byte 155,155,155
	.byte "Diskio 6.++ 1985 B.E. Oppenheim",155
	.byte "extended 1991,2013 THOR-Software",155
	.byte "Type HELP for commands",155
TitleLen	=	*-Title
	
HelpText:
	.byte 125
	.byte "Commands :",155,155
	.byte "DIR     Directory",155,155
	.byte ".X->LI.->LISTD  List to disk",155
	.byte ".S->S. ->SAVE   Save to disk",155
	.byte ".L->LO.->LOAD   Load from disk",155
	.byte ".E->E. ->ENTER  Enter from disk",155
	.byte ".R->     RUND   Run disk file",155
	.byte ".K->     LOCK   Lock  file",155
	.byte ".U->UN.->UNLOCK Unlock file",155
	.byte ".N->RE.->RENAME Rename file",155
	.byte ".D->DE.->DELETE Delete file",155
	.byte ".B->BL.->BLOAD  Binary load",155
	.byte ".C->CO.->COPY   Copy file",155,155
	.byte ".R9 runs #9 etc.",155
HelpLen	=	*-HelpText
;;; *** Continue Text
ContinueText:
	.byte 29,'P'+$80,"ress ",'E'+$80,'S'+$80,'C'+$80
	.byte " -> Abort    "
	.byte 'R'+$80,'E'+$80,'T'+$80,'U'+$80,'R'+$80,'N'+$80
	.byte " ->Continue"
	.byte 28,28,155
;;; ClearLine Text
ClearLineText:
	.byte 28,28+$80,28+$80,28+$80,28,155
ListEnd:
	.byte "32767",0	;last possible line;
;;; *** Global variables
;;; ***
InputCounter:		.byte 0	;number of characters in the buffer so far.
LineLen:		.byte 0 ;total size of the input in bytes
CmdLength:		.byte 0	;size of the command in the input buffer (<=LineLen)
AcceptOK:		.byte 0	;if ne, the user needs to accept the command
FreeSecsFlag:		.byte 0	;if set: current entry is "free sectors"
DirEnumerator:		.byte 0 ;in BCD: file counter, to be inserted on the screen
XIOCmd:			.byte 0	;next CIO Cmd to be run on disk
CmdLengthStore:		.byte 0	;a temporary for storing the current length of the command
CopyTmp:		.byte 0	;a temporary counter when copying data
EnumerationPat:		.byte 0,0 ;the ANTIC code of the file enumeration to locate on screen
SortCount:		.byte 0	  ;number of entries to sort in directory
SortIndex:		.byte 0	  ;temporary for finding the insertion position
TrueReset:		.byte 0	  ;set on a true reset, initially zero
SortPtr:		.word 0	  ;stores the screen address of the dir entry to be sorted
InsertPtr:		.word 0	  ;temporary pointer for directory insertion sort
CopyLen:		.word 0	  ;available memory for copy
CopyBase:		.word 0	  ;first free memory
HeadLine:		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

HiLen		=	*-HiStart
