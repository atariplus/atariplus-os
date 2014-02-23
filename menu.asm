;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menu.asm,v 1.32 2013/11/24 19:28:59 thor Exp $		**
;;; **									**
;;; ** In this module:	 Menu driven DUP for Dos 2.++			**
;;; ** Derived from version 1.12 (19-Jan-1990)				**
;;; **********************************************************************

	.include "menu.i"
	.include "cio.i"
	.include "kernel.i"
	.include "editor.i"
	.include "screen.i"
	.include "reset.i"
	.include "errors.i"
	.include "irq.i"
	.include "pokey.i"
	.include "nmi.i"
	.include "mathpack.i"
	.include "gtia.i"
	.include "antic.i"
	.include "fmsreloc.i"
	
	.segment "menu"
	
;;; *** Skip two bytes (by a dummy BIT)
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro
;;; *****
;;; System constants etc....
	.include "menuglobals.i"
	
;;; **** System jump table starts here
	.global	StartOffset
StartAddress	=	*
StartOffset	=	StartAddress-MenuStart
	jmp Start
	jmp Init
	jmp Print		;print the string at x,y length a
	jmp SetIOCB 		;set the IOCB to use for the next commands, X=#
	jmp LoadInputBufferPtr 	;load the input buffer and its size to ->X,Y and A
	jmp LoadBuf1Ptr		;load the buffer 1 pointer and its size to X,Y and A
	jmp LoadBuf2Ptr		;load the buffer 2 pointer and its size to X,Y and A
	jmp LoadBuf3Ptr		;load the buffer 3 pointer and its size to X,Y and A
	jmp Open		;open the current IOCB for the filespec (X,Y) mode A
	jmp Close		;close the current IOCB
	jmp XIO			;detach the XIO cmd A on the filespec (X,Y)
	jmp ClearAux		;reset the Aux values for the current IOCB
	jmp GetLine		;read a line from the editor to X,Y, length A
	jmp Error   		;display the error in Y
	jmp SetHeadLine		;set the headline with the text at X,Y
	jmp ClearHeadLine	;clear the headline
	jmp Reserve 		;reserve (X,Y) bytes of memory, return the memory pointer in X,Y
	jmp Dispose		;return the memory block (X,Y) to the system. Must be called in inverse order of RESERVE
	jmp ReleaseAllMemory  	;release all allocated memory at once
	jmp CheckIOError	;test whether Y is a relevant IO error. C=0: none, C=1: EOF, other error generated
	jmp AbortOnBreak	;test whether Y is an error. C=0: none. On BREAK, return to main. Otherwise, set C and N
	jmp BlockDisplay   	;disable the display switching capability
	jmp ReadInputParameter 	;read parameter from input buffer to ->(X,Y), Z=1 if empty,C=1 if EOL
	jmp Copy    		;copy A bytes from (X,Y) to (fr0,1)
	jmp ParameterError   	;signal a parameter error
	jmp SetRegularColor	;install the regular background color
	jmp SetRedColor
	jmp SetGreenColor
	jmp SetBlueColor
	jmp SetYellowColor
	jmp UnblockDisplay	;re-allow switching the display
	jmp DisableBREAK	;disable the BREAK key
	jmp EnableBREAK		;re-enable the BREAK key
	jmp ToDecimal   	;X,Y->Buf3
	jmp ToHex   		;X,Y->Buf3
	jmp FromDecimal   	;(X,Y)->X,Y
	jmp FromHex   		; "
	jmp SetAux		;X->AUX1,Y->AUX2
	jmp SetIOCBAdr  	;install the address X,Y into the current IOCB
	jmp SetLength		;install the length X,Y into the current IOCB
	jmp SetCommand		;install A as next CIO command
	jmp RunCIOCmd   	;run the CIO Command in A with all the other settings already in place
	jmp GetKey		;read a character from the keyboard, return in A
	jmp BGet		;read the block to (x,y) from the current IOCB
	jmp BPut     		;write the block at (x,y) to the current IOCB
	jmp PrintRecord 	;Print the EOL terminated record at (X,Y) len A
	jmp AddDevice		;add the default device to the buffer (x,y) if not present
	jsr ConvertDirectoryEntry	;convert the directory entry in (x,y) to a filespec in buf3.
	jmp GetArgumentExtension	;check whether (x,y) contains extension /A. If so, remove and return C=1
	jmp CopyBuffer2Buffer   ;copy buffer X-> buffer Y
	jmp CursorOff		;disable the cursor
	jmp CursorOn		;enable the cursor
	jmp PrintEOL		;print a line feed
	jmp SetZPtr		;set the DUP ZPtr from X,Y
	jmp RunCartridge	;run the cartridge
	jmp RunCommandLine	;run the standard DUP command line
	jmp YesNo		;ask for a single keypress, return C=1 if yes, else C=0
	jmp CloseAll		;close all channels
	jmp GetDirectoryList	;read the directory contents as a list, filespec in (x,y), use #1. Return on heap in (x,y)
	jmp GetDirectoryEntry 	; A=unit#,(X,Y)->InputBuffer, C=1 end or (X,Y) pointer to filespec.
	jmp GetUnitNumber 	; (X,Y)->A = DEVICE #
	jmp SetDefaultUnit	; set the default unit in A
	jmp GetLength		; return the length of the last IOCB command to X,Y
	jmp RestoreOsDisplay	; restore the Os display
	jmp InstallDisplay	; install the menu display if necessary
	jmp InstallDestructor	; install a destructor at X,Y
	jmp RemoveMenu		; remove the menu display and menu specific vectors
	jmp InstallMenu		; re-install the menu vectors.
	jmp GetDefaultDevice	; read the default device to X,Y
	jmp SetDefaultDevice	; set the default device from X,Y
	jmp NextDirectoryEntry	; point the Z-page location X point to the next entry.
	jmp GetBufferedLength	; store data X,Y into the directory entry at fr0+2,fr0+3
	jmp SetBufferedLength	; store data X,Y into the directory entry at fr0+2,fr0+3
;;; *** End of the jump table
;;; *** Print
;;; **** Print the string of size A, X=lo,Y=hi
.proc	Print
	sta IOCBLen
	stx IOCBAdr
	sty IOCBAdr+1
	ldx #0
	stx IOCBLen+1
	lda #CmdPutBlock
	sta IOCBCmd
	jsr CIOVector
	bpl ok
	jmp Main
ok:
	rts
.endproc
;;; *** CursorOn
;;; *** Enable the cursor
.proc	CursorOn
	lda #0
	Skip2
.endproc
;;; *** CursorOff
;;; *** Disable the cursor
.proc	CursorOff
	lda #1
	sta CursorInhibit
	ldx #<MoveCursor
	ldy #>MoveCursor
	lda #2
	jsr Print
	rts

.endproc
;;; *** PrintEOL
;;; *** Print a line feed
.proc	PrintEOL
	ldx #<EOL
	ldy #>EOL
	lda #1
	jsr Print
	rts
.endproc
;;; *** PrintRecord
;;; *** Print the next record up the next EOL
;;; *** A=len,adr=x,y
.proc	PrintRecord
	sta IOCBLen
	stx IOCBAdr
	sty IOCBAdr+1
	ldx #0
	stx IOCBLen+1
	lda #CmdPutRecord
	sta IOCBCmd
	jsr CIOVector
	bpl ok
	jmp Main
ok:
	rts
.endproc
;;; *** SetIOCB
;;; *** Set the IOCB for the IOCB dependent commands in X
.proc	SetIOCB
	stx IOCB
	rts
.endproc
;;; *** LoadInputBufferPtr
;;; *** Load the size and address of the input buffer to X,Y and A
.proc	LoadInputBufferPtr
	ldx #<InputBuffer
	ldy #>InputBuffer
	lda #80
	rts
.endproc
;;; *** LoadBuf1Ptr
;;; *** Load the size and address to Buf1 to X,Y and A
.proc 	LoadBuf1Ptr
	ldx #<Buf1
	ldy #>Buf1
	lda #40
	rts
.endproc
;;; *** LoadBuf2Ptr
;;; *** Load the size and address to Buf1 to X,Y and A
.proc 	LoadBuf2Ptr
	ldx #<Buf2
	ldy #>Buf2
	lda #40
	rts
.endproc
;;; *** LoadBuf3Ptr
;;; *** Load the size and address to Buf1 to X,Y and A
.proc	LoadBuf3Ptr
	ldx #<Buf3
	ldy #>Buf3
	lda #40
	rts
.endproc
;;; *** GetLength
;;; *** Get the length of the IOCB to Y (hi) and X (lo)
.proc	GetLength
	ldx IOCB
	lda IOCBLen+1,x
	tay
	lda IOCBLen,x
	tax
	rts
.endproc
;;; *** Open
;;; *** Open the IOCB to the filespec in X,Y in the mode A
.proc	Open
	pha
	jsr SetIOCBAdr
	pla
	sta IOCBAux1,x
	lda #CmdOpen
	sta IOCBCmd,x
	lda #0
	sta IOCBAux2,x
	jsr CIOVector
	rts
.endproc
;;; *** Close
;;; *** Close the current IOCB
.proc	Close
	lda #CmdClose
	ldx IOCB
	sta IOCBCmd,x
	jsr CIOVector
	rts
.endproc
;;; *** CloseAll
;;; *** Close All IOCB channels
.proc	CloseAll
	ldx #$10
	stx IOCB
loop:
	jsr Close
	lda IOCB
	clc
	adc #$10
	sta IOCB
	cmp #$70
	bcc loop
	rts
.endproc
;;; *** XIO
;;; *** Detach the XIO Command A on the filespec X,Y
.proc	XIO
	pha
	jsr SetIOCBAdr
	pla
	sta IOCBCmd,x
	jsr CIOVector
	rts
.endproc
;;; *** BGet
;;; *** Read into the block at X,Y from the current IOCB
.proc	BGet
	jsr SetIOCBAdr
	lda #CmdGetBlock
	sta IOCBCmd,x
	jsr CIOVector
	rts
.endproc
;;; *** BPut
;;; *** Write the block at X,Y to the current IOCB
.proc	BPut
	jsr SetIOCBAdr
	lda #CmdPutBlock
	sta IOCBCmd,x
	jsr CIOVector
	rts
.endproc
;;; *** ClearAux
;;; *** Reset the Aux values for the current IOCB
.proc	ClearAux
	ldx IOCB
	lda #0
	sta IOCBAux1,x
	sta IOCBAux2,x
	rts
.endproc
;;; *** SetAux
;;; *** Install X into Aux1 and Y into Aux2
.proc	SetAux
	txa
	ldx IOCB
	sta IOCBAux1,x
	tya
	sta IOCBAux2,x
	rts
.endproc
;;; *** SetIOCBAdr
;;; *** Install the address X,Y
.proc	SetIOCBAdr
	txa
	ldx IOCB
	sta IOCBAdr,x
	tya
	sta IOCBAdr+1,x
	rts
.endproc
;;; *** SetLength
;;; *** Set the Length of the next CIO command to X (lo) and Y (hi)
.proc	SetLength
	txa
	ldx IOCB
	sta IOCBLen,x
	tya
	sta IOCBLen+1,x
	rts
.endproc
;;; *** SetCommand
;;; *** Install A as next IOCB Command
.proc	SetCommand
	ldx IOCB
	sta IOCBCmd,x
	rts
.endproc
;;; *** RunCIOCmnd
;;; *** Execute the CIO command in A over the CIO system vector
.proc	RunCIOCmd
	jsr SetCommand
	jsr CIOVector
	rts
.endproc
;;; *** OpenKeyboard
;;; *** Open #7 for the keyboard handler, install zero length and prepare
;;; *** for reading a single character
.proc	OpenKeyboard
	ldx #$70
	jsr SetIOCB
	jsr Close
	ldy #>KeySpec
	ldx #<KeySpec
	lda #4			;for reading
	jsr Open
	ldx #0
	ldy #0
	jsr SetLength
	lda #CmdGetBlock
	jsr SetCommand
	rts
.endproc
;;; *** GetKey
;;; *** Get a character from the keyboard, return the character in A
.proc	GetKey
	ldx #$70
	jsr CIOVector
	bpl ok
	lda #0
	sta BreakFlag
	jmp Main
ok:
	rts
.endproc
;;; *** GetLine
;;; *** Read a line from E: into the buffer at X,Y lengthA
.proc	GetLine
	sta IOCBLen
	stx IOCBAdr
	sty IOCBAdr+1
	ldx #0
	stx IOCBLen+1
	lda #CmdGetRecord
	sta IOCBCmd
	jsr CIOVector
	bpl ok
	jmp Main
ok:
	rts
.endproc
;;; *** YesNo
;;; *** Ask the user for a single keypress, return C=1 if accept, C=0 otherwise
.proc	YesNo
	jsr GetKey
	and #$5f		;to uppercase
	cmp #'Y'
	beq yes
	cmp #'J'
	beq yes
	clc
yes:				;C=1 here by itself
	rts
.endproc
;;; *** Run the cartridge
;;; *** RunCartridge
.proc	RunCartridge
	jsr RemoveDUP
	jsr WaitForKeyRelease
	jmp WarmStartVector
.endproc
;;; *** Run the DUP Command Line
;;; *** RunCommandLine
.proc	RunCommandLine
	jsr RemoveDUP
	lda FmsBootFlag
	ora #$40
	sta FmsBootFlag
	jsr WaitForKeyRelease
	jmp WarmStartVector
.endproc
;;; *** WaitForKeyRelease
;;; *** Wait for the user to release the key,
;;; *** then return.
.proc	WaitForKeyRelease
	ldx #$ff
cps:
	cpx KeyStat
	bne cps			; wait until the keyboard goes up again
	stx KeyCodeShadow
	rts
.endproc
;;; *** SetHeadLine
;;; *** Set the Headline with the string at X,Y size A
.proc	SetHeadLine
	jsr SetZPtr
	tay
	beq exit
	dey
copy:
	lda (ZPtr),y
	ora #$80
	sta HeadLine,y
	dey
	bpl copy
exit:
	rts
.endproc
;;; *** SetZPtr
;;; *** Install X,Y in ZPtr
.proc	SetZPtr
	stx ZPtr
	sty ZPtr+1
	rts
.endproc
;;; *** ClearHeadLine
;;; *** Clear the headline
.proc	ClearHeadLine
	ldy #39
	lda #$80
clr:
	sta HeadLine,y
	dey
	bpl clr
	rts
.endproc
;;; *** SetHeadLineASCII
;;; *** Instead of taking ANTIC codes, this one takes ASCII codes and sets it to the headline
.proc	SetHeadLineASCII
	jsr SetZPtr
	tay
	beq exit
	dey
copy:
	lda (ZPtr),y
	;; convert to screen code
	cmp #$60
	bcs inverse
	sbc #$1f
	bcs inverse
	adc #$60
inverse:
	ora #$80
	sta HeadLine,y
	dey
	bpl copy
exit:
	rts
.endproc
;;; *** Reserve
;;; *** Reserve X,Y bytes of memory, return the reserved memory block in X,Y
.proc	Reserve
	lda AppMemHi+1
	pha
	lda AppMemHi
	pha
	clc
	txa
	adc AppMemHi
	sta AppMemHi
	tya
	adc AppMemHi+1
	sta AppMemHi+1
	;; Still valid
	lda AppMemHi
	cmp MemTop
	lda AppMemHi+1
	sbc MemTop+1
	bcs outofmem
	pla
	tax
	pla
	tay
	rts
outofmem:
	jsr ClearHeadLine
	ldx #<OutOfMemoryE
	ldy #>OutOfMemoryE
	lda #OutOfMemoryEL
	jsr SetHeadLineASCII
	jmp SystemError
.endproc
;;; *** Dispose
;;; *** Return the memory block at (X,Y) to the system. Must be called in the inverse order of Reserve
.proc	Dispose
	stx AppMemHi
	sty AppMemHi+1
	rts
.endproc
;;; *** ReleaseAllMemory
;;; *** Release all allocated memory to the system
.proc	ReleaseAllMemory
	ldx DupEnd
	ldy DupEnd+1
	jsr Dispose
	rts
.endproc
;;; *** CheckIOError
;;; *** Check whether Y is a relevant IO error. Return C=0 if not so, otherwise generate an error.
;;; *** Return C=1 on EOF error
.proc	CheckIOError
	cpy #0
	clc
	bpl exit
	cpy #EndOfFile
	beq exit
	jmp Error
exit:
	rts
.endproc
;;; *** AbortOnBreak
;;; *** Check Y is an error. If not, return with C=0. If BREAK, return to main
;;; *** otherwise, set M and C
.proc	AbortOnBreak
	cpy #0
	bpl noerror
	cpy #BreakError
	beq break
	cpy #0
	sec
	rts
noerror:
	clc
	rts
break:
	ldx STAX
	txs
	jmp Main
.endproc
;;; *** BlockDisplay
;;; *** Disallow display swithing
.proc	BlockDisplay
	lda #$ff
	sta AllowSwitch
	lda #$00
	sta DisplaySwitch
	rts
.endproc
;;; *** UnblockDisplay
;;; *** Re-enable display switching
.proc	UnblockDisplay
	lda #$00
	sta AllowSwitch
	rts
.endproc
;;; *** DisableBREAK
;;; *** Disable the break key
.proc	DisableBREAK
	lda IRQStatShadow
	and #$7f
	sta IRQStatShadow
	sta IRQStat
	rts
.endproc
;;; *** EnableBREAK
;;; *** Enable the break key
.proc	EnableBREAK
	lda IRQStatShadow
	ora #$80
	sta IRQStatShadow
	sta IRQStat
	lda #$80
	sta BreakFlag
	rts
.endproc
;;; *** SetRegularColor
;;; *** Set the background color to the regular color scheme
.proc	SetRegularColor
	lda #$90
	Skip2
.endproc
;;; *** SetRedColor
;;; *** Select the red color scheme
.proc	SetRedColor
	lda #$30
	Skip2
.endproc
;;; *** SetGreenColor
;;; *** Select the Green color scheme
.proc	SetGreenColor
	lda #$b0
	Skip2
.endproc
;;; *** SetBlueColor
;;; *** Select the blue color scheme
.proc	SetBlueColor
	lda #$80
	Skip2
.endproc
;;; *** SetYellowColor
;;; *** Select the yellow color scheme
.proc	SetYellowColor
	lda #$10
.endproc
;;; *** SetColorScheme
;;; *** Install the color scheme with the background color in A
.proc	SetColorScheme
	sta Color2Shadow
	ora #2
	sta ColorBackShadow
	and #$f0
	ora #$0c
	sta Color1Shadow
	jsr WaitVBI
	rts
.endproc
;;; *** ParameterError
;;; *** Signal a parameter error
.proc	ParameterError
	jsr ClearHeadLine
	ldx #<ParamError
	ldy #>ParamError
	lda #ParamErrorL
	jsr SetHeadLineASCII
	jmp HandleError
.endproc
;;; *** Error
;;; *** Signal the error in Y
.proc	Error
	tya
	pha
	jsr ClearHeadLine
	ldy #0
	pla
	pha
	tax
	jsr ToDecimal
	ldx #2
copy:
	lda Buf3,x
	sta ErrorValue,x
	dex
	bpl copy
	pla
	ldx #0
fnderror:
	ldy ErrorTable,x
	beq end
	cmp ErrorTable,x
	beq found
	pha
	txa
	clc
	adc #$10
	tax
	pla
	cpx #0
	bne fnderror
	beq end
found:
	ldy #0
copyerr:	
	lda ErrorTable+1,x
	sta ErrorTextRead,y
	inx
	iny
	cpy #15
	bne copyerr
	beq sethdline
end:
	ldy #0
	lda #' '
blank:	
	sta ErrorTextRead,y
	iny
	cpy #15
	bne blank
sethdline:
	ldx #<ErrorText
	ldy #>ErrorText
	lda #ErrorTextL
	jsr SetHeadLineASCII
	jmp HandleError
.endproc
;;; *** HandleError
;;; *** Signal an accoustic warning, release all memory, return to the main memory
.proc	HandleError
	jsr SetYellowColor
	jsr CloseAll
	jsr ReleaseAllMemory
	jsr WarningSound
	jmp Main
.endproc
;;; *** ToDecimal
;;; *** Convert the decimal X,Y to ASCII in Buf3
.proc	ToDecimal
	stx fr0
	sty fr0+1
	jsr IntToBCDVector
	jsr BCDToAsciiVector
	cld
	ldy #0
lp:	lda (inbuff),y
	php
	and #$7f
	sta Buf3,y
	iny
	plp
	bpl lp
	lda #' '
fill:
	sta Buf3,y
	iny
	cpy #40
	bcc fill
	rts
.endproc
;;; *** ToHex
;;; *** Convert the hex X,Y to ASCII in Buf3
.proc	ToHex
	jsr SetZPtr
	lda #' '
	ldx #39
sx:	sta Buf3,x
	dex
	bpl sx

	lda ZPtr+1
	jsr ConvertDigit
	stx Buf3
	sty Buf3+1
	lda ZPtr
	jsr ConvertDigit
	stx Buf3+2
	sty Buf3+3
	cld
	rts
ConvertDigit:
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	jsr NibbleToASCII
	tax
	pla
	and #$0f
	jsr NibbleToASCII
	tay
	rts
NibbleToASCII:
	sed
	clc
	adc #$90
	adc #$40
	cld
	rts
.endproc
;;; *** FromDecimal
;;; *** Convert the decimal number pinted to by (x,y) to x,y
.proc	FromDecimal
	stx inbuff
	sty inbuff+1
	lda #0
	sta cix
	jsr AsciiToBCDVector
	bcs error
	jsr BCDToIntVector
	bcs error
	ldx fr0
	ldy fr0+1
	rts
error:
	jmp ParameterError
.endproc
;;; *** FromHex
;;; *** Conver the hex number in (x,y) to x,y
.proc	FromHex
	stx fr0+2
	sty fr0+3
	ldy #0
	sty fr0
	sty fr0+1
loop:	
	lda (fr0+2),y
	jsr ConvertDigit
	bcs abort
	pha
	jsr Shift
	pla
	ora fr0
	sta fr0
	iny
	cpy #5
	bcc loop
	bcs error
abort:
	cpy #0
	beq error
	lda (fr0+2),y
	cmp #155
	beq exit
	cmp #','
	bne error
exit:
	ldx fr0
	ldy fr0+1
	rts
error:
	jmp ParameterError
Shift:
	asl fr0
	rol fr0+1
	asl fr0
	rol fr0+1
	asl fr0
	rol fr0+1
	asl fr0
	rol fr0+1
	rts
ConvertDigit:
	sec
	sbc #'0'
	bcc cberror
	cmp #10
	bcc cdexit
	sbc #7
	cmp #10
	bcc cberror
	cmp #16
	bcs cberror
cdexit:
	clc
	rts
cberror:
	sec
	rts
.endproc
;;; *** ReadInputParameter
;;; *** Read an extended parameter from the input buffer to (x,y), return with Z=1 if empty, C=1 if EOL
.proc	ReadInputParameter
	jsr SetZPtr
	ldy #0
	ldx #0
getloop:
	lda InputBuffer,x
	cmp #$9b
	beq end
	cmp #','
	beq end
	sta (ZPtr),y
	iny
	inx
	cpy #39
	bcc getloop
	jmp ParameterError	;too long
end:
	clc
	php
	cmp #$9b
	bne noteol
	plp
	sec
	php
	sta InputBuffer+1,x
noteol:
	lda #$9b
	sta (ZPtr),y
	inx
	ldy #0
shiftloop:
	lda InputBuffer,x
	sta InputBuffer,y
	iny
	inx
	cpx #80
	bcc shiftloop
	ldy #0
	lda (ZPtr),y
	plp
	eor #$9b
	rts
.endproc
;;; *** Copy
;;; *** Copy A bytes from (x,y) (to fr0)
.proc	Copy
	jsr SetZPtr
	tay
	beq exit
	dey
lp:	lda (ZPtr),y
	sta (fr0),y
	dey
	bpl lp
exit:
	rts
.endproc
;;; *** WarningSound
;;; *** Play a warning sound
.proc	WarningSound
	lda #3
	sta SkStat
	lda #0
	sta AudioCtrl
	lda #$cf
	sta AudCtrl0
	sta AudCtrl1
	lda #12
	sta AudFreq0
	lda #15
	sta AudFreq1
	ldx #20
wt2:
	jsr WaitVBI
	dex
	bpl wt2
	lda #0
	sta AudCtrl0
	sta AudCtrl1
	rts
.endproc
;;; *** AddDevice
;;; *** Add the default device to the buffer pointed to by (X,Y)
;;; *** returns C=0 if a device was present, returns Z=1 if the input was empty
.proc	AddDevice
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	cmp #$9b
	beq add
	iny
	lda (ZPtr),y
	cmp #':'
	beq exitok
	cmp #$9b
	beq addnz
	iny
	lda (ZPtr),y
	cmp #':'
	beq exitok
addnz:
	lda #1			;set NZ
add:
	php
	;; make room in the buffer
	lda ZPtr
	clc
	adc DefaultDeviceLen
	sta Z2Ptr
	lda ZPtr+1
	adc #0
	sta Z2Ptr+1
	lda #39
	sec
	sbc DefaultDeviceLen
	tay
mv:
	lda (ZPtr),y
	sta (Z2Ptr),y
	dey
	bpl mv
	ldy DefaultDeviceLen
	beq exitnodev
	dey
ilop:
	lda DefaultDevice,y
	sta (ZPtr),y
	dey
	bpl ilop
exitnodev:
	plp
	sec
	rts
exitok:
	lda #1
	clc
	rts
.endproc
;;; *** ConvertDirectoryEntry
;;; *** Convert the directory entry in (x,y) to a file spec in Buf3 and return it in (x,y)
.proc	ConvertDirectoryEntry
	jsr SetZPtr
	ldy #2
	ldx #0
con1:
	lda (ZPtr),y
	cmp #' '
	beq abr1
	sta Buf3,x
	iny
	inx
	cpy #10
	bcc con1
	lda #' '
abr1:
	ldy #10
	cmp (ZPtr),y		;is the extension also blank?
	beq end			;if so, do not insert the dot.
	lda #'.'
	sta Buf3,x
	inx
cn2:
	lda (ZPtr),y
	cmp #' '
	beq end
	sta Buf3,x
	inx
	iny
	cpy #13
	bne cn2
end:
	lda #$9b
	sta Buf3,x
	jsr LoadBuf3Ptr
	rts
.endproc
;;; *** FindSlash
;;; *** In the buffer (ZPtr)+Y, find the next '/' and return with C=1 if found.
;;; *** Otherwise, return with C=0, and A=EOL
.proc	FindSlash
get2:
	lda (ZPtr),y
	cmp #$9b
	beq exit
	cmp #'/'
	beq found
	iny
	bne get2
	lda #$9b		;end of buffer
exit:
	clc
found:	
	rts
.endproc
;;; *** GetArgumentExtension
;;; *** Remove the /A extension from the EOL terminated buffer (X,Y). If found, return with C=1
;;; *** and remove it.
.proc	GetArgumentExtension
	jsr SetZPtr
	sta ZTemp
	ldy #0
get2:
	jsr FindSlash
	bcc exit
	iny
	lda (ZPtr),y
	cmp ZTemp
	bne get2
	dey
	lda #$9B		;replace
	sta (ZPtr),y
	lda ZTemp
	sec
exit:	
	rts
.endproc
;;; *** GetFileCountExtension
;;; *** Find a /n extension, n=1..9 and return it in A if found, then set C=1
;;; *** Otherwise, return with C=0.
.proc	GetFileCountExtension
	jsr SetZPtr
	ldy #0
get2:
	jsr FindSlash
	bcc exit
	iny
	lda (ZPtr),y
	cmp #'1'
	bcc get2
	cmp #'9'+1
	bcs get2
	;; will not be removed
	sec
exit:	
	rts
.endproc	
;;; *** CopyBuffer2Buffer
;;; *** Copy buffer X to Y
.proc	CopyBuffer2Buffer
	tya
	asl a
	tay
	lda BufferAddress,y
	sta fr0
	lda BufferAddress+1,y
	sta fr0+1
	txa
	asl a
	tax
	lda BufferAddress+1,x
	tay
	lda BufferAddress,x
	tax
	lda #40
	jsr Copy
	rts
BufferAddress:
	.word InputBuffer,Buf1,Buf2,Buf3
.endproc
;;; *** VBI
;;; *** The vertical blank IRQ
.proc	VBI
	lda #8
	sta Console
	lda AllowSwitch
	bmi dl2
	lda DisplaySwitch
	bmi wtoff
	lda Console
	cmp #5
	bne ninv
	lda DisplaySwitch
	eor #$81
	sta DisplaySwitch
ninv:
	lda DisplaySwitch
	ldy #>DList1
	ldx #<DList1
	lsr a
	bcs install
dl2:
	ldy #>DList2
	ldx #<DList2
install:
	stx DListShadow
	stx DList
	sty DListShadow+1
	sty DList+1
exit:	
	jmp ImmediateVBIVector
wtoff:
	lda Console
	cmp #7
	bne exit
	lda DisplaySwitch
	and #$7f
	sta DisplaySwitch
	bpl exit
.endproc
;;; *** VBIInit
;;; *** Install the VBI vector
.proc	VBIInit
	ldx #0
	stx AllowSwitch
	inx
	stx DisplaySwitch
	jsr ReInstallVBI
	rts
.endproc
;;; *** ReInstallVBI
;;; *** Install or Re-Install the VBI
.proc	ReInstallVBI
	lda #6			;immediate VBI
	ldx #>VBI
	ldy #<VBI
	jsr SetIRQVector
	rts
.endproc
;;; *** InstallDisplay
;;; *** Restore the display if altered
.proc	InstallDisplay
	lda GfxMode
	beq iszero
	jsr Graphics0
iszero:
	jsr ReInstallVBI
	lda #0
	sta LeftMargin
	sta CursorColumn
	sta CursorColumn+1
	lda #39
	sta RightMargin
	lda #23
	sta CursorRow
	rts
.endproc
;;; *** Graphics0
;;; *** Re-open IOCB #0 for the editor
.proc	Graphics0
	lda #CmdClose
	ldx #0
	sta IOCBCmd
	jsr CIOVector
	lda #CmdOpen
	sta IOCBCmd
	lda #<EDevice
	sta IOCBAdr
	lda #>EDevice
	sta IOCBAdr+1
	lda #12
	sta IOCBAux1
	lda #0
	sta IOCBAux2
	jsr CIOVector
	lda DListShadow
	sta OsDList
	lda DListShadow+1
	sta OsDList+1
	rts
.endproc
;;; *** RestoreOsDisplay
;;; *** Restore the original Os display
.proc	RestoreOsDisplay
	lda #6			;immediate
	ldx #>ImmediateVBIVector
	ldy #<ImmediateVBIVector
	jsr SetIRQVector
	lda OsDList
	sta DListShadow
	lda OsDList+1
	sta DListShadow+1

	lda #$ca
	sta Color1Shadow
	lda #$94
	sta Color2Shadow
	lda #0
	sta ColorBackShadow

	lda #2
	sta LeftMargin
	sta CursorColumn
	lda #0
	sta CursorColumn+1
	jsr CursorOn

	lda #1
	ldx #<ClrScreen
	ldy #>ClrScreen
	jsr Print

	jsr WaitVBI
	rts
ClrScreen:	.byte $7d
.endproc
;;; *** WaitVBI
;;; *** Wait for a VBI to happen
.proc	WaitVBI
	lda Clock
wt:	cmp Clock
	beq wt
	rts
.endproc
;;; *** GenerateDisplayList
;;; *** Generate the display list at ZPtr with X rows representing the menu
.proc	GenerateDisplayList
	stx Z2Ptr

	jsr WaitVBI
	
	lda ZPtr+1
	pha
	lda ZPtr
	pha		 ;keep for later
	lda #$70	 ;blank8
	jsr Put
	lda #$42
	jsr Put
	lda #<Title
	jsr Put
	lda #>Title
	jsr Put
	
	lda GfxOrigin
	sta ZTemp
	lda GfxOrigin+1
	sta ZTemp+1

	lda #24
	sta Z2Ptr+1
	ldx Z2Ptr		;number of menu lines
	beq endmenu
	
	lda #$42
	jsr Put
	lda #<MenuScreenMemory
	jsr Put
	lda #>MenuScreenMemory
	jsr Put
	jsr IncLine
writemenu:	
	dec Z2Ptr
	beq endmenu
	lda #2
	jsr Put
	jsr IncLine
	clc
	bcc writemenu
endmenu:
	lda #$42
	jsr Put
	lda #<HeadLine
	jsr Put
	lda #>HeadLine
	jsr Put
	
	lda #$42
	jsr Put
	lda ZTemp
	jsr Put
	lda ZTemp+1
	jsr Put
	
	dec Z2Ptr+1
	beq end3
lp3:
	lda #2
	jsr Put
	dec Z2Ptr+1
	bne lp3
end3:
	lda #$41
	jsr Put
	pla
	jsr Put
	pla
	jsr Put

	ldy #39
settitle:
	lda TitleString,y
	cmp #$60
	bcs inverse
	sbc #$1f
	bcs inverse
	adc #$60
inverse:
	ora #$80
	sta Title,y
	dey
	bpl settitle
	rts
;;; Write a display list instruction
Put:
	ldy #0
	sta (ZPtr),y
	inc ZPtr
	bne exit
	inc ZPtr+1
exit:
	rts
;;; Advance to the next line of the regular screen
;;; in order to skip it
IncLine:
	lda ZTemp
	clc
	adc #40
	sta ZTemp
	bcc noc
	inc ZTemp+1
noc:
	dec Z2Ptr+1		;one row less
	rts
.endproc	
;;; *** MakeDisplay
;;; *** Create the display
.proc	MakeDisplay
	jsr ClearDisplayRegion
	ldx #<DList1
	ldy #>DList1
	jsr SetZPtr
	ldx #0
	jsr GenerateDisplayList
	ldx #<DList2
	ldy #>DList2
	jsr SetZPtr
	ldx #0
	jsr GenerateDisplayList	;ditto for the secodn
	rts
ClearDisplayRegion:
	ldx #<DList1
	ldy #>DList1
	jsr SetZPtr
	ldx #4
	ldy #0
	tya
clr1:
	sta (ZPtr),y
	iny
	bne clr1
	inc ZPtr+1
	dex
	bne clr1
	rts
.endproc
;;; *** GetDirectoryList
;;; *** Read the contents of the directory through IOCB #1, filespec in (x,y)
;;; *** Return the matching files on the heap in (x,y)
.proc	GetDirectoryList
	stx FileSpec
	sty FileSpec+1
	jsr GetFileCountExtension
	sta fr0+2
	lda #0
	sta FileCounter
	ldx #$10
	jsr SetIOCB
	jsr Close
	ldx FileSpec
	ldy FileSpec+1
	jsr AddDevice

	;; Filter out a couple of cases
	ldy #0
	lda (FileSpec),y
	cmp #'C'
	beq nodisk2
	cmp #'E'
	beq nodisk2
	cmp #'S'
	beq nodisk2
	cmp #'R'
	beq nodisk2
	cmp #'K'
	bne disk
nodisk2:	
	jmp nodisk
disk:

	jsr LoadBuf1Ptr
	stx fr0
	sty fr0+1
	ldx FileSpec
	ldy FileSpec+1
	lda #4			;copy the source device to Buf1
	jsr Copy
	
	ldx FileSpec
	ldy FileSpec+1
	lda #6			;directory, no headline
	jsr Open
	cpy #$a0		;a file system specific error?
	bcs nodisk2		;if so, do not handle as a disk
	
	jsr CheckIOError
	ldx #0
	ldy #0			;no bytes so far
	jsr Reserve
	stx fr0
	sty fr0+1
loop:
	ldx #40
	ldy #0
	jsr SetLength
	jsr LoadInputBufferPtr
	jsr SetIOCBAdr
	lda #CmdGetRecord
	jsr RunCIOCmd
	jsr CheckIOError
	ldx #32
	ldy #0
	jsr Reserve
	stx FileSpec
	sty FileSpec+1
	ldy #0
	lda #0
	sta (FileSpec),y
	lda InputBuffer+1
	cmp #' '		;blank unless it is free sectors
	bne eof
	
	lda InputBuffer+2	;headline?
	bpl valid
	
	ldx FileSpec		;do not keep the headline
	ldy FileSpec+1
	jsr Dispose
	clc
	bcc loop
valid:	
	inc FileCounter
	
	jsr LoadInputBufferPtr
	jsr ConvertDirectoryEntry ;to a filename
	ldy #1
	;; copy the device specification
storeploop:
	lda Buf1-1,y
	sta (FileSpec),y
	iny
	cmp #':'		;up to the colon
	bne storeploop
	ldx #0
storelp:
	lda Buf3,x
	sta (FileSpec),y
	inx
	iny
	cmp #$9b
	bne storelp
	cmp fr0+2		;do we have a file count extension?
	beq loop		;if not, continue
	lda #'/'
	dey
	sta (FileSpec),y	;if so, add it
	iny
	lda fr0+2
	sta (FileSpec),y
	iny
	lda #$9b
	sta (FileSpec),y	;and done
	bmi loop
eof:
	lda #$9b
	ldy #1
	sta (FileSpec),y
	lda FileCounter
	beq errornomatch
	jsr Close
	ldx fr0
	ldy fr0+1
	rts
errornomatch:
	ldy #FileNotFound
	jmp Error
nodisk:
	;; This is not a disk, insert the pattern directly.
	jsr Close
	ldx #64
	ldy #0
	jsr Reserve
	stx fr0
	sty fr0+1
	ldy #0
	tya
	sta (fr0),y
	ldy #$20
	sta (fr0),y
	iny
	lda #$9b
	sta (fr0),y
	ldy #0
copydirect:
	lda (FileSpec),y
	iny
	sta (fr0),y
	cmp #$9b
	bne copydirect
	ldx fr0
	ldy fr0+1
	rts
.endproc
;;; *** GetDirectoryEntry
;;; *** Read the directory entry on unit #A from the directory list (x,y)
.proc	GetDirectoryEntry
	jsr SetZPtr
	sta Z2Ptr
	ldx #3
	;; keep the default device length
lsc:	lda DefaultDeviceLen,x
	sta fr1,x
	dex
	bpl lsc
	lda Z2Ptr
	jsr SetDefaultUnit
get:
	ldy #0
	lda (ZPtr),y
	bpl notused
	lda ZPtr
	clc
	adc #$20
	sta ZPtr
	bcc get
	inc ZPtr+1
	bne get
notused:
	lda ZPtr
	sta fr0+2
	lda ZPtr+1
	sta fr0+3
	iny
	lda (ZPtr),y
	cmp #$9b
	beq end
	jsr LoadInputBufferPtr
	stx fr0
	sty fr0+1
	ldx ZPtr
	ldy ZPtr+1
	inx
	bne inc2
	iny
inc2:
	lda #40
	jsr Copy
	jsr LoadInputBufferPtr
	jsr AddDevice
	clc
	Skip1
end:	sec
	php
	;; restore the default.
	ldx #3
rsloop:	
	lda fr1,x
	sta DefaultDeviceLen,x
	dex
	bpl rsloop
	plp
	ldx fr0+2
	ldy fr0+3
	rts
.endproc
;;; *** NextDirectoryEntry
;;; *** Advance the pointer at X to point to the next directory entry,
;;; *** Return C=1 if this is the last one.
.proc	NextDirectoryEntry
	clc
	lda $0,x
	adc #$20
	sta $0,x
	sta ZPtr
	bcc noinc
	inc $1,x
noinc:
	lda $1,x
	sta ZPtr+1
	ldy #1
	lda (ZPtr),y
	cmp #$9b
	beq exit
	clc
exit:
	rts
.endproc
;;; *** GetBufferedLength
;;; *** return the length of the current directory entry in X,Y
.proc	GetBufferedLength
	ldy #$1e
	lda (fr0+2),y
	tax
	iny
	lda (fr0+2),y
	tay
	rts
.endproc
;;; *** SetBufferedLength
;;; *** Remember the read number of bytes X,Y in
;;; *** the current entry.
.proc	SetBufferedLength
	tya
	ldy #$1f
	sta (fr0+2),y
	dey
	txa
	sta (fr0+2),y
	rts
.endproc
;;; *** GetUnitNumber
;;; *** Return the default unit number of the filespec (x,y) in A
.proc	GetUnitNumber
	stx FileSpec
	sty FileSpec+1
	jsr AddDevice
	ldy #1
	lda (FileSpec),y
	cmp #':'
	beq unitone
	rts
unitone:
	lda #'1'
	rts
.endproc
;;; *** GetDefaultDevice
;;; *** Return the default device in X,Y
.proc	GetDefaultDevice
	jsr SetZPtr
	ldy DefaultDeviceLen
	lda #$9b
cp:	sta (ZPtr),y
	lda DefaultDevice-1,y
	dey
	bpl cp
	rts
.endproc
;;; *** SetDefaultDevice
;;; *** Set the default device from the buffer X,Y
;;; *** return C=0 if ok, or C=1 if the buffer does not
;;; *** contain a device
.proc	SetDefaultDevice
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	beq nodevice		;NUL marks the end of HATABS and is not a valid device.
	bmi nodevice
	iny
	lda #':'
	cmp (ZPtr),y		;the unit number?
	beq onechar
	iny
	cmp (ZPtr),y		;at position 3?
	bne nodevice
	dey
	lda (ZPtr),y
	cmp #'1'
	bcc nodevice
	cmp #'9'+1
	bcs nodevice
	Skip2
onechar:
	lda #'1'
	sta DefaultDevice+1
	ldy #0
	lda (ZPtr),y
	sta DefaultDevice
	lda #':'
	sta DefaultDevice+2
	lda #3
	sta DefaultDeviceLen
	clc
	rts
nodevice:
	sec
	rts
.endproc	
;;; *** SetDefaultUnit
;;; *** Set the default unit to #A
.proc	SetDefaultUnit
	sta DefaultDevice+1
	lda #3
	sta DefaultDeviceLen
	lda #'D'
	sta DefaultDevice
	lda #':'
	sta DefaultDevice+2
	rts
.endproc
;;; *** InstallDestructor
;;; *** Install the destructor at X,Y into the
;;; *** destructor chain.
.proc	InstallDestructor
	jsr SetZPtr
	ldy #4
	lda FirstDestructor
	sta (ZPtr),y
	iny
	lda FirstDestructor+1
	sta (ZPtr),y
	lda ZPtr
	sta FirstDestructor
	lda ZPtr+1
	sta FirstDestructor+1
	rts
.endproc
;;; *** Reset
;;; *** The Reset goes here: Actually, this is the new DUP
;;; *** entry vector
.proc	Reset
	lda #3
	sta KeyDelay
	lda #$ff
	sta KeyCodeShadow

	lda FmsOvl
	beq noovl
	jsr ResetEntry
noovl:	

	lda OldAppMemHi
	sta AppMemHi
	lda OldAppMemHi+1
	sta AppMemHi+1

	lda #<RTSVector
	sta FirstDestructor
	lda #>RTSVector
	sta FirstDestructor+1
	jmp Init
.endproc
;;; *** RemoveMenu
;;; *** Remove the menu infrastructure, but do
;;; *** not change the cart switch since this
;;; *** might destroy the display
.proc	RemoveMenu
	jsr RestoreOsDisplay
	lda OldDupVector
	sta DupVector
	lda OldDupVector+1
	sta DupVector+1
	lda FmsBootFlag
	and #$bf		;do not run the DUP after removing the Os
	sta FmsBootFlag
	rts
.endproc
;;; *** InstallMenu
;;; *** Re-install the menu vectors after a binary load
.proc	InstallMenu
	jsr InstallDisplay
	
	lda #<Reset
	sta DupVector
	lda #>Reset
	sta DupVector+1

	;; run Dup through reset
	lda FmsBootFlag
	ora #$40
	sta FmsBootFlag
	rts
.endproc
;;; *** RemoveDUP
;;; *** Remove the current DUP and continue operation normally
.proc	RemoveDUP
	jsr RemoveMenu
	
	lda CartsDisabled
	beq continue

	;; check whether we should re-enable the carts:
	;; do so here.
	lda OldOssSwitch
	beq nomodload
	ldx #0
	jsr SwitchOss
nomodload:
	lda OldBasSwitch
	sta BasicDisabled
	lda OldCartSum
	sta CartSum
continue:
	;; Ensure that the cart sees the reset.
	lda FmsBootFlag
	ora #1			;erase application area
	sta FmsBootFlag
	rts
.endproc
;;; *** SwitchOss
;;; *** Switch an Oss super cartridge on or off
.proc	SwitchOss
	lda #0
	sta NMIEnable
	sta $d500,x		;cartctrl
	lda Trigger3
	sta Trigger3Shadow
	lda #$40
	sta NMIEnable
	rts
.endproc
;;; *** DisableCarts
;;; *** Disable all the cartridges
.proc	DisableCarts
	lda Trigger3Shadow
	bne havecart
	lda BasicDisabled
	bne nobasic		;nothing to disable
havecart:	
	inc CartsDisabled

	lda #1
	sta BasicDisabled
	ldx #8
	jsr SwitchOss
	jmp WarmStartVector
nobasic:
	rts
.endproc
;;; *** ResetDefaultDevice
;;; *** Restore the default device to its default.
.proc	ResetDefaultDevice
	lda #3
	sta DefaultDeviceLen
	lda #'D'
	sta DefaultDevice
	lda #'1'
	sta DefaultDevice+1
	lda #':'
	sta DefaultDevice+2
	rts
.endproc
;;; *** Start
;;; *** The program starts here
.proc 	Start
	lda #0
	sta InExtended
	sta CartsDisabled
	sta FmsOvl

	jsr ResetDefaultDevice
	
	ldx #$ff
	stx STAX		;stack

	lda DupVector
	sta OldDupVector
	lda DupVector+1
	sta OldDupVector+1

	lda #<Reset
	sta DupVector
	lda #>Reset
	sta DupVector+1

	;; run Dup through reset
	lda FmsBootFlag
	ora #$40
	sta FmsBootFlag
	
	lda AppMemHi
	sta OldAppMemHi
	lda AppMemHi+1
	sta OldAppMemHi+1
	
	lda Trigger3Shadow
	sta OldOssSwitch
	lda BasicDisabled
	sta OldBasSwitch
	lda CartSum
	sta OldCartSum
	
	;; check whether the FMS is under the OS

	lda #$20		;must be a JSR
	cmp RegularEntry
	bne noswitch
	cmp POBEntry
	bne noswitch
	lda #$4c		;must be a JMP
	cmp RegularEntry+6
	bne noswitch
	cmp POBEntry+6
	bne noswitch
	
	lda RegularEntry+1
	cmp POBEntry+1
	bne noswitch
	lda RegularEntry+2
	cmp POBEntry+2
	bne noswitch

	lda RegularEntry+7
	cmp POBEntry+7
	bne noswitch
	lda RegularEntry+8
	cmp POBEntry+8
	bne noswitch
	inc FmsOvl
	bne fmsundercart
noswitch:
	;; fms is not under RAM. Disable the carts
	jsr DisableCarts
fmsundercart:	
	jmp WarmStartVector
.endproc
;;; *** SetupGraphics
;;; *** Install the graphical screen
.proc	SetupGraphics
	jsr OpenKeyboard
 	jsr SetRegularColor
	jsr ClearHeadLine
	jsr VBIInit

	lda #0
	sta LeftMargin
	lda #0
	sta CursorColumn
	sta CursorColumn+1
	lda #23
	sta CursorRow
	rts
.endproc
;;; *** Main
;;; *** The Main program loop
.proc	Init
	bit InExtended
	bvc willbesetup
	jsr Graphics0
	jsr SetupGraphics
willbesetup:
.endproc
	;; runs into here
.proc	Main
	lda BreakFlag
	bne isfine
	bit InExtended
	bpl reload
isfine:	
	bit InExtended
	bvs mainloop
reload:
	lda #0
	sta InExtended
	
	jsr Graphics0
	jsr MakeDisplay
	jsr SetupGraphics

	ldx #<LoadingTxt
	ldy #>LoadingTxt
	lda #LoadingTxtL
	jsr Print
	jsr ResetMenuPoints
	jsr LoadMenu
	lda #$FF
	sta InExtended
	bmi clrheadline
clearscreen:
	lda #1
	sta DisplaySwitch
	ldx #<Clear
	ldy #>Clear
	lda #1
	jsr Print
clrheadline:
	jsr ClearHeadLine
mainloop:
	lda #0
	sta LeftMargin
	lda #0
	sta CursorColumn
	sta CursorColumn+1
	lda #23
	sta CursorRow
	sta BreakFlag
	jsr PrintEOL
	jsr CloseAll
	jsr ReleaseAllMemory

	bit InExtended
	bmi normal
	ldx #<SelectExtended
	ldy #>SelectExtended
	lda #SelectExtendedL
	bne print
normal:	
	ldx #<SelectItem
	ldy #>SelectItem
	lda #SelectItemL
print:	
	jsr Print
	jsr CursorOff
	jsr UnblockDisplay
	lda #$ff
	sta KeyCodeShadow
	jsr EnableBREAK
	jsr SetRegularColor
waitkey:
	lda KeyCodeShadow
	cmp #$ff
	bne cont
	lda BreakFlag
	bmi waitkey
cont:
	jsr ClearHeadLine
	jsr CopyJumpTable

	jsr GetKey
	pha
	ldx #<ClearLine
	ldy #>ClearLine
	lda #2
	jsr Print
	pla
	and #$5f
	cmp #$0f		;is '/' for extended?
	beq ExtendedCommand
	cmp #$1b		;EOL
	beq clearscreen
	cmp LastMenuPoint
	bcs normal
	sec
	sbc #'A'
	bcc normal
	asl a
	tax
	jsr RunMenuEntry
	jmp mainloop
ExtendedCommand:	
	ldx #<InExtendedT
	ldy #>InExtendedT
	lda #InExtendedTL
	jsr Print
	jsr CursorOn
	jsr LoadBuf1Ptr
	jsr GetLine
	jsr CursorOff
	ldx #$20
	jsr SetIOCB
	jsr Close
	jsr LoadBuf1Ptr
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	cmp #$9b
	beq tomain
	jsr LoadBuf1Ptr
	jsr AddDevice
	jsr LoadBuf1Ptr
	lda #4			;for reading
	jsr Open
	jsr CheckIOError
	lda #$0
	sta InExtended
	jsr ResetMenuPoints
	jsr MakeDisplay
	jsr LoadMenuItem
	jsr PrintEOL
	lda #$40
	sta InExtended
tomain:	
	jmp mainloop
.endproc
;;; *** ResetMenuPoints
;;; *** Unload all menu points
.proc	ResetMenuPoints
	jsr CallDestructors
	lda #<RTSVector
	sta FirstDestructor
	lda #>RTSVector
	sta FirstDestructor+1
	lda #'A'
	sta LastMenuPoint
	lda #0
	sta MenuItemCntr
	lda # <MenuScreenMemory
	sta MenuOrigin
	lda # >MenuScreenMemory
	sta MenuOrigin+1
	lda # <LastMenuAddress
	sta DupEnd
	sta AppMemHi
	lda # >LastMenuAddress
	sta DupEnd+1
	sta AppMemHi+1
	rts
CallDestructors:
	jmp (FirstDestructor)
.endproc
;;; *** RunMenuEntry
;;; *** Run the external menu item indexed by X
.proc	RunMenuEntry
	lda MenuEntryPoints,X
	sta ZPtr
	lda MenuEntryPoints+1,X
	sta ZPtr+1
	jmp (ZPtr)
.endproc
;;; *** CopyJumpTable
;;; *** Copy the jump table to the dup menu helper functions to page 6
.proc	CopyJumpTable
	ldx #$ff
copy:	
	lda JUMPTAB,x
	sta MenuEntryVectors,x
	dex
	bne copy
	rts
.endproc
;;; *** LoadMenu
;;; *** Load the menu points from disk in the order they appear in MENULST.SYS
.proc	LoadMenu
	ldx #<LoadMessage
	ldy #>LoadMessage
	lda #LoadMessageL
	jsr SetHeadLineASCII
	ldx #$10
	jsr SetIOCB
	jsr Close
	ldx #<MenuFilespec
	ldy #>MenuFilespec
	lda #4
	jsr Open
	bmi menuerror
loadcnt:
	ldx #$10
	jsr SetIOCB
	jsr LoadBuf1Ptr
	jsr SetIOCBAdr
	ldx #40
	ldy #0
	jsr SetLength
	lda #CmdGetRecord
	jsr RunCIOCmd
	cpy #EndOfFile
	beq end
	cpy #0
	bmi menuerror
	ldx #$20
	jsr SetIOCB
	jsr Close
	jsr LoadBuf1Ptr
	jsr AddDevice
	jsr LoadBuf1Ptr
	lda #4
	jsr Open
	bmi menuerror
	jsr LoadMenuItem
	jmp loadcnt
end:
	jsr Close
	rts
menuerror:
	ldx #$10
	jsr SetIOCB
	jsr Close
	ldx #$20
	jsr SetIOCB
	jsr Close
	jsr OpenKeyboard
	jsr ClearHeadLine
	ldx #<MenuError
	ldy #>MenuError
	lda #MenuErrorL
	jsr SetHeadLineASCII
	jmp SystemError
.endproc
;;; *** SystemError
;;; *** Serious System error, ask user to retry or abort
.proc	SystemError
	jsr CloseAll
	lda OldAppMemHi
	sta AppMemHi
	lda OldAppMemHi+1
	sta AppMemHi+1
	jsr GetKey
	cmp #'Q'
	beq quit
	lda #0
	sta InExtended
	jsr ResetDefaultDevice
	jmp WarmStartVector
quit:
	jmp RunCommandLine
.endproc
;;; *** LoadMenuItem
;;; *** Load a menu item into RAM
.proc	LoadMenuItem
	lda DupEnd
	beq isrounded
	lda #0
	sta DupEnd
	sta AppMemHi
	inc DupEnd+1
	inc AppMemHi+1
isrounded:
	ldx #$20
	jsr SetIOCB
	ldx #2
	ldy #0
	jsr SetLength
	ldx #<ZPtr
	ldy #>ZPtr
	jsr BGet
	bmi error3
	;; magic header is EOL EOL
	lda #$9b
	cmp ZPtr
	bne error3
	cmp ZPtr+1
	beq continue
error3:	jmp error
continue:
	ldx #2
	ldy #0
	jsr SetLength
	ldx #<ZPtr
	ldy #>ZPtr
	jsr BGet
	bmi error3

	lda ZPtr		;end of entry table?
	ora ZPtr+1
	beq fintable

	;; is offset to menu function 
	lda MenuItemCntr
	asl a
	tax
	lda ZPtr
	clc
	adc DupEnd
	sta MenuEntryPoints,x
	lda ZPtr+1
	adc DupEnd+1
	sta MenuEntryPoints+1,x
	ldx #<DList1
	ldy #>DList1
	jsr SetZPtr
	;; enlarge the menu
	inc MenuItemCntr
	lda MenuItemCntr
	lsr a
	tax
	bcc noincline
	inx
noincline:	
	jsr GenerateDisplayList

	lda LastMenuPoint
	sec
	sbc #$20		;to ANTIC
	ldy #$00
	sta (MenuOrigin),y
	iny
	lda #$0e		;dot
	sta (MenuOrigin),y

	ldx #18
	ldy #0
	jsr SetLength
	lda MenuOrigin
	clc
	adc #2
	tax
	ldy MenuOrigin+1
	bcc noiny
	iny
noiny:
	jsr BGet		;read menu name as ANTIC string
	bmi error2
	inc LastMenuPoint
	
	lda MenuOrigin
	clc
	adc #20			;next entry
	sta MenuOrigin
	bcc noinc
	inc MenuOrigin+1
noinc:
	jmp continue
	;; read init address
fintable:
	ldx #<fr0
	ldy #>fr0
	jsr BGet
	bmi error2
	;; read length
	ldx #<ZPtr
	ldy #>ZPtr
	jsr BGet
error2:	bmi error

	ldx ZPtr
	ldy ZPtr+1
	jsr SetLength
	;; allocate memory
	ldx ZPtr
	ldy ZPtr+1
	jsr Reserve
	jsr BGet
	bmi error

	;; where the thing has been loaded to
	lda DupEnd
	sta ZPtr
	lda DupEnd+1
	sta ZPtr+1
	lda #1
	sta Z2Ptr
	ldx #0
	ldy #0
	jsr SetLength		;individual bytes
	;; relocate data to place
relocationloop:
	lsr Z2Ptr
	bcc nextbit
	ror Z2Ptr		;reset to $80
	jsr BGet
	cpy #EndOfFile
	beq relocationdone
	cpy #0
	bmi error
	sta Z2Ptr+1		;relocation data
nextbit:
	lda Z2Ptr+1
	and Z2Ptr		;relocate me?
	beq direct
	;; here relocate
	ldy #0
	lda (ZPtr),y
	clc
	adc DupEnd+1
	sta (ZPtr),y
direct:
	inc ZPtr
	bne relocationloop
	inc ZPtr+1
	bne relocationloop
relocationdone:
	jsr Close
	;; fixup init address
	lda fr0
	clc
	adc DupEnd
	sta fr0
	lda fr0+1
	adc DupEnd+1
	sta fr0+1
	;; advance DupEnd
	lda AppMemHi
	sta DupEnd
	lda AppMemHi+1
	sta DupEnd+1
	;; Now construct the submenu
	jsr CopyJumpTable
	jsr ToInit
	rts
ToInit:	jmp (fr0)
error:	jmp SystemError
.endproc
;;; **** End of the program area
;;; Texts
TitleString:	.byte "THOR-Dup Version 2.++ (c) 1990,2013 THOR"

ClearLine:	.byte $1c,$1c+$80
SelectItem:	.byte 155,"Select item or "
		.byte 'R'+$80,'E'+$80,'T'+$80,'U'+$80,'R'+$80,'N'+$80
		.byte " to clear",155
SelectItemL	=	*-SelectItem

SelectExtended:	.byte 155,"Select item or "
		.byte 'B'+128,'R'+128,'E'+128,'A'+128,'K'+128
		.byte " to return",155
SelectExtendedL	=	*-SelectExtended
	
Clear:		.byte $7d
	
LoadingTxt:	.byte 155,155,"Type / on keyboard to load a menu",155
		.byte "function exclusively."
		.byte 155,155,"Loading menu functions, please wait..."
LoadingTxtL	=	*-LoadingTxt
	
InExtendedT:	.byte "Which menu to load :"
InExtendedTL 	= 	*-InExtendedT

OutOfMemoryE:	.byte "Out of memory, Q to Quit, R to retry"
OutOfMemoryEL	=	*-OutOfMemoryE
	
LoadMessage:	.byte	"loading menu..."
LoadMessageL	=	*-LoadMessage
	
MenuFilespec:	.byte	"D:MENULST.SYS",155
	
MenuError:	.byte "Load error, hit Q to quit,R to retry."
MenuErrorL	=	*-MenuError

ParamError:	.byte "Parameter error !"
ParamErrorL 	=	*-ParamError

ErrorText:	.byte "Error - "
ErrorValue:	.byte "           "
ErrorTextRead:	.byte "               "
ErrorTextL	=	*-ErrorText
	
MoveCursor:	.byte 31,30	;ATASCII for cursor right, cursor left
KeySpec:	.byte "K:",$9b
EDevice:	.byte "E:",$9b
EOL:		.byte 155

;;; *** Table with all the error codes
ErrorTable:
	.byte BreakError,     "break abort    "
	.byte NoBinaryFile,   "can't load     "
	.byte TimeoutError,   "device timeout "
	.byte FrameError,     "frame error    "
	.byte DeviceError,    "device error   "
	.byte ChkSumError,    "checksum error "
	.byte DeviceNak,      "device nak     "
	.byte IllegalUnit,    "illegal unit   "
	.byte DirectoryFull,  "directory full "
	.byte DiskFull,	      "disk full      "
	.byte FileProtected,  "file protected "
	.byte FileNotFound,   "file not found "
	.byte NotADosDisk,    "not a dos disk "
	.byte FileNameInvalid,"invalid name   "
	.byte 0	
;;; private globals
Title:			.res 40
OldAppMemHi:		.word 0	;what AppMemHi should be
OldBasSwitch:		.byte 0	;the Os basic disable switch
OldCartSum:		.byte 0	;the cart checksum
OldOssSwitch:		.byte 0 ;switch flag for Oss cartriges
CartsDisabled:		.byte 0	;if one, then carts have been disabled
FmsOvl:			.byte 0	;if one, then FMSOVL is active
OldDupVector:		.word 0	;where the DUP should go
FirstDestructor:	.word 0	;first destructor to call (on the chain)
OsDList:		.word 0 ;display list installed by the Os
	LastMenuAddress	=	*
	.global	MenuLength
MenuLength	=	*-StartAddress

