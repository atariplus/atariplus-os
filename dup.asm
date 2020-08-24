;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: dup.asm,v 1.29 2016/11/10 20:07:35 thor Exp $		**
;;; **									**
;;; ** In this module:	 Minimal DUP command line parser		**
;;; **********************************************************************

	.include "fms.i"
	.include "kernel.i"
	.include "dup.i"
	.include "cio.i"
	.include "reset.i"
	.include "errors.i"
	.include "nmi.i"
	.include "irq.i"
	.include "misc.i"
	.include "pokey.i"
	
	.segment  "SelfTest"
	
;;; *** Skip two bytes (by a dummy BIT)
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro
	
;;; ***
;;; *** DUP init vector. The DUP launches right here
;;; ***
	.global DupInit
.proc	DupInit
	;; initialize the input buffer with the device name
	jsr InitDriveName
	lda FmsBootFlag
	and #$bf		; remove the "launch dup" flag now
	sta FmsBootFlag
	ldy #<Title
	lda #>Title
	ldx #TitleL
	jsr Print		; print the header message
	;; runs into the following
.endproc
;;; *** DupLoop
;;; *** This is the main loop of the DUP
.proc	DupLoop
	jsr PrintLF		; a separator
	;; check whether the device is valid. If not,
	;; it has been overwritten.
	lda DupBuffer+2
	cmp #':'		; this should contain a colon
	bne notfine
	lda DupBuffer+1
	cmp #'1'
	bcc notfine
	cmp #'9'+1
	bcc isfine
notfine:	
	jsr InitDriveName
isfine:
	lda #>(DupLoop-1)	; write a return address (here!)
	pha
	lda #<(DupLoop-1)
	pha
	jsr CloseChannel
	ldy #<DupBuffer
	lda #>DupBuffer
	ldx #DirInitL
	jsr Print		; print the "prompt", namely the directory name
	ldx #0
	jsr AssignInputBuf
	lda #64-3		; size of the buffer
	sta IOCBLen,x
	lda #CmdGetRecord
	sta IOCBCmd,x
	jsr CIOVector		; read the user command
	bmi repeat2		; request input another time on error
	ldy #3			; start at the separation
	jsr SkipBlanks
	beq repeat2		; run again if there is no input
	lda DupBuffer+3		; is the first character
	cmp #$22		; a quote?
	bne keepit		; if so, do not use external commands
	ldy #1+3
	ldx #3
	jsr MoveBuffer		; skip this part
	beq internal		; jumps always
keepit:	
	;; First try is to load this as a command
	;; This is required to override the internal commands by external ones
	ldx #0			; into the primary buffer
	jsr SetDevice		; extract the source directory of the command and set it
	ldx #3			; move the command to buffer+3
	jsr MoveBuffer		; drop the device if there is one
	ldy #3
	jsr CheckForEOL		; test if this is all. If so, then just a directory change
	beq repeat2
	ldx #$10
	lda #CmdBload
	sta IOCBCmd,x
	lda #$c0		; run & init
	sta IOCBAux1,x
	ldy #<DupBuffer
	lda #>DupBuffer		; buffer to read from
	jsr SetIOCBAddress
	jsr CIODirect		; do not use the command executer, this would call the error report if not found
	cpy #$00
	bpl repeat		; if this worked, then execute the command
	sty DupError		; keep the error code
	;; External command not available, now
	;; try the internal
internal:
	ldx #NumCommands-1
	ldy #3			; command starts now again at the zero offset
scanloop:
	lda CmdChar1,x
	cmp DupBuffer,y
	bne nextcmd
	lda CmdChar2,x
	cmp DupBuffer+1,y
	bne nextcmd
	lda CmdChar3,x
	cmp DupBuffer+2,y
	beq foundcmd
nextcmd:
	dex
	bpl scanloop

	;; The command is unknown. Signal this condition
	lda DupError
	jmp ReportError		; signal the error
repeat2:
	rts
foundcmd:
	stx DupTmp		; keep the command index
	jsr FindEndOfToken	; advance to the end of the token. There could be an argument here
	beq bufferend
	ldx #0			; insert the device again at the start of the buffer
	jsr SetDevice		; check whether we have a device name here. If so, skip it and advance to the file name
	sty DupStartAdr
	jsr FindEndOfToken	; no internal command takes extra arguments separated by spaces
	bne error	
	ldx #3
	ldy DupStartAdr
	jsr MoveBuffer		; move it to the buffer position
	ldy #3
bufferend:
	ldx DupTmp
	lda CmdHi,x
	pha
	lda CmdLo,x
	pha			; set the target jump destination	
	ldx #CmdRename		; set default command for short invocation
repeat:				; call the command and return
	rts
error:
	jmp ErrorExtraArgument
.endproc
;;; *** Commands start here:	Generic CIO commands are handled by a single jump-in
;;; *** CmdFor
;;; *** Format a disk (long format)
.proc	CmdFor
	lda #CmdFormatStandard
	Skip2
	;; runs into the following
.endproc
;;; *** CmdCle
;;; *** Clear a disk (quick format)
.proc	CmdCle
	lda #CmdInit
	pha
	jsr CheckForEOL		; do we have an option?
	bne notempty
	ldy #3
	lda #$9b		; terminate here
	sta DupBuffer,y
notempty:	
	jsr LastOption		; check whether this is the last option (must be)
	pla
	tay
	lda #0
	beq ExecuteCmdAux1
.endproc
;;; *** CmdLoa
;;; *** Load a filespec (without running it)
.proc	CmdLoa
	ldx #CmdBload
	bne CmdGenericOneArg
.endproc
;;; *** CmdUnl
;;; *** Unprotect a filespec
.proc	CmdUnl
	inx
.endproc
;;; *** CmdLoc
;;; *** Protect a filespec
.proc	CmdLoc
	inx
	inx			; two above delete
.endproc
;;; *** CmdDel
;;; ***	Delete a filespec
.proc	CmdDel			; delete a filespec
	inx			; is one above rename
.endproc
;;; *** CmdGenericOne
;;; *** Generic handling for one-argument commands
.proc	CmdGenericOneArg
	jsr CheckForEOL		; do we have an option?
	beq empty
	jsr LastOption		; check whether this is the last option (must be)
	txa			; command->Y
	tay
	lda #$00		; no Aux1
	beq ExecuteCmdAux1
empty:
	jmp ErrorMissingArgument
.endproc
;;; *** CmdRen
;;; *** Rename a filespec
.proc	CmdRen
	jsr FetchOption		; there must be at least one additional option
	jsr LastOption
	ldy #CmdRename
	lda #$00		; no Aux1
	Skip2
.endproc
;;; *** OpenChannel
;;; *** Open a channel with A = Aux1
.proc	OpenChannel
	ldy #CmdOpen
	;; runs into the following
.endproc
;;; *** ExecuteCmdAux1
;;; *** Execute a filespec with Y=command, A = Aux1
.proc	ExecuteCmdAux1
	ldx #$10
	sta IOCBAux1,x
	tya
	pha			; keep the command
	ldy #<DupBuffer
	lda #>DupBuffer		; buffer to read from
	jsr SetIOCBAddress
	lda #$0
	sta IOCBAux2,x		; clear Aux2
	pla			; get the command again
	;; runs into the following
	Skip2
.endproc
;;; *** Close the channel
.proc	CloseChannel
	lda #CmdClose
	;; runs into the following
.endproc
;;; *** ExecuteCmd
;;; *** Execute a filespec with A = command
.proc	ExecuteCmd	
	ldx #$10
	sta IOCBCmd,x
	;; runs into the following
.endproc
;;; *** InvokeCIO
;;; *** Run CIO disabling the selftest, possibly reporting an error
.proc	InvokeCIO
	jsr CIODirect		; call CIO disabling the selftest
	cpy #$00		; set flags (not done by the above)
	bpl exit
	cpy #EndOfFile		; EOF isn't reported
	bne ReportErrorY	; report the error
exit:	
	rts
.endproc
;;; *** FetchOption
;;; *** Check whether there is an next option from the present index. Error if there is not.
;;; *** Otherwise advance to it.
.proc	FetchOption
	jsr FindEndOfOption
	beq ErrorMissingArgument
	rts
.endproc
;;; *** LastOption
;;; *** Check whether the present option is the last one. If not, signal an error
.proc	LastOption
	tya
	pha			; keep the position of the option
	jsr FindEndOfOption
	bne ErrorExtraArgument
	pla
	tay			; restore its position
	rts
.endproc
;;; *** ErrorExtraArgument
;;; *** Report than we found an extra argument that doesn't belong here
.proc	ErrorExtraArgument
	ldy #ExtraArgument
	Skip2
	;; runs into the following
.endproc
;;; *** ErrorMissingArgument
;;; *** Report that we miss an argument that should be here
.proc	ErrorMissingArgument
	ldy #MissingArgument
	;; runs into the following
.endproc
;;; *** ReportErrorY
;;; *** Report the error in the Y register
.proc	ReportErrorY
	tya			; ->A
	;; runs into the following
.endproc
;;; *** ReportError
;;; *** Report the error in the A register
.proc	ReportError
	;; The error is between 1 and 255. Thus, the first digit is either 0,1 or 2
	ldx #'0'-1		; initialize first digit
hunloop:
	inx
	sec
	sbc #100		; is at least higher than 100
	bcs hunloop
	adc #100
	stx DupBuffer+3		; store the final result here
	
	ldx #'0'-1		; second digit is between 0 and 9
	sec
digit2:
	inx
	sbc #10			; subtract 10 until we get a wraparound. 
	bcs digit2
	adc #10			; correct the addition
	stx DupBuffer+4		; second digit
	and #15
	ora #'0'		; last digit
	sta DupBuffer+5
	lda #>ErrorMsg
	ldy #<ErrorMsg
	ldx #ErrorMsgL
	jsr Print		; print the message
	lda #>(DupBuffer+3)
	ldy #<(DupBuffer+3)
	ldx #3			; three characters
	jsr Print
	jsr PrintLF
	ldx #$ff
	txs			; reset the stack
	jmp DupLoop
.endproc
;;; *** ConvertHexDigit
;;; *** Convert the Hex digit in the input buffer at offset Y to binary
.proc	ConvertHexDigit
	lda DupBuffer,y
	sec
	sbc #'0'
	cmp #10
	bcc numeric
	sbc #7			; is a letter (hex A to F)
	cmp #10			; but must be at least 10 ('A')
	bcc failure
numeric:
	cmp #16			; fails also if we have a character below '0'
	bcs failure
	iny			; advance the buffer
	rts
failure:
	lda #InvalidNumber	; Invalid number error
	bne ReportError
.endproc
;;; *** ConvertHexAddress
;;; *** Convert the hex address in the input buffer at offset Y to a number in X = hi, A = Lo
.proc	ConvertHexAddress
	jsr ConvertHexByte	; get the high-byte
	tax			; keep it
	;; runs into the following
.endproc
;;; *** ConvertHexByte
;;; *** Convert the hex byte in the input buffer at offset Y to a number in A
.proc	ConvertHexByte
	jsr ConvertHexDigit
	asl a
	asl a
	asl a
	asl a			; high-nibble
	sta DupTmp		; keep me
	jsr ConvertHexDigit
	ora DupTmp		; low-nibble
	rts
.endproc
;;; *** Non-generic commands here
;;; *** CmdDir
;;; *** Show the directory contents
.proc	CmdDir
	jsr LastOption		; check that there's only one argument (at most)
	jsr CheckForEOL		; is this the only one
	bne havepattern
	sta DupBuffer+4		; advance by one
	lda #'-'
	sta DupBuffer+3		; insert the "any" pattern
havepattern:
	lda #6			; open for directory
	jsr OpenChannel
	jsr AssignInputBuf	; read into input buffer+3
dirloop:
	ldx #$10		; input channel
	lda #20
	sta IOCBLen,x		; set length
	sta IOCBLen		; also of the output channel
	lda #CmdGetRecord
	jsr ExecuteCmd		; run CIO
	cpy #EndOfFile		; on end, return (closes IOCB in main)
	beq exit
	ldx #00
	lda #CmdPutRecord
	sta IOCBCmd,x		; input buffer is still in the address
	jsr CIOVector
	bpl dirloop
exit:
	rts
.endproc
;;; *** CmdSav
;;; *** Binary save a file
.proc	CmdSav
	lda #0
	sta DupRunHeader	; no run header so far
	jsr FetchOption		; advance to the start address	
	lda #$9b
	sta DupBuffer-1,y	; terminate the filename here
	jsr ConvertHexAddress	; get start address
	sta DupStartAdr
	stx DupStartAdr+1
	jsr FetchOption
	jsr ConvertHexAddress
	sta DupEndAdr
	stx DupEndAdr+1		; keep the end address
	jsr SkipComma
	beq norunheader
	jsr ConvertHexAddress	; run or init address
	sta DupRunAdr
	stx DupRunAdr+1		; specify the run address
	sta DupInitAdr
	stx DupInitAdr+1	; also keep as init vector
	ldx #<RunVector		; where to load this to: here
	lda #>RunVector
	stx DupRunHeader
	sta DupRunHeader+1
	inx
	stx DupRunHeader+2
	sta DupRunHeader+3
norunheader:
	jsr SkipComma		; if we have another argument, then this is the real run vector
	beq noinitheader
	jsr ConvertHexAddress
	sta DupRunAdr
	stx DupRunAdr+1		; specify the run address, this is now the real thing
	inc DupRunHeader+2	; increment the target run address to include the init vector
	inc DupRunHeader+2
noinitheader:
	jsr LastOption		; no additional arguments accepted here
	;; now write the stuff out
	lda #8
	jsr OpenChannel		; open the channel for writing
	lda #0
	sta IOCBLen,x		; zero bytes, value in A
	lda #CmdPutBlock
	sta IOCBCmd,x		; put a block out
	lda #$ff		; binary file header
	jsr InvokeCIO
	jsr InvokeCIO		; header
	
	ldy #<DupStartAdr
	lda #>DupStartAdr	; write start and end address
	jsr SetIOCBAddress
	lda #4
	sta IOCBLen,x		; ditto
	jsr InvokeCIO
	
	ldy DupStartAdr
	lda DupStartAdr+1
	jsr SetIOCBAddress	; write data from here
	lda DupEndAdr
	sec
	sbc DupStartAdr
	tay			; keep low
	lda DupEndAdr+1
	sbc DupStartAdr+1	; length hi	
	bcc rangeerror		; warn if not
	iny			; bump hi
	bne nocarry
	adc #00			; should have carry set
nocarry:
	sta IOCBLen+1,x
	tya
	sta IOCBLen,x		; set length
	jsr InvokeCIO		; write the body data
	lda DupRunHeader	; do we have a run address?
	beq noruninit
	;; here we do. Save the DupRunHeader in one go
	ldy #<DupRunHeader
	lda #>DupRunHeader
	jsr SetIOCBAddress	; write the data in here
	lda DupRunHeader+2	; compute the length here
	sec
	sbc DupRunHeader	; as the difference of the low bytes
	adc #4			; has carry set. +4 for the header, +1 for inclusive/exclusive conversion
	sta IOCBLen,x
	jsr InvokeCIO		; and write the header
noruninit:
	rts			; back to the main
rangeerror:			; warn if the out of range
	lda #Range
	jmp ReportError
.endproc
;;; *** CmdRun
;;; *** Run a machine code program at a specific address
.proc	CmdRun
	jsr CheckForEOL		; do we have an argument here?
	beq noaddress
	jsr ConvertHexAddress	; convert the target to hex
	stx RunVector+1
	sta RunVector
	jsr LastOption		; check whether this is really the last one
noaddress:
	lda RunVector
	ora RunVector+1
	beq novector
	jmp RunRunVector	; run thru the RUN vector disabling the DUP for that time.
novector:
	rts
.endproc
;;; *** CmdCar
;;; *** Run the cartridge
.proc	CmdCar
	jsr CheckForEOL		; must not have any argument
	bne error
	ldx #$ff
cps:
	cpx KeyStat
	bne cps			; wait until the keyboard goes up again
	stx KeyCodeShadow	
	jmp WarmStartVector
error:
	jmp ErrorExtraArgument
.endproc
;;; *** CmdNew
;;; *** Erase the user memory, make available for copy
.proc	CmdNew
	jsr CheckForEOL		; must not have any argument
	bne error
	lda FmsBootFlag
	ora #1			; indicate that we're erasing application memory
	sta FmsBootFlag		; Yikes!
	lda #0
	sta AppMemHi
	sta AppMemHi+1
	rts
error:
	jmp ErrorExtraArgument
.endproc
;;; *** CmdCop
;;; *** Copy a file or a couple of files
.proc	CmdCop
	tya
	pha				; keep the buffer position
	ldy #0				; copy from here
	sty SourceCount			; first source file
	sty HasTarget			; no target
	sty SameDevice			; are on the same device
	ldx #DupTargetBuffer-DupBuffer	; default:	Target equals source
	jsr MoveBuffer			; copy over
	pla
	tay			; now check whether there is an additional argument
	jsr FindEndOfOption	; check whether there is a second option
	beq nooption
	lda #$9b
	sta DupBuffer-1,y	; terminate the buffer here
	sta DupTargetBuffer-1,y	; ditto in the target
	;; here we have a target. Maybe this is only a device name?
	ldx #DupTargetBuffer-DupBuffer ; copy to over here
	jsr SetDevice		; if we have it
	jsr CheckForEOL		; Is there really a filespec?
	beq checkforidentical
	jsr LastOption
	ldx #DupTargetBuffer-DupBuffer+3 ; without the device spec
	jsr MoveBuffer		; move the remaining stuff to the target
	dey
	inc HasTarget		; we do so, indeed
checkforidentical:	
	;; check whether the two devices (source, target) are identical
	lda DupTargetBuffer
	cmp DupBuffer
	bne separate
	lda DupTargetBuffer+1
	cmp DupBuffer+1
	beq nooption
separate:
	inc SameDevice		; are on separate devices
nooption:
	jsr LastOption		; but then, this must be the last option
	;; Now loop over the source files
copyloop:
	lda #8			; open mode for the file: writing
	sta DupTmp
	inc SourceCount
	;;
nextchunk:
	;; Now check whether we need to resolve a wildcard in the source
	lda HasTarget
	bne singlefile
	;; otherwise, move it over to the target without overwriting the filename
	ldy #0
	ldx #DupTargetBuffer-DupBuffer+3
	jsr MoveBuffer
		
	ldx #$10
	lda #>(DupTargetBuffer+3)
	ldy #<(DupTargetBuffer+3)
	jsr SetIOCBAddress	; install this as the address
	lda #4
	sta IOCBAux1,x
	lda SourceCount
	sta IOCBAux2,x		; set file cunter
	lda #CmdResolve		; resolve the wildcard
	sta IOCBCmd,x
	jsr CIOVector
	;; on the first call, this may fail (because the source FS doesn't support it),
	;; on the second or later, we then abort without notice
	bpl isfine
	lda SourceCount		; is this the first go? If so, ignore the error
	cmp #1
	beq isfinezero
	rts			; otherwise, abort silently
isfinezero:			; do not copy further
	dec SourceCount
isfine:
	;; Now open from here the source. We cannot use the source buffer
	;; since it contains the unresolved wildcard.
	ldy #<(DupTargetBuffer+3)
	lda #>(DupTargetBuffer+3)
	jsr SetIOCBAddress
	lda #4
	jsr OpenAux
	jsr PrintSource		; print out what we're doing here

	ldy #DupTargetBuffer-DupBuffer+6
	ldx #DupTargetBuffer-DupBuffer+3 ; now drop the source device
	jsr MoveBuffer
	beq contread		; jumps always
singlefile:
	;; no matter about the result code. If the file name could not be resolved, or the
	;; handler doesn't support it, keep the name as it is

	;; Start now copying the file
	lda #4
	jsr OpenChannel
	jsr PrintSource		; print out what we're doing here
contread:
	;; check now whether we're continuing a copy
	ldx #$10
	lda DupTmp
	lsr a
	bcc noseek		; do not set to the continuation position
	ldy #0			; seek in the source
	jsr Seek
noseek:
	jsr GetMemLo
	jsr SetIOCBAddress	; start reading from here
	;; 
	sec
	lda MemTop
	sbc IOCBAdr,x
	sta IOCBLen,x
	lda MemTop+1
	sbc IOCBAdr+1,x		; up to MemTop
	sta IOCBLen+1,x		; set length of data to be read
	lda #CmdGetBlock
	jsr ExecuteCmd	
	
	lda IOCBLen,x
	sta BlockSize
	lda IOCBLen+1,x
	sta BlockSize+1

	sty DupError		; keep the error byte if there is one
	cpy #EndOfFile		; need a compare here, the above doesn't set flags and the code above changes it
	beq notell		; on EOF, we don't need a seek (and don't want to confuse alian handlers)

	ldy #0
	jsr Tell		; keep the position within the source file
notell:
	jsr CloseChannel

	lda SameDevice
	bne isonother
	jsr PrintInsert
	ldy #<DestinationMsg
	lda #>DestinationMsg
	ldx #DestinationMsgL
	jsr Print		; print that the user has now to insert the target
	jsr PrintReturn
	jsr WaitForKey		; wait that the user hits a key
isonother:

	ldx #$10
	lda #>DupTargetBuffer	; open from here
	ldy #<DupTargetBuffer
	jsr SetIOCBAddress	; input data
	lda DupTmp		; open mode
	sta IOCBAux1,x		; keep it
	lda #CmdOpen
	jsr ExecuteCmd		; open the target now

	lda DupTmp		; are we appending?
	lsr a
	bcc firstgo

	ldy #3
	jsr Seek		; seek to the continuation position where we stopped last time
firstgo:

	jsr GetMemLo
	jsr SetIOCBAddress	; start writing from here, buffer length should be still in the IOCB

	lda BlockSize
	sta IOCBLen,x
	lda BlockSize+1
	sta IOCBLen+1,x

	lda #CmdPutBlock
	jsr ExecuteCmd		; write data out

	ldy DupError		; is this the last go?
	bmi last

	ldy #3
	jsr Tell		; otherwise keep the position where we are
last:	
	jsr CloseChannel

	ldy DupError		; if EOF error, done with it
	bpl next
	lda SourceCount		; if zero, then the source cannot resolve wildcards and we can't go on
	beq exit
	lda HasTarget
	bne exit
	jsr RequestSource	
	jmp copyloop		; continue with the next file
next:	
	lda #13			; use now the "read/write/append" mode
	sta DupTmp
	jsr RequestSource
	jmp nextchunk
exit:
	rts
;;; Print out what we're copying here. This data
;;; is found in the IOCBAdr pointer
PrintSource:
	ldy #<CopyHdr
	lda #>CopyHdr
	ldx #CopyHdrL
	jsr Print
	ldx #$00		; channel
	lda IOCBAdr+1+$10
	ldy IOCBAdr+$10
	jsr SetIOCBAddress
	lda #40
	sta IOCBLen,x		; maximal file length
	lda #CmdPutRecord
	sta IOCBCmd,x
	jmp CIOVector
;;; OpenAux:
;;; Another open variation, this time without setting
;;; the address
;;; A contains Aux1
OpenAux:
	ldx #$10
	sta IOCBAux1,x		; keep it
	lda #$0
	sta IOCBAux2,x
	lda #CmdOpen
	jmp ExecuteCmd		; open the target now
;;; Tell:	Get the current file pointer on the stream and store it
;;; relatively to DupSourcePosition + Y
Tell:
	tya
	pha			; keep index
	lda #CmdNote
	jsr ExecuteCmd		; run it
	pla
	tay			; keep the result
	
	lda IOCBAux3,x
	sta DupSourcePosition,y
	lda IOCBAux4,x
	sta DupSourcePosition+1,y
	lda IOCBAux5,x
	sta DupSourcePosition+2,y
	rts
;;; *** Seek:	Set the current file pointer on the stream from
;;; *** DupSourcePosition + Y
Seek:
	lda DupSourcePosition,y
	sta IOCBAux3,x
	lda DupSourcePosition+1,y
	sta IOCBAux4,x
	lda DupSourcePosition+2,y
	sta IOCBAux5,x
	lda #CmdPoint
	jmp ExecuteCmd
;;; PrintInsert: Print the first part of the message
PrintInsert:
	ldy #<InsertMsg
	lda #>InsertMsg
	ldx #InsertMsgL
	jmp Print
;;; PrintReturn: Print the second part of the message
PrintReturn:
	ldy #<ReturnMsg
	lda #>ReturnMsg
	ldx #ReturnMsgL
	jmp Print
;;; RequestSource
RequestSource:
	lda SameDevice
	bne exitwait		; no need to request the source
	jsr PrintInsert
	ldy #<SourceMsg
	lda #>SourceMsg
	ldx #SourceMsgL
	jsr Print		; print that the user has now to insert the target
	jsr PrintReturn
	;; runs into the following
;;; WaitForKey:	Wait for a keypress
WaitForKey:
	ldx #$ff
	stx BreakFlag
kwlop:
	cpx BreakFlag
	bne brkd
	cpx KeyCodeShadow
	beq kwlop
brkd:
	lda BreakFlag		; hit break?
	stx BreakFlag
	stx KeyCodeShadow
	tay			; hit break?
	beq exitbrk
exitwait:	
	rts
exitbrk:
	txs			; reset the stack
	jmp DupLoop
.endproc	
;;; GetMemLo
;;; Get the smallest address that can be populated for copying data
;;; lo in Y, hi in A
.proc	GetMemLo
	lda AppMemHi		; is this set?
	cmp MemLo
	lda AppMemHi+1
	sbc MemLo+1
	bcs useAppMem		; if so, copy from this point on
	;; not set. Assume that either the application forgot to set it,
	;; or there is none. Erase user memory
	lda FmsBootFlag
	ora #1			; indicate that we're erasing application memory
	sta FmsBootFlag		; Yikes!
	lda MemLo+1
	ldy MemLo
	rts
useAppMem:			; be nice and use Application memory
	lda AppMemHi+1
	ldy AppMemHi
	rts
.endproc
;;; *** InitDriveName
;;; *** Install the device name
.proc	InitDriveName
	ldx #DirInitL-1
ilp:
	lda DirInit,x
	sta DupBuffer,x
	dex
	bpl ilp
	rts
.endproc	
;;; *** PrintLF
;;; *** Print a line feed
LF:		.byte $9b
.proc	PrintLF
	ldy #<LF
	lda #>LF
	ldx #1
	;; runs into the following
.endproc
;;; *** Print
;;; *** Print the text at Lo=Y, Hi=A, length in X thru IOCB #0
.proc	Print
	stx IOCBLen
	ldx #0
	jsr SetIOCBAddress
	lda #CmdPutBlock
	sta IOCBCmd
	jmp CIOVector
.endproc
;;; *** AssignInputBuf
;;; *** Set the IOCB target address to that of the input buffer
.proc	AssignInputBuf
	lda #>(DupBuffer+3)
	ldy #<(DupBuffer+3)
	;; runs into the following
.endproc
;;; *** SetIOCBAddress
;;; *** Set the target address for the IOCB in X with A=hi,Y=lo
.proc	SetIOCBAddress
	sta IOCBAdr+1,x
	tya
	sta IOCBAdr,x
	lda #0
	sta IOCBLen+1,x
	rts
.endproc
;;; *** FindEndOfOption
;;; *** Advance the buffer to the end of the comma-separated option
.proc	FindEndOfOption
	lda #','
	Skip2
	;; runs into the following
.endproc
;;; *** FindEndOfToken
;;; *** Advance the buffer offset to the end of the token and to the start of the next
.proc	FindEndOfToken
	lda #' '
	;; runs into the following
.endproc
;;; *** FindEndOfTokenAt
;;; *** Similar to the above, but the token delimiter is expected in A
;;; *** Returns EQ (Z=1) on EOF, otherwise NE
.proc	FindEndOfTokenAt
	sta FmsPtr+1
	dey
findloop:
	iny
	sty FmsPtr
	lda FmsPtr+1		; get the character
	jsr SkipSeparator	; skip over the current separator
	beq exit		; in case of EOL, leave immediately
	cpy FmsPtr		; still at the same character?
	beq findloop
exit:
	rts
.endproc
;;; *** SkipComma
;;; *** Skip the comma
.proc	SkipComma
	lda #','
	Skip2
	;; runs into/over the following
.endproc
;;; *** SkipBlanks
;;; *** Skip blanks in the input buffer
.proc	SkipBlanks
	lda #' '
	;; runs into the following
.endproc
;;; *** SkipSeparator
;;; *** Skip the separator in A in the command buffer. Y contains the offset into the
;;; *** command buffer. Returns EQ if an EOL has been found
.proc	SkipSeparator
skiploop:
	iny
	cmp DupBuffer-1,y
	beq skiploop
	dey
	;; runs into the following
.endproc
;;; *** CheckForEOL
;;; *** Check whether at the indiciated (Y) position of the input buffer an EOL is present
.proc	CheckForEOL
	lda DupBuffer,y
	cmp #$9b
	rts
.endproc
;;; *** SetDevice
;;; *** Check whether in the buffer at offset Y a device specification is present.
;;; *** If so, copy it to the device name to the buffer at X and advance Y over it.
.proc	SetDevice
	lda #'1'		; the default unit
	pha			; keep it
	lda #':'
	cmp DupBuffer+1,y	; a colon here?
	beq setdeviceone
	cmp DupBuffer+2,y
	bne exit		; not here, then nothing
	pla
	lda DupBuffer+1,y	; is the unit instead
	pha
	lda DupBuffer,y		; get device
	iny			; skip unit
	bne insertme
setdeviceone:
	lda DupBuffer,y		; get device name
insertme:
	sta DupBuffer,x		; set the device name
	pla	
	sta DupBuffer+1,x	; install the unit
	iny			; skip name
	iny			; skip colon
	rts
exit:
	pla
	rts
.endproc
;;; *** MoveBuffer
;;; *** Move the input buffer at offset Y to the input buffer at offset X
;;; *** with X <= Y, up to the EOL
.proc	MoveBuffer
movelp:
	lda DupBuffer,y
	sta DupBuffer,x
	iny
	inx
	cmp #$9b
	bne movelp
	rts
.endproc
	
CopyHdr:	.byte "Copying "
CopyHdrL	=	*-CopyHdr

InsertMsg:	.byte "Insert "
InsertMsgL	= 	*-InsertMsg

ReturnMsg:	.byte " ,press "
		.byte 'R'+$80,'E'+$80,'T'+$80,'U'+$80,'R'+$80,'N'+$80,$9b
ReturnMsgL	=	*-ReturnMsg
	
DestinationMsg:	.byte "Target"
DestinationMsgL	=	*-DestinationMsg

SourceMsg:	.byte "Source"
SourceMsgL	=	*-SourceMsg

ErrorMsg:	.byte "Error - "
ErrorMsgL	= *-ErrorMsg
	
DirInit:	.byte "D1:"	; initializer for the device directory
DirInitL	=	*-DirInit
Title:		.byte $7d
		.byte "Thor Dos 2.++ V 1.8 Enhanced Density",$9b
		.byte "Copyright (c) 1990-2014 by THOR",$9b
TitleL		=	*-Title

	;;; Commands sorted by character position for easy comparison
CmdChar1:	.byte "DDRPUCICRSCLN"
CmdChar2:	.byte "IEERNANLUAOOE"
CmdChar3:	.byte "RLNOPRIRNVPAW"
NumCommands	=	*-CmdChar3
;;; command targets/jump addresses
CmdHi:		.byte >(CmdDir-1),>(CmdDel-1),>(CmdRen-1),>(CmdLoc-1),>(CmdUnl-1),>(CmdCar-1)
		.byte >(CmdFor-1),>(CmdCle-1),>(CmdRun-1),>(CmdSav-1),>(CmdCop-1),>(CmdLoa-1)
		.byte >(CmdNew-1)
CmdLo:		.byte <(CmdDir-1),<(CmdDel-1),<(CmdRen-1),<(CmdLoc-1),<(CmdUnl-1),<(CmdCar-1)
		.byte <(CmdFor-1),<(CmdCle-1),<(CmdRun-1),<(CmdSav-1),<(CmdCop-1),<(CmdLoa-1)
		.byte <(CmdNew-1)
