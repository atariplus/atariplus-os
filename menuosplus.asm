;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menuosplus.asm,v 1.3 2013/06/02 20:41:06 thor Exp $         **
;;; **                                                                  **
;;; ** In this module:  	Os/A+ emulator				**
;;; **********************************************************************

        .include "menujmptab.i"
	.include "kernel.i"
	.include "fms.i"
	.include "dup.i"
	.include "reset.i"
	.include "errors.i"
	.segment "menufunction"

BinStartAddress	=	$f0
BinEndAddress	=	$f2
Flags		=	$f8
	
;;; ***
;;; *** Useful macro definitions
;;; ***
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

;;; *** Init vector
        jmp InitBinary
;;; *** Menu functions
	jmp OsPlusLoad
;;; *** The init vector. Set the RUN vector to something harmless
.proc	InitBinary
	lda #<RTSVector
	sta RunVector
	sta InitVector
	lda #>RTSVector
	sta RunVector+1
	sta InitVector+1
	rts
.endproc
;;; *** OsPlusLoad
;;; *** Load a Dos Os/A+ file and provide the command line
;;; *** arguments and the Os/A+ interface.
OsPlusLoadExit:
	rts
.proc	OsPlusLoad
	ldx #<OsReq
	ldy #>OsReq
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr LoadInputBufferPtr
	jsr AddDevice
	beq OsPlusLoadExit

	ldx #$10
	jsr SetIOCB
	jsr LoadInputBufferPtr
	lda #4
	jsr Open
	jsr CheckIOError
	;; get the start address
	ldx #<2
	ldy #>2
	jsr SetLength
	ldx #<RunAddress
	ldy #>RunAddress
	jsr BGet
	jsr CheckIOError
	lda RunAddress
	and RunAddress+1
	cmp #$ff
	bne badloadfile
	ldx #<RunAddress
	ldy #>RunAddress
	jsr BGet
	jsr CheckIOError
	jsr Close
	;; this should better be above AppMemHi
	lda RunAddress
	cmp AppMemHi
	lda RunAddress+1
	sbc AppMemHi+1
	bcs noproblem

	lda #UseExternalL
	ldx #<UseExternal
	ldy #>UseExternal
	jsr Print
	jmp ParameterError
noproblem:
	jsr RemoveMenu
	ldx #$80		;init, but do not run
	ldy #0
	jsr SetAux
	jsr CreateOsPlusAPI
	jsr LoadInputBufferPtr
	lda #CmdBload
	jsr XIO
	bmi norun
	lda RunVector
	cmp #<RTSVector
	bne haverun
	lda RunVector+1
	cmp #>RTSVector
	bne haverun
	lda RunAddress
	sta RunVector
	lda RunAddress+1
	sta RunVector+1
haverun:
	jsr RunProgram
	ldy #1
norun:
	sty Flags
	jsr DeleteOsPlusAPI
	jsr InstallMenu
	ldy Flags
	Skip2
badloadfile:
	ldy #NoBinaryFile
	jsr CheckIOError
exit:	
	rts
RunProgram:
	jmp (RunVector)
OsReq:		.byte "Load from which OS/A+ file ?",155
UseExternal:	.byte 155
		.byte "Command overwrites MENU code, press ",155
		.byte "/ on keyboard, then enter OSPLUS.MEN",155
		.byte "to run the OS A/+ loader exclusively.",155,155
UseExternalL	=	*-UseExternal
.endproc
;;; CreateOsPlusAPI
;;; Create the Os A/+ binary
.proc	CreateOsPlusAPI
	lda DosVector
	sta DosVectorSave
	lda DosVector+1
	sta DosVectorSave+1
	lda DosInit
	sta DosInitSave
	lda DosInit+1
	sta DosInitSave+1
	lda MemLo
	sta MemLoSave
	lda MemLo+1
	sta MemLoSave+1
	
	lda AppMemHi
	sta MemLo
	lda AppMemHi+1
	sta MemLo+1
	
	lda RunVector
	sta RunAddress
	sta RunLocation
	lda RunVector+1
	sta RunAddress+1
	sta RunLocation+1

	;; prepare the command line
	jsr LoadInputBufferPtr
	jsr SetZPtr
	ldy #0
setcmdline:
	lda (ZPtr),y
	sta LBuf,y
	cmp #$9b
	beq cpdone
	iny
	cpy #64
	bcc setcmdline
cpdone:
	;; copy the device from the command input buffer over
	;; this always starts with a device, but it may be short
	;; by one character
	ldy #0
	lda (ZPtr),y
	sta DefaultDeviceName
	iny
	lda (ZPtr),y
	cmp #':'		;already the end of it?
	bne hasunit
	lda #'1'		;default unit is one
	dey
hasunit:
	iny
	sta DefaultDeviceName+1
	lda #':'
	sta DefaultDeviceName+2
	iny
	sty BufferOffset

	;; Already copy the first argument to ComfNam
	jsr GetArgument
	
	lda #<FakeDOSVector
	sta DosVector
	lda #>FakeDOSVector
	sta DosVector+1
	rts
.endproc
;;; GetArgument
;;; This is actually part of the Os A/+ API and reads the
;;; next argument from the argument buffer
.proc	GetArgument
	ldy BufferOffset
	ldx #0
copyarg:
	lda LBuf,y
	sta ComfNam,x
	cmp #$9b
	beq done
	iny
	cmp #' '
	beq cmdend
	cmp #','
	beq cmdend
	inx
	cpx #24
	bcc copyarg
cmdend:
	lda #$9b		;add a EOL instead of a space or comma
	sta ComfNam,x
	;; skip over blanks, but not over commas
skip:
	lda LBuf,y
	iny
	cmp #' '
	beq skip
	dey
done:
	cpy BufferOffset	;removed anything?
	sty BufferOffset
	php
	;; replace potentially the default device name
	ldx #1
	lda #':'
	cmp ComfNam,x
	beq unitone
	inx
	cmp ComfNam,x		;or with the full unit?
	bne nodevice
	lda ComfNam+1		;get the unit
	Skip2
unitone:
	lda #'1'
	sta DefaultDeviceName+1
	lda ComfNam
	sta DefaultDeviceName
	lda #':'
	sta DefaultDeviceName+2
	txa
	tay
	ldx #0
move:
	iny
	lda ComfNam,y
	sta ComfNam,x
	inx
	cmp #$9b
	bne move
nodevice:
	plp			;restore the Z flag
	rts
.endproc
;;; *** The new Dos-Run vector
.proc	OsASRun
	lda DosInitSave
	sta DosInit
	lda DosInitSave+1
	sta DosInit+1
	lda DosVectorSave
	sta DosVector
	lda DosVectorSave+1
	sta DosVector
	jmp (DosVector)
.endproc
;;; *** DeleteOsPlusAPI
;;; *** remove the Os A/+ API from memory
.proc	DeleteOsPlusAPI
	lda DosInitSave
	sta DosInit
	lda DosInitSave+1
	sta DosInit+1
	lda DosVectorSave
	sta DosVector
	lda DosVectorSave+1
	sta DosVector+1
	lda MemLoSave
	sta MemLo
	lda MemLoSave+1
	sta MemLo+1
	rts
.endproc
;;; *** DOS Vector launch
.proc	NewDosVector
	;; for some reason unclear to me, BASIC304 kills the dos vector.
	;; restore it.
	jsr DeleteOsPlusAPI
	jmp LaunchDosVector
.endproc
;;; *** IO redirection does not work, supply dummies
DivertIO:
ResetIO:
	rts
DosInitSave:		.word 0
DosVectorSave:		.word 0
DupVectorSave:		.word 0
MemLoSave:		.word 0
;;; The "fake API" starts here. None of that is actually in use.
SIOAddress:		.word SIOVector
EchoFlag:		.word $ffff ;no echo. Dos XL has here the "execute command" vector.
BatchFlag:		.word $ffff ;no batch
WriteCmd:		.byte 'P'   ;SIO write command
OsWarmStartFlag:	.byte 0	    ;not after coldstart
FakeDOSVector:
			jmp NewDosVector    ;runs the DOS *here*
			jmp GetArgument	    ;supplies the next argument from the command line
			.word DivertIO	    ;actually, not used.
			.word ResetIO	    ;actually, not used either.
BufferOffset:		.byte 0		    ;offset into the command buffer
ZOrg:			.word $0700	    ;start address of the resident FMS part
CurDate:		.byte 0,0,0	    ;not present
CurTime:		.byte 0,0,0	    ;not present
AltDate:		.byte 0,0,0
AltTime:		.byte 0,0,0
	;; offset = 25 here
UseAltTime:		.byte 0		    ;not used
RunAddress:		.word 0
			.byte 128 	    ;dummy
Density:		.byte 128	    ;all we have...
			.byte 0,0,0	    ;reserved
DefaultDeviceName:	.res 3		    ;placed here
ComfNam:		.res 25		    ;the actual command line buffer, output buffer
	;; offset = 61 here
RunLocation:		.word 0	            ;unclear what the difference is to RunAddress...
LBuf:			.res 64	            ;the input buffer, contains all of the stuff...
