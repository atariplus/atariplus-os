;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menuxio.asm,v 1.3 2013/06/02 20:41:06 thor Exp $		**
;;; **									**
;;; ** In this module:	 Delete,Rename,lock,unlock menu for DUP 2.++	**
;;; **********************************************************************

	.include "menujmptab.i"
	.include "fms.i"
	
	.segment "menufunction"

CheckFlag	=	$f0	;the default whether the user should be asked or not
XioCallBack	=	$f1	;call back for the implementation
XioProceed	=	$f3	;position and size of the message to confirm action
XioCmd		=	$f6	;XIO command to execute
XioConfirm	=	$f7	;position and size of the confirmation message
PatternList	=	$fa	;address of the directory pattern list
	
;;; Init-vector
	rts			;not required
;;; Entry points
	jmp Delete
	jmp Rename
	jmp Lock
	jmp Unlock

;;; *** Delete multiple files
;;; *** Delete
.proc	Delete
	lda #'Y'
	sta CheckFlag		;ask by default
	lda #<DeleteCallback
	sta XioCallBack
	lda #>DeleteCallback
	sta XioCallBack+1

	lda #DeleteProceedL
	ldx #<DeleteProceed
	ldy #>DeleteProceed
	stx XioProceed
	sty XioProceed+1
	sta XioProceed+2

	lda #CmdDelete
	sta XioCmd

	lda #DeleteConfirmL
	ldx #<DeleteConfirm
	ldy #>DeleteConfirm
	stx XioConfirm
	sty XioConfirm+1
	sta XioConfirm+2
	
	ldx #<DeleteQuestion
	ldy #>DeleteQuestion
	jsr GenericXIO
	rts
DeleteCallback:
	jsr SetRedColor
	jsr GoXio
	rts
DeleteQuestion:	.byte "Which file to delete ?",155
DeleteConfirm:	.byte 155,"Type ",34,"Y",34," to delete "
DeleteConfirmL	=	*-DeleteConfirm
DeleteProceed:	.byte "Deleting "
DeleteProceedL	=	*-DeleteProceed
.endproc
;;; *** Rename one or multiple files
;;; *** Rename
.proc	Rename
	lda #'Y'
	sta CheckFlag		;ask by default
	lda #<RenameCallback
	sta XioCallBack
	lda #>RenameCallback
	sta XioCallBack+1

	ldx #<RenameQuestion
	ldy #>RenameQuestion
	jsr GenericXIO
	rts
RenameCallback:
	lda CheckFlag
	cmp #'N'
	beq dontask
	lda #RenameConfirmL
	ldx #<RenameConfirm
	ldy #>RenameConfirm
	jsr Print
	jsr LoadInputBufferPtr
	jsr PrintRecord
	lda #RenameConfirm2L
	ldx #<RenameConfirm2
	ldy #>RenameConfirm2
	jsr Print
	jsr LoadBuf2Ptr
	jsr PrintRecord
	jsr YesNo
	bcc skip
dontask:
	lda #RenameProceedL
	ldx #<RenameProceed
	ldy #>RenameProceed
	jsr Print
	jsr LoadInputBufferPtr
	jsr PrintRecord
	jsr LoadInputBufferPtr
	jsr SetZPtr
	jsr LoadBuf2Ptr
	stx Z2Ptr
	sty Z2Ptr+1
	ldy #0
fndend:	lda (ZPtr),y
	cmp #$9b
	beq found
	iny
	bne fndend
found:
	lda #','
	sta (ZPtr),y
	iny
	sty Z3Ptr
	ldy #0
	sty Z3Ptr+1
append:
	ldy Z3Ptr+1
	inc Z3Ptr+1
	lda (Z2Ptr),y
	ldy Z3Ptr
	inc Z3Ptr
	sta (ZPtr),y
	cmp #$9b
	bne append
	jsr DisableBREAK
	jsr LoadInputBufferPtr
	lda #CmdRename
	jsr XIO
	jsr CheckIOError
	jsr EnableBREAK
skip:
	rts
RenameQuestion:	.byte "Rename - give old name,new",155
RenameConfirm:	.byte 155,"Type ",34,"Y",34," to rename "
RenameConfirmL	=	*-RenameConfirm
RenameConfirm2:	.byte " to "
RenameConfirm2L	=	*-RenameConfirm2
RenameProceed:	.byte "Renaming "
RenameProceedL	=	*-RenameProceed
.endproc
;;; *** Lock (protect) files.
;;; *** Lock
.proc	Lock
	lda #'N'
	sta CheckFlag		;do not ask by default
	lda #<GoXio
	sta XioCallBack
	lda #>GoXio
	sta XioCallBack+1

	lda #LockProceedL
	ldx #<LockProceed
	ldy #>LockProceed
	stx XioProceed
	sty XioProceed+1
	sta XioProceed+2

	lda #CmdLock
	sta XioCmd

	lda #LockConfirmL
	ldx #<LockConfirm
	ldy #>LockConfirm
	stx XioConfirm
	sty XioConfirm+1
	sta XioConfirm+2
	
	ldx #<LockQuestion
	ldy #>LockQuestion
	jsr GenericXIO
	rts
LockQuestion:	.byte "What file to lock ?",155
LockConfirm:	.byte 155,"Type ",34,"Y",34," to lock "
LockConfirmL	=	*-LockConfirm
LockProceed:	.byte "Locking "
LockProceedL	=	*-LockProceed
.endproc
;;; *** Unlock (unprotect) files.
;;; *** Unlock
.proc	Unlock
	lda #'N'
	sta CheckFlag		;do not ask by default
	lda #<GoXio
	sta XioCallBack
	lda #>GoXio
	sta XioCallBack+1

	lda #UnlockProceedL
	ldx #<UnlockProceed
	ldy #>UnlockProceed
	stx XioProceed
	sty XioProceed+1
	sta XioProceed+2

	lda #CmdUnlock
	sta XioCmd

	lda #UnlockConfirmL
	ldx #<UnlockConfirm
	ldy #>UnlockConfirm
	stx XioConfirm
	sty XioConfirm+1
	sta XioConfirm+2
	
	ldx #<UnlockQuestion
	ldy #>UnlockQuestion
	jsr GenericXIO
	rts
UnlockQuestion:	.byte "What file to unlock ?",155
UnlockConfirm:	.byte 155,"Type ",34,"Y",34," to unlock "
UnlockConfirmL	=	*-UnlockConfirm
UnlockProceed:	.byte "Unlocking "
UnlockProceedL	=	*-UnlockProceed
.endproc
;;; *** Run a disk command that is XIO based
;;; *** Print the question in X,Y, run the callback
;;; *** for every match
;;; *** GenericXIO
.proc	GenericXIO
	lda #40
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr PrintEOL
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq nofile
	jsr LoadBuf2Ptr
	jsr ReadInputParameter
	
	jsr LoadBuf1Ptr
	lda #'Y'
	jsr GetArgumentExtension
	bcc not1
	sta CheckFlag
not1:
	jsr LoadBuf1Ptr
	lda #'N'
	jsr GetArgumentExtension
	bcc not2
	sta CheckFlag
not2:

	jsr LoadBuf2Ptr
	lda #'Y'
	jsr GetArgumentExtension
	bcc not3
	sta CheckFlag
not3:
	jsr LoadBuf2Ptr
	lda #'N'
	jsr GetArgumentExtension
	bcc not4
	sta CheckFlag
not4:
	jsr LoadBuf1Ptr
	jsr GetDirectoryList
	stx PatternList
	sty PatternList+1
xioloop:
	ldx PatternList
	ldy PatternList+1
	lda DefaultDevice+1
	jsr GetDirectoryEntry
	bcs empty
	jsr SetZPtr
	ldy #0
	lda #$ff
	sta (ZPtr),y		;mark entry as handled

	ldx #$10
	jsr SetIOCB
	jsr ClearAux
	jsr CallCallBack
	jmp xioloop
empty:
nofile:	
	rts
CallCallBack:
	jmp (XioCallBack)
.endproc
;;; *** Perform the actual action by asking the user
;;; *** printing messages and finally run the command
;;; *** GoXio
.proc	GoXio
	lda CheckFlag
	cmp #'N'
	beq dontask
	ldx XioConfirm
	ldy XioConfirm+1
	lda XioConfirm+2
	jsr Print
	jsr LoadInputBufferPtr
	jsr PrintRecord
	jsr YesNo
	bcc exit
dontask:
	jsr DisableBREAK
	
	ldx XioProceed
	ldy XioProceed+1
	lda XioProceed+2
	jsr Print
	jsr LoadInputBufferPtr
	jsr PrintRecord
	jsr LoadInputBufferPtr
	lda XioCmd
	jsr XIO
	jsr CheckIOError
	jsr EnableBREAK
exit:
	rts
.endproc

