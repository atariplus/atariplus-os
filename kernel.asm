;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: kernel.asm,v 1.5 2008-09-24 13:59:48 thor Exp $		**
;;; **									**
;;; ** In this module:	 Kernel definitions: The jump vectors of the Os	**
;;; **********************************************************************

	.include "editor.i"
	.include "screen.i"
	.include "keyboard.i"
	.include "printer.i"
	.include "tape.i"
	.include "diskinterf.i"
	.include "sio.i"
	.include "cio.i"
	.include "nmi.i"
	.include "misc.i"
	.include "reset.i"
	.include "dup.i"

	.segment  "Kernel"

	;; init table for the editor
	.word EditorOpen-1
	.word EditorClose-1
	.word EditorGet-1
	.word EditorPut-1
	.word EditorStatus-1
	.word EditorSpecial-1
	jmp EditorInit
	.byte 0
	;; init table for the screen
	.word ScreenOpen-1
	.word ScreenClose-1
	.word ScreenGet-1
	.word ScreenPut-1
	.word ScreenStatus-1
	.word ScreenSpecial-1
	jmp ScreenInit
	.byte 0
	;; init table for the keyboard
	.word KeyboardOpen-1
	.word KeyboardClose-1
	.word KeyboardGet-1
	.word KeyboardPut-1
	.word KeyboardStatus-1
	.word KeyboardSpecial-1
	jmp KeyboardInit
	.byte 0
	;; init table for the printer
	.word PrinterOpen-1
	.word PrinterClose-1
	.word PrinterGet-1
	.word PrinterPut-1
	.word PrinterStatus-1
	.word PrinterSpecial-1
	jmp PrinterInit
	.byte 0
	;; init table for the tape
	.word TapeOpen-1
	.word TapeClose-1
	.word TapeGet-1
	.word TapePut-1
	.word TapeStatus-1
	.word TapeSpecial-1
	jmp TapeInit
	.byte 0
	;;
	;; kernel routine jump table starts here
	;;
	jmp DiskInit
	jmp DiskInterf
	jmp CIO
	jmp SIO
	jmp SetIRQ
	jmp SystemVBI
	jmp ExitVBI
	jmp SIOInit
	jmp InitForSend
	jmp InitNMI
	jmp CIOInit
	jmp Bye
	jmp ResetWarm
	jmp ResetCold
	jmp ReadTapeBlock
	jmp OpenTapeChannel
	jmp PowerupDisplay
	jmp SelfTest
	jmp MountHandler
	jmp UnlinkParHandler
	jmp LinkParHandler
	jmp Boot850
	jmp LaunchDos
	.word DupBuffer		; for OS/A compatibility
	.byte 0
	
