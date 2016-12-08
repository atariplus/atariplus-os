;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menujmptab.i,v 1.11 2015/08/15 14:47:43 thor Exp $		**
;;; **									**
;;; ** In this module:	 Menu driven DUP for Dos 2.++			**
;;; ** Derived from version 1.12 (19-Jan-1990)				**
;;; **********************************************************************

;;; This file defines the external references available for menu functions
;;; They are all located in page 6.


IOCB			=	$b0     ; currently active IOCB channel
ZPtr			=	$b2	; Zero page pointer of the DUP
Z2Ptr			=	$b4	; another pointer
Z3Ptr			=	$b6	; another temporary
Z4Ptr			=	$b8
ZFlag			=	$c0

	.segment "jumptab"
JUMPTAB:			 	;entry points into the relocatable part of DUP
DupEnd:			.res	2	;Last memory address used by Dup, aka MemLo for DUP
STAX:			.res	1	;stack pointer storage
InExtended:		.res	1	;if negative, global menu is loaded (not in external)
ResetFlags:		.res	1	;reserved, no longer used
AllowSwitch:		.res	1	;read-only: menu switching disabled or enabled?
DisplaySwitch:		.res	1	;DUP menu visible?
LastMenuPoint:		.res	1	;number of menu items visible.
DefaultDeviceLen:	.res 	1	;number of characters in the default device (2 or 3)
DefaultDevice:		.res	3	;the default device.
	;; DUP entry points start here
Start:			.res	3       ;system use only, run DUP (START)
Init:			.res	3	;system use only, initialize DUP (INIT)
Print:			.res	3	;print the string at x,y length a (PRINT)
SetIOCB:		.res	3	;set the IOCB to use for the next commands, X=# (SETIOCB)
LoadInputBufferPtr:	.res	3	;load the input buffer and its size to ->X,Y and A(SETINPBUF)
LoadBuf1Ptr:		.res	3	;load the buffer 1 pointer and its size to X,Y and A (SETBUF1)
LoadBuf2Ptr:		.res	3	;load the buffer 2 pointer and its size to X,Y and A (SETBUF2)
LoadBuf3Ptr:		.res	3	;load the buffer 3 pointer and its size to X,Y and A (SETBUF3)
Open:			.res	3	;open the current IOCB for the filespec (X,Y) mode A (OPEN)
Close:			.res	3	;close the current IOCB (CLOSE)
XIO:			.res	3	;detach the XIO cmd A on the filespec (X,Y) (XIO)
ClearAux:		.res	3	;reset the Aux values for the current IOCB (CLRAUX)
GetLine:		.res	3	;read a line from the editor to X,Y, length A (INPUT)
Error:			.res	3	;display the error in Y (ERROR)
SetHeadLine:		.res	3	;set the headline with the text at X,Y (TOHDL)
ClearHeadLine:		.res	3	;clear the headline (INHDL)
Reserve:		.res	3	;reserve (X,Y) bytes of memory, return the memory pointer in X,Y (RESERVE)
Dispose:		.res	3	;return the memory block (X,Y) to the system. Must be called in inverse order of RESERVE (DISPOSE)
ReleaseAllMemory:	.res	3	;release all allocated memory at once (SETOLD)
CheckIOError:		.res	3	;test whether Y is a relevant IO error. C=0: none, C=1: EOF, other error generated (SUBERROR)
AbortOnBreak:		.res	3	;test whether Y is an error. C=0: none. On BREAK, return to main. Otherwise, set C and N (SUBERROR2)
BlockDisplay:		.res	3	;disable the display switching capability (BLOCK)
ReadInputParameter:	.res	3	;read parameter from input buffer to ->(X,Y), Z=1 if empty,C=1 if EOL(GETNEXT)
Copy:			.res	3	;copy A bytes from (X,Y) to (fr0,1) (COPY)
ParameterError:		.res	3	;signal a parameter error (PARAM)
SetRegularColor:	.res	3	;install the regular background color (NORM)
SetRedColor:		.res	3	;set the background to red (RED)
SetGreenColor:		.res	3	;set the background to green (GREEN)
SetBlueColor:		.res	3	;set the background to blue (BLUE)
SetYellowColor:		.res	3	;set the background to yellow (YELLOW)
UnblockDisplay:		.res	3	;re-allow switching the display (EXBLOCK)
DisableBREAK:		.res	3	;disable the BREAK key (BBREAK)
EnableBREAK:		.res	3	;re-enable the BREAK key (EBREAK)
ToDecimal:		.res	3	;convert binary X,Y->Buf3 decimal (TODEC)
ToHex:			.res	3	;convert binary X,Y->Buf3 hex (TOHEX)
FromDecimal:		.res	3	;convert decimal (X,Y)->X,Y binary (FRDEC)
FromHex:		.res	3	;convert hex (X,Y)->X,Y binary (FRHEX)
SetAux:			.res	3	;X->AUX1,Y->AUX2 (SETAUX)
SetIOCBAdr:		.res	3	;install the address X,Y into the current IOCB (SETBUF)
SetLength:		.res	3	;install the length X,Y into the current IOCB (SETLEN)
SetCommand:		.res	3	;install A as next CIO command (SETCOM)
RunCIOCmd:		.res	3	;run the CIO Command in A with all the other settings already in place (TOCIO)
GetKey:			.res	3	;read a character from the keyboard, return in A (GKEY)
BGet:			.res	3	;read the block to (x,y) from the current IOCB (BGET)
BPut:			.res	3	;write the block at (x,y) to the current IOCB (BPUT)
PrintRecord:		.res	3	;Print the EOL terminated record at (X,Y) len A (PRIND)
AddDevice:		.res	3	;add the default device to the buffer (x,y) if not present (ADDD)
ConvertDirectoryEntry:	.res	3	;convert the directory entry in (x,y) to a filespec in buf3. (CONFILE)
GetArgumentExtension:	.res	3	;check whether (x,y) contains extension /A. If so, remove and return C=1 (GETEXT)
CopyBuffer2Buffer:	.res	3	;copy buffer X-> buffer Y (MOVE)
CursorOff:		.res	3	;disable the cursor (CURSOFF)
CursorOn:		.res	3	;enable the cursor (CURSON)
PrintEOL:		.res	3	;print a line feed (PRINTEOL)
SetZPtr:		.res	3	;set the DUP ZPtr from X,Y (TOZERO)
RunCartridge:		.res	3	;run the cartridge (TOCAR)
RunCommandLine:		.res	3	;run the standard DUP command line (TODOS)
YesNo:			.res	3	;ask for a single keypress, return C=1 if yes, else C=0 (YESNO)
CloseAll:		.res	3	;close all channels (CLOSEALL)
GetDirectoryList:	.res	3	;read the directory contents as a list, filespec in (x,y), use #1. Return on heap in (x,y) (GETLIST)
GetDirectoryEntry:	.res	3	; A=unit#,(X,Y)->InputBuffer, C=1 end or (X,Y) pointer to filespec. (GETENTRY)
GetUnitNumber:		.res	3	; (X,Y)->A = DEVICE # (GETDEVNO)
SetDefaultUnit:		.res	3	; set the default unit in A (SETDEVNO)
GetLength:		.res	3	; return the length of the last IOCB command to X,Y (GETLEN)
RestoreOsDisplay:	.res	3	; restore the Os display
InstallDisplay:		.res	3	; install the menu display if necessary
InstallDestructor:	.res	3	; install a call-back if a menu function is unloaded
RemoveMenu:		.res	3	; remove the menu display and menu specific vectors
InstallMenu:		.res	3	; re-install the menu vectors.
GetDefaultDevice:	.res	3	; copy the default device to the buffer X,Y
SetDefaultDevice:	.res	3	; set the default device from the buffer X,Y
NextDirectoryEntry:	.res	3	; advance the location X at ZPage to the next directory
GetBufferedLength:	.res	3	; store data X,Y into the directory entry at fr0+2,fr0+3
SetBufferedLength:	.res	3	; store data X,Y into the directory entry at fr0+2,fr0+3


