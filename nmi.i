;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: nmi.i,v 1.6 2003/04/02 19:37:14 thor Exp $			**
;;; **									**
;;; ** In this module:	 Support for NMI routines of all kinds		**
;;; **********************************************************************


Clock		=	$14		;; VBI driven clock registers
CriticIO	=	$42		;; Ciritical IO flag, shortens VBI
Attract		=	$4d		;; Attract mode timer
DarkMask	=	$4e		;; attract mode darken mask
ColorMask	=	$4f		;; attract mode color mask

VecDLI		=	$200		;; DLI vector
VBITimer0	=	$218		;; first VBI system timer
VecImmediate	=	$222		;; VBI (immediate) vector
VecDeferred	=	$224		;; VBI (deferred) vector
VecVBITimer0	=	$226		;; First timer vector
VecVBITimer1	=	$228		;; Second timer vector
Timer3Flag	=	$22a		;; flag that gets cleared on timer 3 underruns
KeyTimer	=	$22b		;; keyboard countdown timer
Timer4Flag	=	$22c		;; flag that gets cleared on timer 3 underruns
Timer5Flag	=	$22e		;; flag that gets cleared on timer 3 underruns

DMACtrlShadow	=	$22f		;; Antic DMA Control shadow register
DListShadow	=	$230		;; Antic display list shadow register
PenHShadow	=	$234		;; light pen horizontal position shadow register
PenVShadow	=	$235		;; light pen vertical position shadow register

FineScroll	=	$26c		;; Antic fine scrolling
KeyDisable	=	$26d		;; keyboard disable flag
GPriorShadow	=	$26f		;; GTIA Prior shadow register

Paddle0		=	$270		;; Paddle #0 position
Paddle1		=	$271		;; Paddle #1 position
Paddle2		=	$272		;; Paddle #2 position
Paddle3		=	$273		;; Paddle #3 position
Paddle4		=	$274		;; Paddle #4 position
Paddle5		=	$275		;; Paddle #5 position
Paddle6		=	$276		;; Paddle #6 position
Paddle7		=	$277		;; Paddle #7 position

Stick0		=	$278		;; Joystick #0 position
Stick1		=	$279		;; Joystick #1 position
Stick2		=	$27a		;; Joystick #2 position
Stick3		=	$27b		;; Joystick #3 position

PTrig0		=	$27c		;; Paddle trigger #0 button
PTrig1		=	$27d		;; Paddle trigger #1 button
PTrig2		=	$27e		;; Paddle trigger #2 button
PTrig3		=	$27f		;; Paddle trigger #3 button
PTrig4		=	$280		;; Paddle trigger #4 button
PTrig5		=	$281		;; Paddle trigger #5 button
PTrig6		=	$282		;; Paddle trigger #6 button
PTrig7		=	$283		;; Paddle trigger #7 button

Strig0		=	$284		;; Joystick #0 button
Strig1		=	$285		;; Joystick #1 button
Strig2		=	$286		;; Joystick #2 button
Strig3		=	$287		;; Joystick #3 button

PColor0Shadow	=	$2c0		;; Player 0 color shadow
PColor1Shadow	=	$2c1		;; Player 1 color shadow
PColor2Shadow	=	$2c2		;; Player 2 color shadow
PColor3Shadow	=	$2c3		;; Player 3 color shadow
Color0Shadow	=	$2c4		;; Playfield 0 color shadow
Color1Shadow	=	$2c5		;; Playfield 1 color shadow
Color2Shadow	=	$2c6		;; Playfield 2 color shadow
Color3Shadow	=	$2c7		;; Playfield 3 color shadow
ColorBackShadow	=	$2c8		;; Playfield background color shadow

KeyRepeatDelay	=	$2d9		;; initial keyboard repeat value
KeyRepeat	=	$2da		;; Keyboard auto repeat value
KeyDelay	=	$2f1		;; Keyboard repeat timer
ChCtrlShadow	=	$2f3		;; character generator mode
ChBaseShadow	=	$2f4		;; character generator base address
KeyCodeShadow	=	$2fc		;; last keyboard code shadow register

;;; Exported symbols from here
	.global NMIEntry
	.global	ExitVBI
	.global SystemVBI
	.global ExitDLI
