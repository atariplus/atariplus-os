;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: gtia.i,v 1.5 2003/04/13 22:11:56 thor Exp $			**
;;; **									**
;;; ** In this module:	 Equates for the GTIA chip			**
;;; **********************************************************************


GTIABase	=	$d000		;; gtia starts here
Trigger0	=	$d010		;; Joystick #0 trigger
Trigger1	=	$d011		;; Joystick #1 trigger
Trigger2	=	$d012		;; Joystick #2 trigger
Trigger3	=	$d013		;; Joystick #3 trigger
PalNTSC		=	$d014		;; indicates PAL or NTSC color system

PColor0		=	$d012		;; Player 0 color 
PColor1		=	$d013		;; Player 1 color 
PColor2		=	$d014		;; Player 2 color 
PColor3		=	$d015		;; Player 3 color 
Color0		=	$d016		;; Playfield 0 color 
Color1		=	$d017		;; Playfield 1 color 
Color2		=	$d018		;; Playfield 2 color 
Color3		=	$d019		;; Playfield 3 color 
ColorBack	=	$d01a		;; Playfield background color 

GPrior		=	$d01b	;; graphics priority register
Console		=	$d01f	;; console key register


