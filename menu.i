;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menu.i,v 1.5 2015/08/15 14:47:43 thor Exp $			**
;;; **									**
;;; ** In this module:	 Menu driven DUP for Dos 2.++			**
;;; ** Derived from version 1.12 (19-Jan-1990)				**
;;; **********************************************************************

MenuAsmAddress		=	$800	; address to be assembled to (ready for relocation)
IOCB			=	$b0	; IOCB to use for the IOCB-dependent commands
ZPtr			=	$b2	; Zero page pointer of the DUP
Z2Ptr			=	$b4	; another pointer
ZTemp			=	$b6	; a temporary or a pointer
FileSpec		=	$b8	; a pointer for a directory file specifiction
FileCounter		=	$ba	; counts entries in the directory
MenuItemCntr		=	$c0	; counts menu items
MenuOrigin		=	$c2	; origin of the menu screen
MenuEntryVectors	=	$0600 	; external menu items find here the entry points to call

	.global	StartOffset
	.global MenuLength

