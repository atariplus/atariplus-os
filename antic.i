;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: antic.i,v 1.5 2003-04-02 19:37:14 thor Exp $		**
;;; **									**
;;; ** In this module:	 Antic registers				**
;;; **********************************************************************

;; Various ANTIC equates

AnticBase	=	$d400
DMACtrl		=	$d400		;; Antic DMA Control register
ChCtrl		=	$d401		;; character generator mode
DList		=	$d402		;; display list
VScroll		=	$d405		;; vertical scrolling
ChBase		=	$d409		;; character base
WSync		=	$d40a		;; wait for horizontal sync
NMIEnable	=	$d40e		;; NMI enable register
NMIStat		=	$d40f
YPos		=	$d40b		;; vertical beam position
PenH		=	$d40c
PenV		=	$d40d

