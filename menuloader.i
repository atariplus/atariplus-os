;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menuloader.i,v 1.2 2013-04-07 17:18:23 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DUP Menu - resident loader			**
;;; **********************************************************************

LoaderStart		=	$0600

;;;
;;; Base address of the menu
TargetPtr		=	$f0
RunAddress		=	$f2
RelocOffset		=	$f4
	
