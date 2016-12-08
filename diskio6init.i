;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: diskio6init.i,v 1.2 2015/08/15 14:47:42 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DiskIO helper 					**
;;; **********************************************************************

;;; Where the resident part of diskIO is placed in high-memory, if present
DiskIOCartAddress	=	$ac00

;; where the resident lo-part of Diskio loads from
ResidentLoLoad		=	$2300
;; where the resident hi-part of Diskio loads from
ResidentHiLoad		=	$2400
	
;; where DiskIO goes in case the cart can be disabled
ResidentHiTarget	=	$ac00
	
