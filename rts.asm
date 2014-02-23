;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: rts.asm,v 1.3 2013/06/02 20:41:07 thor Exp $		**
;;; **									**
;;; ** In this module:	 A single RTS					**
;;; **********************************************************************

	.segment  "RtsVector"

;;; Here (at 0xe4c0) is only a single RTS that is sometimes used
	.global RtsVector
.proc	RtsVector
	rts
.endproc
