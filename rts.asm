;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: rts.asm,v 1.2 2003/05/17 22:21:16 thor Exp $		**
;;; **									**
;;; ** In this module:	 A single RTS					**
;;; **********************************************************************

	.segment  "RtsVector"

;;; Here (at 0xe4c0) is only a single RTS that is sometimes used
	.global RtsVector
.proc	RtsVector
	rts
.endproc
