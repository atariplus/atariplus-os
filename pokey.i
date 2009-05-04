;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: pokey.i,v 1.7 2008-11-24 21:22:55 thor Exp $		**
;;; **									**
;;; ** In this module:	 Pokey registers				**
;;; **********************************************************************

PokeyBase	=	$d200
AudFreq0	=	$d200	;; frequency channel #0
AudCtrl0	=	$d201	;; audio control channel #0
AudFreq1	=	$d202	;; frequency channel #1
AudCtrl1	=	$d203	;; audio control channel #0
AudFreq2	=	$d204	;; frequency channel #2
AudCtrl2	=	$d205	;; audio control channel #0
AudFreq3	=	$d206	;; frequency channel #3
AudCtrl3	=	$d207	;; audio control channel #0
AudioCtrl	=	$d208	;; overall audio output control
Pot0		=	$d200	;; potentiometer #0 input
Pot1		=	$d201	;; potentiometer #0 input
Pot2		=	$d202	;; potentiometer #0 input
Pot3		=	$d203	;; potentiometer #0 input
Pot4		=	$d204	;; potentiometer #0 input
Pot5		=	$d205	;; potentiometer #0 input
Pot6		=	$d206	;; potentiometer #0 input
Pot7		=	$d207	;; potentiometer #0 input
KeyCode		=	$d209	;; keycode of last pressed key
SkReset		=	$d20a	;; serial status reset (write)
Random		=	$d20a	;; random (read)
PotGo		=	$d20b	;; start potentiometer read
SerDat		=	$d20d	;; serial port I/O register
IRQStat		=	$d20e	;; IRQ status register
KeyStat		=	$d20f	;; keyboard status register
SkStat		=	$d20f	;; also the serial status register
