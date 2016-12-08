;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: irq.i,v 1.7 2014/03/10 21:35:39 thor Exp $			**
;;; **									**
;;; ** In this module:	 Support for IRQ routines of all kinds		**
;;; **********************************************************************


;; Definitions for the IRQ support
IRQStatShadow		=	$10	;; shadow register for pokey IRQStat
BreakFlag		=	$11	;; gets cleared if user hits Break
ProceedVec		=	$202	;; interrupt on PIA "Proceed" line (unused)
InterruptVec		=	$204	;; interrupt on PIA "Interrupt" line (unused)
BRKVec			=	$206	;; interrupt for 6502 BRK software interrupt
KeyVec			=	$208	;; keyboard IRQ vector
SerInVec		=	$20a	;; serial input vector
SerOutVec		=	$20c	;; serial output vector
SerXmtVec		=	$20e	;; serial transfer done vector
PokeyTimer1Vec		=	$210	;; timer 1 underrun
PokeyTimer2Vec		=	$212	;; timer 2 underrun
PokeyTimer4Vec		=	$214	;; timer 4 underrun
ImmediateIRQVec		=	$216	;; immediate IRQ vector
BreakVec		=	$236	;; IRQ break vector
HelpFlag		=	$2dc	;; HELP keyboard flag
LastKey			=	$2f2	;; for debouncing: The last pressed keyboard key
StartStopFlag		=	$2ff	;; toggles output on/off

;; Exports for the irq module
	.global IRQEntry
	.global OsIRQEntry
	.global DummyIRQ
	.global KeyIRQ
	.global BreakIRQ
