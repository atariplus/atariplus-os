;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: reset.i,v 1.14 2015/05/09 19:51:28 thor Exp $		**
;;; **									**
;;; ** In this module:	 Startup and Reset handling			**
;;; **********************************************************************

ZeroBase		=	$0	; base address of the reset
CasInit			=	$2	; tape boot init vector
RamTest			=	$4	; used during reset for the long ram test
BootPtr			=	$4	; also used during booting
RamChkPtr		=	$5	; used during reset for the ram test
CartFlag		=	$6	; set if a cart is available
WarmStartFlag		=	$8	; if ff, then this is a warmstart
BootFlag		=	$9	; type of boot that has been loaded
DosVector		=	$a	; starts the dos utility program (if available)
DosInit			=	$c	; initializes the dos
AppMemHi		=	$e	; highest address the application reserved
PalNTSCShadow		=	$62	; sort of shadow register for GTIA PalNTSC
;; the following are (illegally) used to checksum the ROM. No matter,
;; all this gets cleared afterwards
RomSum			=	$04	; checksum so far
RomPtr			=	$0a	; byte that gets checksummed
RomRegionEnd		=	$0c	; end of the region to be checksummed
;;
DiskBootFlag		=	$240	; meaning and value is zero, but must be here
DiskBootSectors		=	$241	; number of sectors to boot
DiskBootAddress		=	$242	; address to boot from
ColdStartFlag		=	$244	; if set, then a warmstart becomes a coldstart
RamSize			=	$2e4	; similar to MemTop, but the Os provided setting
MemTop			=	$2e5	; lowest address used by the Os
MemLo			=	$2e7	; lowest used address
InitMagic0		=	$33d	; must be $5c to accept a reset as warm
InitMagic1		=	$33e	; must be $93
InitMagic2		=	$33f	; must be $25
CartSum			=	$3eb	; cart checksum is here
ScreenFailure		=	$3ec	; set if opening the screen failed
FmsBootFlag		=	$3f5	; now misused for ROM based FMS
				 ; bit 7: if set initialize FMS on reset
				 ; bit 6: if set run DUP after reset
				 ; bit 4: do not allocate memory for buffers (behind os?)
				 ; bit 0: if set reset MemLo, erase program area
DupVector		=	$3f6	; run to launch the dup if FmsBootFlags & 0x40 is set
BasicDisabled		=	$3f8	; if NE, then basic is disabled
Trigger3Shadow		=	$3fa	; GTIA trigger shadow register (cartridge flags)
BootBuffer		=	$400	; boot sector gets loaded to here

CartSumRegion		=	$bf00	; start of the cart checksum region
CartRun			=	$bffa	; the cart run vector
CartTest		=	$bffc	; must be zero for carts
CartType		=	$bffd	; contains the cart flags
CartInit		=	$bffe	; the cart init vector

RomSumLow		=	$c000	; contains checksum of the low ROM area
RomSumHigh		=	$fff8	; contains the checksum of the hi ROM area

CustomRamStart		=	$0700	; custom applications may start here
;;
;; Exported symbols
;;
	.global CPUReset
	.global ResetWarm
	.global ResetCold
	.global	LaunchDos
	.global	InitVectors
	.global DosInitRun
