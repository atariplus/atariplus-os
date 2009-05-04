##################################################################################
## THOR Os									##
## A free operating system for the Atari 8 bit series				##
## (c) 2003 THOR Software, Thomas Richter					##
## $Id: makefile,v 1.28 2008-12-29 23:37:23 thor Exp $				##
##										##
## Makefile for this project							##
##################################################################################

.PHONY:		all clean

MAKEILE		=	makefile
ASM		=	ca65
AFLAGS		=	-W1 -g --cpu 6502 -t atari
RM		=	rm
LN		=	ld65
OSCONFIG	=	osrom.conf
ROM850CONFIG1	=	850.conf
ROM850CONFIG2	=	850.other.conf
LOADERCONFIG	=	loader.conf
LNFLAGS		=	
ECHO		=	echo
CC		=	cc
DIR		=	osrom/


OSSOURCES	=	charmap intlmap \
			kernel reset nmi irq diskinterf sio cio misc \
			editor screen keyboard printer tape vectors fms rts \
			dup romtest mathpack
ROM850SOURCES	=	850
LOADERSOURCES	=	loader
HEADER		=	antic gtia pia pokey errors 850 loader

OSOBJECTS	=	$(foreach file,$(OSSOURCES),$(file).o)
ROM850OBJECTS	=	$(foreach file,$(ROM850SOURCES),$(file).o)
LOADEROBJECTS	=	$(foreach file,$(LOADERSOURCES),$(file).o)
INCLUDES	=	$(foreach file,$(HEADER),$(file).i) $(foreach file,$(SOURCES),$(file).i)

all	:	osdist rom850 boot850.dump rom850.dump osdist.dump

boot850	:	rom850

rom850	:	prehandlerrom1 prehandlerrom2 reloc loader
	@ $(ECHO) "Preparing the loader relocatable"
	@ reloc prehandlerrom1 prehandlerrom2 relocation
	@ dd if=loader count=1 bs=256 conv=sync | cat - relocation > boot850
	@ rm loader relocation prehandlerrom2
	@ cp prehandlerrom1 rom850

osrom	:	prerom checksum
	@ $(ECHO) "Combining ROM sources..."
	@ cp prerom osrom
	@ checksum osrom

#
# Use the following make target for the official os++ distribution
# without the math pack which is copyrighted.
osdist	:	prerom checksum
	@ $(ECHO) "Combining ROM sources..."
	@ cp prerom osdist
	@ checksum osdist

prerom	:	$(OSOBJECTS) $(INCLUDES) $(OSCONFIG)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(OSCONFIG) $(OSOBJECTS) -o prerom

prehandlerrom1:	$(ROM850OBJECTS) $(INCLUDES) $(ROM850CONFIG1)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(ROM850CONFIG1) $(ROM850OBJECTS) -o prehandlerrom1

prehandlerrom2:	$(ROM850OBJECTS) $(INCLUDES) $(ROM850CONFIG2)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(ROM850CONFIG2) $(ROM850OBJECTS) -o prehandlerrom2

loader:	$(LOADEROBJECTS) $(INCLUDES) $(LOADERCONFIG)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(LOADERCONFIG) $(LOADEROBJECTS) -o loader

clean	:
	@ $(RM) -f *.o *.dump osrom rom850 boot850 loader os++.tgz
	@ $(RM) -f checksum reloc bintohex prerom osdist prehandlerrom1

%.o	: 	%.asm %.i
	@ $(ECHO) "Assembling" $*.asm
	@ $(ASM) $(AFLAGS) $*.asm -o $*.o

%.dump	:	% bintohex
	@ $(ECHO) "Converting" $* "to" $*.dump
	@ echo "unsigned char" $*"[] = " >$*.dump
	@ bintohex <$* >>$*.dump

checksum:	checksum.c
	@ $(ECHO) "Compling the checksum program"
	$(CC) checksum.c -o checksum

reloc:		reloc.c
	@ $(ECHO) "Compling the relocation program"
	$(CC) reloc.c -o reloc

bintohex:	bintohex.c
	@ $(ECHO) "Compling the bintohex program"
	$(CC) bintohex.c -o bintohex

distrib:	os++.tgz

os++.tgz:
	@ make clean
	@ cd .. && tar -czf $(DIR)os++.tgz $(DIR)*.asm $(DIR)*.i $(DIR)makefile $(DIR)README.* $(DIR)*.conf $(DIR)*.c
