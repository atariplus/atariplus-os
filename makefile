##################################################################################
## THOR Os									##
## A free operating system for the Atari 8 bit series				##
## (c) 2003 THOR Software, Thomas Richter					##
## $Id: makefile,v 1.53 2013/06/01 17:23:21 thor Exp $				##
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
DISKIOCONFIG1	=	diskio.conf
DISKIOCONFIG2	=	diskio.other.conf
TAPECONFIG1	=	tape.conf
TAPECONFIG2	=	tape.other.conf
MENUCONFIG1	=	menu.conf
MENUCONFIG2	=	menu.other.conf
LOADERCONFIG	=	loader.conf
FMSOVLCONFIG	=	fmsreloc.conf
FUNCCONFIG1	=	menufunction.conf
FUNCCONFIG2	=	menufunction.other.conf
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
FMSOVLSOURCES	=	fmsreloc fmsrelocinit
FMSOVLINC	=	fmsreloc.i
DISKIOSOURCES	=	diskio6init diskio6lo diskio6hi
TAPESOURCES	=	taperesident tapeinit
MENUSOURCES	=	menuloader menuglobals menu
HEADER		=	antic gtia pia pokey errors 850 loader
FUNCSOURCES	=	menudir menuxio menuformat menubinary menuosplus menuduplicat menudupdisk menuutility menuconfig menufms3

OSOBJECTS	=	$(foreach file,$(OSSOURCES),$(file).o)
ROM850OBJECTS	=	$(foreach file,$(ROM850SOURCES),$(file).o)
LOADEROBJECTS	=	$(foreach file,$(LOADERSOURCES),$(file).o)
FMSOVLOBJECTS	=	$(foreach file,$(FMSOVLSOURCES),$(file).o)
DISKIOOBJECTS	=	$(foreach file,$(DISKIOSOURCES),$(file).o)
TAPEOBJECTS	=	$(foreach file,$(TAPESOURCES),$(file).o)
MENUOBJECTS	=	$(foreach file,$(MENUSOURCES),$(file).o)
FUNCOBJECTS	=	$(foreach file,$(FUNCSOURCES),$(file).o)
TAPEOBJECTS	=	$(foreach file,$(TAPESOURCES),$(file).o)
FUNCMENUS	=	$(foreach file,$(FUNCSOURCES),$(file).men)
INCLUDES	=	$(foreach file,$(HEADER),$(file).i) $(foreach file,$(SOURCES),$(file).i)


all	:	osdist rom850 boot850.dump rom850.dump osdist.dump fmsovl.exe diskio.exe menu.exe $(FUNCMENUS) basic.exe help.exe sethdl.exe tape.exe

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

fmsovl.exe	:	fmsovl createbinfile
	@ $(ECHO) "Creating fmsovl.exe"
	@ createbinfile fmsovl fmsovl.exe 0x1f00 0x1f00

#
# Use the following make target for the official os++ distribution
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

prediskio1:	$(DISKIOOBJECTS) $(INCLUDES) $(DISKIOCONFIG1)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(DISKIOCONFIG1) $(DISKIOOBJECTS)

prediskio2:	$(DISKIOOBJECTS) $(INCLUDES) $(DISKIOCONFIG2)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(DISKIOCONFIG2) $(DISKIOOBJECTS)

pretape1:	$(TAPEOBJECTS) $(INCLUDES) $(TAPECONFIG1)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(TAPECONFIG1) $(TAPEOBJECTS)

pretape2:	$(TAPEOBJECTS) $(INCLUDES) $(TAPECONFIG2)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(TAPECONFIG2) $(TAPEOBJECTS)

premenu1:	$(MENUOBJECTS) $(INCLUDES) $(MENUCONFIG1)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(MENUCONFIG1) $(MENUOBJECTS)

premenu2:	$(MENUOBJECTS) $(INCLUDES) $(MENUCONFIG2)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(MENUCONFIG2) $(MENUOBJECTS)

diskio.exe:	prediskio1 prediskio2 createbinfile reloc
	@ reloc diskio6lo.7.bin diskio6lo.8.bin diskio6lo.reloc
	@ reloc diskio6hi.7.bin diskio6hi.8.bin diskio6hi.reloc
	@ cat diskio6lo.7.bin diskio6lo.reloc >diskio6lop.bin
	@ cat diskio6hi.7.bin diskio6hi.reloc >diskio6hip.bin
	@ createbinfile diskio6init.bin diskio6.bin 0x1f00 0x1f00
	@ createbinfile -n diskio6lop.bin diskio6lo.bin 0x2300
	@ createbinfile -n diskio6hip.bin diskio6hi.bin 0x2400
	@ cat diskio6.bin diskio6lo.bin diskio6hi.bin >diskio.exe

tape.exe:	pretape1 pretape2 createbinfile reloc
	@ mv tapeinit.bin tapeinitp.bin
	@ reloc taperesident.7.bin taperesident.8.bin taperesident.reloc
	@ cat taperesident.7.bin taperesident.reloc >taperesidentp.bin
	@ createbinfile tapeinitp.bin tapeinit.bin 0x3100
	@ createbinfile -n -i taperesidentp.bin taperesident.bin 0x3440 0x3101
	@ cat tapeinit.bin taperesident.bin >tape.exe

menu.exe:	premenu1 premenu2 reloc
	@ reloc -o menu.7.bin menu.8.bin menu.reloc
	@ createbinfile -i menuload.bin menu.bin 0x0600 0x0600
	@ cat menu.bin menu.7.bin menu.reloc >menu.exe

loader:	$(LOADEROBJECTS) $(INCLUDES) $(LOADERCONFIG)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(LOADERCONFIG) $(LOADEROBJECTS) -o loader

fmsovl:	$(FMSOVLOBJECTS) $(INCLUDES) $(FMSOVLINC) $(FMSOVLCONFIG)
	@ $(ECHO) "Linking..."
	@ $(LN) $(LNFLAGS) -C $(FMSOVLCONFIG) $(FMSOVLOBJECTS) -o fmsovl

clean	:
	@ $(RM) -f *.o *.dump osrom rom850 boot850 loader os++.tgz *~
	@ $(RM) -f fmsovl *.exe *.bin *.reloc *.men *.pre
	@ $(RM) -f checksum reloc bintohex prerom osdist prehandlerrom1
	@ $(RM) -f createbinfile createmenu

%.o	: 	%.asm %.i
	@ $(ECHO) "Assembling" $*.asm
	@ $(ASM) $(AFLAGS) $*.asm -o $*.o

%.dump	:	% bintohex
	@ $(ECHO) "Converting" $* "to" $*.dump
	@ echo "unsigned char" $*"[] = " >$*.dump
	@ bintohex <$* >>$*.dump

%.pre	:	%.o
	@ $(LN) $(LNFLAGS) -C $(FUNCCONFIG1) -o $*_1.bin $*.o
	@ $(LN) $(LNFLAGS) -C $(FUNCCONFIG2) -o $*_2.bin $*.o
	@ touch $*.pre

menudir.men	:	menudir.pre createmenu
	@ createmenu menudir_1.bin menudir_2.bin menudir.men "Directory" "To Cartridge" "Run DOS"

menuxio.men	:	menuxio.pre createmenu
	@ createmenu menuxio_1.bin menuxio_2.bin menuxio.men "Delete File(s)" "Rename File(s)" "Lock File(s)" "Unlock File(s)"

menuformat.men	:	menuformat.pre createmenu
	@ createmenu menuformat_1.bin menuformat_2.bin menuformat.men "Format Single" "Format Disk" "Clear Disk"

menubinary.men	:	menubinary.pre createmenu
	@ createmenu menubinary_1.bin menubinary_2.bin menubinary.men "Binary Save" "Binary Load" "Run at Address"

menuosplus.men	:	menuosplus.pre createmenu
	@ createmenu menuosplus_1.bin menuosplus_2.bin menuosplus.men "Load OS/A+ File"

menuduplicat.men	:	menuduplicat.pre createmenu
	@ createmenu menuduplicat_1.bin menuduplicat_2.bin menuduplicat.men "Duplicate File(s)"

menudupdisk.men		:	menudupdisk.pre createmenu
	@ createmenu menudupdisk_1.bin menudupdisk_2.bin menudupdisk.men "Duplicate Disk"

menuutility.men		:	menuutility.pre createmenu
	@ createmenu menuutility_1.bin menuutility_2.bin menuutility.men "Check Disk" "Radix Convert" "Drive Diagnostics" "Set Disk Headline" "Check Density" "Set Default Device"

menuconfig.men	:	menuconfig.pre createmenu
	@ createmenu menuconfig_1.bin menuconfig_2.bin menuconfig.men "Configure DOS" "System Information"

menufms3.men	:	menufms3.pre createmenu
	@ createmenu menufms3_1.bin menufms3_2.bin menufms3.men

basic.exe	:	enablebasic.o enablebasic.conf createbinfile
	@ $(LN) $(LNFLAGS) -C enablebasic.conf enablebasic.o -o basic.bin
	@ createbinfile basic.bin basic.exe 0x0500 0x0500

help.exe	:	help.o help.conf createbinfile
	@ $(LN) $(LNFLAGS) -C help.conf help.o -o help.bin
	@ createbinfile -i help.bin help.exe 0x0400 0x0400
	@ tr '\012' '\233' <help.txt >>help.exe

sethdl.exe	:	setheadline.o setheadline.conf createbinfile
	@ $(LN) $(LNFLAGS) -C setheadline.conf setheadline.o -o sethdl.bin
	@ createbinfile sethdl.bin sethdl.exe 0x0400 0x0400

createmenu:	createmenu.c
	@ $(ECHO) "Compiling the createmenu program"
	$(CC) createmenu.c -o createmenu

checksum:	checksum.c
	@ $(ECHO) "Compling the checksum program"
	$(CC) checksum.c -o checksum

reloc:		reloc.c
	@ $(ECHO) "Compling the relocation program"
	$(CC) reloc.c -o reloc

bintohex:	bintohex.c
	@ $(ECHO) "Compling the bintohex program"
	$(CC) bintohex.c -o bintohex

createbinfile:	createbinfile.c
	@ $(ECHO) "Compiling the createbinfile program"
	$(CC) createbinfile.c -o createbinfile

distrib:	os++.tgz

os++.tgz:
	@ make clean
	@ cd .. && tar -czf $(DIR)os++.tgz $(DIR)*.asm $(DIR)*.i $(DIR)makefile $(DIR)README.* $(DIR)*.conf $(DIR)*.c
