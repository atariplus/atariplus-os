;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: mathpack.asm,v 1.22 2008-11-24 21:22:55 thor Exp $		**
;;; **									**
;;; ** In this module:	 math pack functions				**
;;; **********************************************************************

	.include "mathpack.i"
	.segment "MathPack"

;;; *** Skip two bytes (by a dummy BIT)
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro
.macro	PlaceAt dest
	.if * > dest
	.error "Previous method is too long, cannot place at desired location"
	.endif
	.if dest > *
	.res dest-*
	.endif
.endmacro

;;; *** AsciiToBCD (also known as AFP)	must go to d800
;;; *** Convert an ATASCII string pointed to by CIX+(INBUFF)
;;; *** into a BCD number in FR0
	.org $d800
	.global AsciiToBCD
.proc	AsciiToBCD
	jsr SkipBlanks		; skip leading blanks
	ldx #frx
	ldy #6
	jsr ZeroRgs		; erase the parsing flags
	dec digrt		; no decimal point yet
	ldx #fr0
	ldy #12			; complete fr0,fr0ext part
	jsr ZeroRgs
	ldy cix
	Skip2
gotdigit:
	dec fchflg		; accumulated numbers
	jsr FetchDigitY		; get the next digit
	bcs dotsign
	ldx fr0+1		; already up to the one-digit?
	cpx #$10
	bcs skip
	pha
	tya
	pha
	jsr NextFr0Digit	; make room for the next digit
	pla
	tay
	pla
	ora fr0+5		; place in bottommost digit
	sta fr0+5
	lda digrt		; already collected the decimal fraction?
	bmi gotdigit		; if not, continue
	dec eexp		; adjust decimal fraction by one position
	bne gotdigit
skip:				; ignore additional digits
	;; this could potentially be done in a better way by rounding if necessary.
	ldx frx			; digits beyond precision go to fr0ext
	bne full
	asl a
	asl a
	asl a
	asl a
	sta fr0ext		; keep as additional digit
	inc frx
full:	lda digrt		; dot accumulated?
	bpl gotdigit		; if so, ignore additional places
	inc eexp		; otherwise, fixup the exponent value
	bne gotdigit
error:
	dey			; back to the invalid character
	sty cix			; keep cix updated
	sec
	rts
dotsign:
	cmp #'.'		; decimal dot?
	beq dot
	cmp #'E'		; start of exponent value
	beq exponent
	ldx fchflg		; start of number found already?
	bne noexp		; if so, this ends it
	cmp #'+'		; positive sign?
	beq gotdigit		; continue right away
	cmp #'-'		; or negative?
	bne error		; if none, then this is not a valid number at all
	lda #$80		; sign for negative
	sta nsign		; got the sign of the number now
	bmi gotdigit
dot:				; got a decimal dot, set flag
	lda digrt		; dot already parsed?
	bpl noexp		; if so, end of number
	inc digrt
	beq gotdigit		; will become zero, thus jumps always
exponent:
	lda fchflg		; if the number starts with E, make this invalid
	beq error
	sty frx			; keep position behind E, is invalid position if exp is invalid
	jsr FetchDigitY
	bcc number
	cmp #'+'		; positive exponent sign
	beq pexp
	cmp #'-'		; negative exponent sign
	bne expabort		; the E and the - sign are not part of the number
	sta esign		; sign of exponent now negative
pexp:
	jsr FetchDigitY		; and instead the next number, now hopefully valid
	bcs expabort		; not really an exponent
number:
	sta ztemp1		; keep low
	jsr FetchDigitY		; and the next digit
	bcs noexpdigit
	sta ztemp1+1		; keep
	lda ztemp1		; get tenth-digit
	asl a			; times two
	asl a			; times four (C = 0 for sure)
	adc ztemp1		; times five
	asl a			; times ten
	adc ztemp1+1		; plus one's digit
	iny			; to be consistent:	move cix behind the first invalid digit
	Skip2
	;; Handle cases where more digits follow, and error then
	;; Doesn't make any difference....
noexpdigit:
	lda ztemp1		; restore the exponent (if required)
	;; now negate the exponent, if required
	ldx esign
	beq exppos
	eor #$ff		; two's complement
	inc eexp		; one's component (gets added to it anyhow)
exppos:
	clc
	adc eexp		; and add to the already existing exponent
	sta eexp		; done
	Skip2
expabort:
	ldy frx			; restore position in front of exponent
noexp:	
	dey			; move cix again back to the first invalid character
	sty cix			; keep now
	;; decimal dot is already included in eexp,
	;; now make the exponent the exponent to base 100, not base 10
	ldx eexp
	txa
	asl a
	txa
	ror a
	clc
	adc #$44		; bias
	ora nsign		; sign
	sta fr0
	txa
	lsr a
	bcc even
	jsr TimesTen		; was one too small
even:
	jmp RoundExt		; Normalize the number
.endproc
;;; *** BCDToAscii		must go to d8e6
;;; *** This routine is the reverse of the above,
;;; *** it converts a floating point value in fr0
;;; *** into an ASCII representation at outbuf = $580
	PlaceAt $d8e6
	.global BCDToAscii
.proc	BCDToAscii
	jsr LoadOutbuff
	ldy #0			; cix
	ldx fr0			; exponent = 0 (=> number = zero?)
	bne nonzero
	tya			; generate a single zero digit
	beq markend
nonzero:
	bpl positive
	lda #'-'
	sta (inbuff),y		; is negative, indicate as such
	iny
positive:
	txa
	and #$7f
	cmp #$3f		; decimal dot required?
	bcc exponential
	cmp #$45		; or recommended?
	bcs exponential
	sbc #$3f-1		; carry was clear
	jsr MantissaToAscii	; only the mantissa
	jsr StripTrailingZeros
	bne markend
exponential:			; use half-logarithmic conversion
	asl a
	eor #$80		; keep exponent (=-128)
	sta eexp		; after converting back to the base of ten
	lda #1			; ensure that we use the dot after the first digit
	jsr MantissaToAscii
	tya
	pha			; keep last position
	ldy #1			; if the decimal dot is at position 1
	lda fr0
	bpl pos
	iny			; must test the next digit - a minus got inserted
pos:	lda (inbuff),y
	cmp #'.'		; then the number is formatted correctly: Only one digit 
	beq fmtfine		; in front of the dot.
	iny			; otherwise, reformat
	sta (inbuff),y
	lda #'.'		; and move the dot one forward
	dey
	sta (inbuff),y
	inc eexp		; at the price of enlarging the exponent
fmtfine:
	pla
	tay			; restore value again
	jsr StripTrailingZeros	; again, remove zeros at the end
	iny
	lda #'E'		; insert the exponent indicator
	sta (inbuff),y
	iny
	lda eexp		; positive or negative?
	bpl posexp
	eor #$ff		; one's component
	sta eexp
	inc eexp		; two's component
	lda #'-'		; negative exponent sign
	Skip2
posexp:
	lda #'+'		; positive exponent sign
	sta (inbuff),y
	iny
	;; now convert the binary exponent to ASCII
	ldx #0
	lda eexp
subloop:
	cmp #10
	bcc done
	sbc #10			; carry is set
	inx			; one more
	bne subloop
done:
	pha			; keep one's digit
	txa			; get tenth digit
	jsr InsertDigit
	pla			; one's digit
markend:
	ora #$80		; mark end of the line
	jsr InsertDigit
	clc
	rts
.endproc
;;; *** Utility functions for multiplication
;;; *** AddTimesFr1:	compute fr0+y*fr1->fr0
.proc	AddTimesFr1	
	tay
	beq done
add:	ldx #5
	clc
loop:	lda fr0,x
	adc fr1,x
	sta fr0,x
	dex
	bpl loop
	dey
	bne add
done:	
	rts
.endproc
;;; *** AddTimesFr2:	compute fr0+y*fr2->fr0
.proc	AddTimesFr2
	tay
	beq done
add:	ldx #5
	clc
loop:	lda fr0,x
	adc fr2,x
	sta fr0,x
	dex
	bpl loop
	dey
	bne add
done:	
	rts
.endproc
;;; *** IntToBCD (also known as IFP)	must go to d9aa
;;; *** Convert the integer in fr0 into BCD
	PlaceAt $d9aa
	.global IntToBCD
.proc	IntToBCD
	ldx fr0
	ldy fr0+1		; hi
.endproc
;;; *** IntXYToBCD
;;; *** Convert the register X,Y = (lo,hi) from integer to BCD
.proc	IntXYToBCD
	stx ztemp1		; lo
	sty ztemp1+1
	jsr ZeroFr0		; clear me
	lda #$42		; exponent as current given, but unnormalized
	sta fr0
	sed
	ldy #16			; number of bits to shift
loop:	
	asl ztemp1		; bring the topmost bit into position
	rol ztemp1+1		; topmost bit into the carry
	ldx #2			; #of registers to add:	Maximal two of them
add:	lda fr0+1,x
	adc fr0+1,x
	sta fr0+1,x
	dex
	bpl add
	dey
	bne loop
	jmp Normalize
.endproc
;;; *** BCDToInt (also known as FPI)	must go to d9d2
;;; *** Convert the floating point register in fr0 to
;;; *** integer in fr0,fr0+1 if possible. Return with C=1
;;; *** if not.
	PlaceAt $d9d2
	.global BCDToInt
.proc	BCDToInt
	jsr BCDToIntDown	; round down the result
	bcs error
	stx ztemp1
	sty ztemp1+1		; store result
	jsr NextFr0Digit	; next digit
	ldx ztemp1		; get lo
	ldy ztemp1+1		; get hi again
	cmp #5			; five or above?
	bcc nocarry		; if not, do not perform rounding
	inx
	bne nocarry
	iny			; carry over into high
	beq error		; overflow (C will be set here anyhow)
nocarry:			; done here, copy the result into fr0
	stx fr0
	sty fr0+1
	clc			
error:	
	rts
.endproc
;;; ***BCDToIntDown
;;; *** Round the BCD value in fr0 *down* to the next integer,
;;; *** deliver the result in X,Y
.proc	BCDToIntDown
	ldy #0			; zero hi
	ldx #0			; zero lo
	sec			; set the error flag
	lda fr0			; get exponent
	bmi error		; negative numbers not handled here
	cmp #$43		; or definitely too large
	bcs error
	sbc #$3f-1		; carry is clear
	beq zero		; is exactly below one
	bcc zero		; exponent too small, make zero (is already zero)
	asl a			; from base 100 to base 10
	sta eexp
	tya			; lo (starts with zero)
loop:
	sty ztemp1+1		; clear hi
	sta ztemp1
	asl a			; *2
	rol ztemp1+1
	bcs error
	asl a
	rol ztemp1+1		; *4
	bcs error
	adc ztemp1		; add up low
	tax			; keep low
	tya			; restore hi
	adc ztemp1+1		; gives *5
	bcs error
	sta ztemp1+1		; keep hi
	txa			; restore low
	asl a			; *2 -> *10
	rol ztemp1+1		; ditto hi
	bcs error
	sta ztemp1		; keep low as well
	jsr NextFr0Digit	; get the next digit
	;; by construction, will return with C = 0
	ldy ztemp1+1		; restore hi
	adc ztemp1
	bcc noinc
	iny			; carry over
	beq error		; if wrap-around, we have a problem
noinc:	dec eexp		; next exponent value
	bne loop		; lo is already loaded in A
	tax			; delivery lo in X
zero:	
	clc			; worked!
error:	
	rts
.endproc
;;; *** NegFr0
;;; *** Compute -fr0 -> fr0
.proc	NegFr0
	lda fr0
	eor #$80		; yes, really that easy.
	sta fr0
	rts
.endproc
;;; *** ZeroFr0 (also known as ZFR0)	must go to da44
;;; *** Clear the floating point register FR0
	PlaceAt $da44
	.global ZeroFr0
.proc	ZeroFr0
	ldx #fr0
.endproc			; runs into the following
;;; *** ZeroFRX	(also known as ZFR1)	must go to da46
;;; *** Clear the floating point register at zero page X
	PlaceAt $da46
	.global ZeroFRX
.proc	ZeroFRX
	ldy #6			; are all six bytes long
.endproc			
	;; runs into the following
;;; *** ZeroRgs:	Must go to da48
;;; *** Clear the Y bytes at zero page X and up
	PlaceAt $da48
	.global ZeroRgs
.proc	ZeroRgs
	lda #$00
loop:	sta $00,x
	inx
	dey
	bne loop
	rts
.endproc
;;; *** LoadOutbuff:	Must go to da51
;;; *** Load the output buffer address into inbuf.
	PlaceAt $da51
	.global LoadOutbuff
.proc	LoadOutbuff
	lda #<outbuff
	sta inbuff
	lda #>outbuff
	sta inbuff+1
	rts
.endproc
;;; *** TimesTwo:	Must go to da5a
;;; *** Multiply ztemp4 by two.
	PlaceAt $da5a
	.global	TimesTwo
.proc	TimesTwo
	asl ztemp4+1
	rol ztemp4
	rts
.endproc
;;; *** BCDSub	(Also known as FSub)	Must go to da60
;;; *** Compute fr0-fr1 -> fr0
	PlaceAt $da60
	.global BCDSub
.proc	BCDSub
	lda fr1
	eor #$80
	sta fr1			; just add the inverse...
	;; runs into the following
.endproc
;;; *** BCDAdd	(Also known as FAdd)	Must go to da66
;;; *** Compute fr0+fr1	-> fr1
	PlaceAt $da66
	.global BCDAdd
.proc	BCDAdd
restart: 
	lda fr1			; exponent of second part
	and #$7f		; without sign
	sta eexp		; keep as exponent
	lda fr0
	and #$7f
	sec
	sbc eexp
	bpl inorder		; sort numbers such that fr0 has the larger exponent
	ldx #5
swap:	lda fr0,x
	ldy fr1,x
	sta fr1,x
	tya
	sta fr0,x
	dex
	bpl swap		; swap around
	bmi restart		; and try again
inorder:
	;; first phase:	Adjust the two exponents to be equal
	cmp #5			; difference higher than number of digit (pairs)?
	bcs nocarry		; if so, fr0 (higher exponent) is the result
	jsr AdjustFr1		; Adjust exponent of fr1 so it fits to fr0
	ldx #5			; already load:	# of mantissa bytes
	lda fr0			; signs differ (or not)?
	eor fr1
	sed
	bmi subtract		; if so, must subtract, otherwise add
	;; here:	add the two numbers
	clc
loop:	lda fr0,x
	adc fr1,x
	sta fr0,x
	dex
	bne loop
	bcc nocarry
	;; here: carry over to the next position: Make room and move
	;; Note: Numbers are normalized such that there is still "headroom" for
	;; the exponent, i.e. the exponent cannot overflow here
	ldy #1			; the digit carried over from above
	jsr PropagateCarry	; rightshift, and adjust the exponent
	bcc nocarry
subtract:
	sec
subloop:
	lda fr0,x
	sbc fr1,x
	sta fr0,x
	dex
	bne subloop
	bcs nocarry		; change of sign?
	;; here: Result has the wrong sign, invert it
	ldx #5
	sec
negloop:
	lda #0
	sbc fr0,x
	sta fr0,x
	dex
	bne negloop
	jsr NegFr0
nocarry:
	jmp Normalize
.endproc
;;; *** BCDMul:		(also known as FMUL)	Must go to dadb
;;; *** Multiply fr0 by fr1, place result in fr0
	PlaceAt $dadb
	.global BCDMul
.proc	BCDMul
	jsr MultiplySigns
	sbc #$40-1		; fixup bias C=0 by design
	sec			; carry is undefined
	adc fr1			; common exponent (C=0 here)
	bvs Error		; exponent got too large
	bmi ZeroOK		; if too small, zero the result (but no error)
	jsr PrepMulDiv
mulloop:
	lda fr0ext+5		; get lowest digit
	and #$0f
	jsr AddTimesFr1
	lda fr0ext+5
	lsr a
	lsr a
	lsr a
	lsr a			; get tenth-digit
	jsr AddTimesFr2
	jsr Fr0ExtShift		; get the next digit into Fr0Ext, move result to free digit
	dec ztemp1		; until all digits done
	bne mulloop
	lda eexp		; fixup the final
	sta fr0			; exponent	
	jmp RoundExt
	.endproc
;;; *** ZeroOK:	Clear FR0, indicate success
.proc	ZeroOK
	jsr ZeroFr0
	clc
	rts
.endproc
;;; *** Error:	Return with an error indicator
.proc	Error
	sec
	rts
.endproc
;;; *** BCDDiv		(also known as FDIV)	Must go to db28
;;; *** Compute fr0/fr1->fr0
	PlaceAt $db28
	.global BCDDiv
.proc	BCDDiv
	lda fr1			; must not be zero
	and #$7f
	beq Error
	jsr MultiplySigns	; prepare the exponent
	sec
	sbc fr1			; exponent of the quotient
	clc
	adc #$40		; fixup the bias
	bvs Error
	bmi ZeroOK		; exponent too small => result is zero
	jsr PrepMulDiv		; all necessary steps as in the multiplicative case
	;; now try to subtract fr2 from fr0ext
divloop:	
	ldy #0			; result digit
trysb1:	ldx #5
	sec
sublp1:	lda fr0ext,x
	sbc fr2,x
	sta fr0ext,x
	dex
	bpl sublp1
	bcc toomuch1		; subtracted one too much, urgh!
	iny			; add up result by one
	bne trysb1		; will always jump
toomuch1:			; restore the previous result (C=0!)
	ldx #5
addlp1:	lda fr0ext,x
	adc fr2,x
	sta fr0ext,x
	dex
	bpl addlp1
	tya
	asl a
	asl a
	asl a
	asl a			; times ten
	tay
	;; again with the original divisor for the one-digit
trysb2:	ldx #5
	sec
sublp2:	lda fr0ext,x
	sbc fr1,x
	sta fr0ext,x
	dex
	bpl sublp2
	bcc toomuch2		; subtracted one too much, urgh!
	iny			; add up result by one
	bne trysb2		; will always jump
toomuch2:			; restore the previous result (C=0!)
	ldx #5
addlp2:	lda fr0ext,x
	adc fr1,x
	sta fr0ext,x
	dex
	bpl addlp2
	sty fr0+5		; keep result
	dec ztemp1		; more digits?
	bmi done		; abort if everything done here
	;; upshift the result in fr0,fr0ext
	ldx #0
uplp:	lda fr0+1,x
	sta fr0,x
	inx
	cpx #12
	bcc uplp
	bcs divloop
done:	;; division completed here
	jsr Fr0ExtShift		; was one too much
	lda eexp		; fixup the final
	sta fr0			; exponent	
	jmp RoundExt		; and normalize
.endproc
	;; Start utility functions
;;; *** SkipBlanks:	Must go to dba1
;;; *** Skip leading blanks pointed to by CIX+(INBUFF)
	PlaceAt $dba1
	.global SkipBlanks
.proc	SkipBlanks
	lda #' '
	ldy cix
	Skip1
loop:
	iny
	cmp (inbuff),y
	beq loop
	sty cix
	rts
.endproc
;;; *** TestDigit:	Must go to dbaf
;;; *** Test whether a digit in cix+(inbuff) is a valid digit,
;;; *** returns with C=0 if so and the digit in A, otherwise
;;; *** returns with carry set.
	PlaceAt $dbaf
	.global TestDigit
.proc	TestDigit
	ldy cix
.endproc
.proc	TestDigitY
	lda (inbuff),y
	sec
	sbc #$30
	bcc error
	cmp #$0a
	rts
error:	
	sec
	rts
.endproc
;;; *** NextFr2Digit:	(dbe7)
.proc	NextFr2Digit
	ldx #fr2+1
	bne NextFr0Digit+2	; runs into the following
.endproc
;;; *** NextFr0Digit:	(dbeb)
;;; *** Get the next digit from fr0, start with the highest digit
;;; *** and by that multiply the number by ten
.proc	NextFr0Digit
	ldx #fr0+1
	ldy #4
	lda #0
loop:
	asl $4,x
	rol $3,x
	rol $2,x
	rol $1,x
	rol $0,x
	rol a
	dey
	bne loop
	rts
.endproc
;;; *** FetchDigitY:	(db94)
;;; *** Test whether a digit is in cix+(inbuff). If so, return it
;;; *** otherwise, get the character itself and increment cix.
.proc	FetchDigitY
	jsr TestDigitY
	bcc found
	lda (inbuff),y
found:
	iny
	rts
.endproc	
;;; *** Normalize:	Must go to dc00
;;; *** Ensure that the first digit in the BCD number is non-zero, i.e. make the
;;; *** number normal. Returns with C=1 if that is not possible.
	PlaceAt $dc00
	.global Normalize
.proc	Normalize
	lda #$0			; keep moving
	sta fr0+6		; move zeros in
	sta fr0+7
.endproc
	;; runs into the following
;;; *** NormalizeExt:	(dc04)
;;; *** Normalize the number in fr0,fr0ext by moving the first two digits
;;; *** of fr0ext into fr0 into the result if necessary
.proc	NormalizeExt
	ldy #$4			; maximal number of moves
	cld			; back to binary
test:	
	clc			; prepare result code (OK!)
	lda fr0			; if now the exponent is zero
	beq exit		; if so, then the number is zero (or non-normalized)
	lda fr0+1		; get first digit
	bne isnormal		; seems to be normal, test the range
	;; here: not normal. Move digits in at the price of adjusting the exponent
	tax			; keep moving
loop:	lda fr0+2,x
	sta fr0+1,x		; move the mantissa
	inx
	cpx #6			; also move fr07->fr06
	bne loop		; until all done
	dec fr0			; adjust the exponent
	dey			; maximal number of moves
	bne test		; and try again
	lda fr0+1		; still zero?
	bne isnormal
	sta fr0			; if so, mantissa is zero, set the number to zero
isnormal:			; check for the exponent size
	lda fr0			; get the exponent again
	and #$7f		; without the sign
	cmp #$72		; exponent too large (overflow?)
	;; if so, signal an error. Otherwise, keep the number denormalized.
exit:	
	rts
.endproc
;;; *** RoundExt
;;; *** Given fr0ext (fr0+6) is valid, potentially round fr0 up
;;; *** to keep the round direction consistent (round to nearest).
;;; *** This should be called instead of NormalizeExt
.proc	RoundExt
	jsr NormalizeExt
	bcs exit		; if NormalizeExt left an error
	lda fr0+6		; the digit to round to
	cmp #$50		; rounding required?
	bcc exit		; if not, ignore
	ldx #5
	sed
addlp:	lda #$00
	adc fr0,x
	sta fr0,x		; add up until done
	dex
	bne addlp
	cld
	bcc exit		; leave if no carry-over
	ldy #1			; move in a one
	jsr PropagateCarry	; round up once again....
	lda fr0			; test exponent
	and #$7f
	cmp #$72		; again, might have overflowed
exit:	rts
.endproc
;;; *** AdjustFr1		(dc3e)
;;; *** De-normalize Fr1 by right-shifting the mantissa by A digit-pairs
.proc	AdjustFr1
	pha
	clc
	adc fr1			; already adjust the exponent		
	sta fr1			; fixup exponent
	pla			; carry should be clear here
	beq exit		; no movement? Nothing to do
	eor #$ff		; one's complement
	adc #5+1		; two's complement = source = 5 - #moves
	tay			; first register to load
	ldx #5			; first target to write
loop:	
	lda fr1,y		; source
	sta fr1,x		; to the target
	dex
	dey
	bne loop		; up to, but not including the exponent
	lda #$0
loop2:
	sta fr1,x		; clear the rest
	dex
	bne loop2		; by the number of digits moved
exit:	
	rts
.endproc
;;; *** Fr0ExtShift		(dc5a)
;;; *** Downshift the fr0,fr0Ext register by two digits
.proc	Fr0ExtShift
	ldx #10			; #of digits
loop:	lda fr0,x
	sta fr0+1,x
	dex
	bpl loop
	inx
	stx fr0
	rts
.endproc
;;; *** MantissaToAscii		(dc70)
;;; *** convert the mantissa to ascii, insert the decimal dot at the
;;; *** position given by the accu
.proc	MantissaToAscii
	sta digrt		; dot position
	ldx #$00		; digit position
loop:
	lda digrt		; reached the position of the dot?
	bne nodot
	txa			; dot at first position?
	bne notfirst
	jsr InsertDigit		; insert a leading zero
notfirst:
	lda #'.'		; insert the decimal dot
	sta (inbuff),y
	iny
nodot:	
	cpx #5			; when all eight digits done
	bcs exit
	dec digrt
	lda fr0+1,x		; next number
	lsr a
	lsr a
	lsr a
	lsr a			; tenth-digit
	bne nonzero		; due to normalization, the first digit might be zero
	cpx #0			; start of the number? If not, no suppression
	bne nonzero
	lda digrt		; decimal dot written?
	bpl ignorezero		; if not yet, may strip the zero
	txa			; zero A again
nonzero:
	jsr InsertDigit
ignorezero:	
	lda fr0+1,x		; and the one-digit
	and #$0f
	jsr InsertDigit
	inx
	bne loop
exit:	
	rts
.endproc
;;; *** InsertDigit		(dc9a)
;;; *** Insert the digit in A into the inbuff output buffer
.proc	InsertDigit
	ora #'0'
	sta (inbuff),y
	iny
	rts
.endproc
;;; *** StripTrailingZeros	(dca4)
;;; *** Remove zeros in inbuf with cix = y pointing behind the end of the buffer
.proc	StripTrailingZeros
loop:	dey
	lda (inbuff),y
	cmp #'.'		; end of fraction?
	beq abort
	cmp #'0'
	beq loop		; continue scanning
	Skip1			; leave the nonzero digit untouched
abort:	dey			; also remove the decimal dot, everything behind it is zero
	lda (inbuff),y
	rts
.endproc
;;; *** MultiplySigns		(dccf)
;;; *** Extract the common sign of the result of a multiplication or
;;; *** division, place the result in nsign, make fr1 positive, place
;;; *** exponent of fr0 in A
.proc	MultiplySigns
	lda fr0
	eor fr1
	and #$80		; sign only
	sta nsign
	asl fr1
	lsr fr1			; remove sign bit
	lda fr0
	and #$7f
	rts
.endproc
;;; *** PrepMulDiv		(dce0)
;;; *** A couple of common preparation steps for
;;; *** multiplication and division
;;; *** expects the new exponent in A
.proc	PrepMulDiv
	ora nsign
	sta eexp
	lda #0
	sta fr0
	sta fr1			; clear exponents, registers are required for other purposes
	;; fr1->fr2, fr0->fr0Ext
	ldx #5			; also #of digits
	stx ztemp1
cploop:	lda fr1,x
	sta fr2,x
	lda fr0,x
	sta fr0ext,x
	dex
	bpl cploop
	jsr NextFr2Digit	; upshift for enhanced precision
	sta fr2			; keep in exponent buffered
	sed			; also already required
	jmp ZeroFr0		; and clear result register
.endproc
;;; *** TimesTen:
;;; *** Multiply fr0 by ten
.proc	TimesTen
	jsr NextFr0Digit	; upscale by 10
	tay			; highest digit
	beq NoCarry		; was zero->no downscaling necessary
.endproc
	;; runs into the following
;;; *** PropagateCarry
;;; *** Shift the number by a digit to the right,
;;; *** propagate the carry correctly by rounding up
;;; *** install Y in the most significant digit.
.proc	PropagateCarry
	sed
	lda fr0+5		; this digit will go away
	cmp #$50		; round up?
	ldx #4
shlp:	lda #$00
	adc fr0,x		; downshift the number
	sta fr0+1,x
	dex
	bne shlp
	tya			; next highest digit
	adc #$00		; advance carry
	sta fr0+1
	cld
	inc fr0			; next higher exponent, then. Should not overflow
.endproc
.proc	NoCarry
	rts
.endproc
;;; *** EvalPoly	Must go to dd40
;;; *** Evaluate the polynomial with coefficients at (x,y) (lo,hi)
;;; *** with their number in A, at the location in fr0.
;;; *** This is a straight-forward implementation of the Horner scheme.
	PlaceAt $dd40
	.global EvalPoly
.proc	EvalPoly
	stx flpt2
	sty flpt2+1
	sta esign		; used as another temporary here
	ldx #<polybuff
	ldy #>polybuff
	jsr StoreFr0IndXY	; keep the argument there

	ldx flpt2
	ldy flpt2+1
	jsr LoadFr0IndXY	; get the constant term (fr0 will remain the partial result)
	clc
loop:
	dec esign		; abort when all done.
	beq exit
	
	ldx #<polybuff
	ldy #>polybuff
	jsr LoadFr1IndXY	; restore the argument->fr1
	
	jsr BCDMul		; times X
	bcs exit
	;; advance to the next coefficient
	lda flpt2
	adc #6			; C=0, size of a BCD number
	sta flpt2
	bcc nocarry
	inc flpt2+1
nocarry:
	tax
	ldy flpt2+1
	jsr LoadFr1IndXY	; next coefficient->fr1
	jsr BCDAdd		; add to the current result
	bcc loop
exit:
	rts
.endproc
;;; *** LoadFr0IndXY	Must go to dd89
;;; *** Load fr0 from the data pointed to by (x,y) (lo,hi)
	PlaceAt $dd89
	.global LoadFr0IndXY
.proc	LoadFr0IndXY
	stx flptr
	sty flptr+1
	.endproc
	;; runs into the following
;;; *** LoadFr0IndPtr	Must go to dd8d
;;; *** Load fr0 from the data pointed to by flptr
	PlaceAt $dd8d
	.global LoadFr0IndPtr
.proc	LoadFr0IndPtr
	ldy #5			; number of bytes to move
loop:	lda (flptr),y
	sta fr0,y
	dey
	bpl loop
	rts
.endproc
;;; *** LoadF10IndXY	Must go to dd98
;;; *** Load fr1 from the data pointed to by (x,y) (lo,hi)
	PlaceAt $dd98
	.global LoadFr1IndXY
.proc	LoadFr1IndXY
	stx flptr
	sty flptr+1
	.endproc
	;; runs into the following
;;; *** LoadFr1IndPtr	Must go to dd9c
;;; *** Load fr1 from the data pointed to by flptr
	PlaceAt $dd9c
	.global LoadFr1IndPtr
.proc	LoadFr1IndPtr
	ldy #5			; number of bytes to move
loop:	lda (flptr),y
	sta fr1,y
	dey
	bpl loop
	rts
.endproc
;;; *** StoreFr0IndXY	Must go to dda7
;;; *** Store fr0 in the data pointed to by (x,y)
	PlaceAt $dda7
	.global StoreFr0IndXY
.proc	StoreFr0IndXY
	stx flptr
	sty flptr+1
	.endproc
	;; runs into the following
;;; *** StoreFr0IndPtr	Must go to ddab
;;; *** store fr0 in the data pinted to by flptr
	PlaceAt $ddab
	.global StoreFr0IndPtr
.proc	StoreFr0IndPtr
	ldy #5
loop:	lda fr0,y
	sta (flptr),y
	dey
	bpl loop
	rts
.endproc
;;; *** Fr0ToFr1	Must go to ddb6
;;; *** move the contents of fr0 to fr1
	PlaceAt $ddb6
	.global Fr0ToFr1
.proc	Fr0ToFr1
	ldx #5
loop:	lda fr0,x
	sta fr1,x
	dex
	bpl loop
.endproc
	;; runs into the following
;;; *** PowError
.proc	PowError
	rts
.endproc
;;; *** BCDExp		Must go to ddc0
;;; *** compute exp(fr0)->fr0
	PlaceAt $ddc0
	.global BCDExp
.proc	BCDExp
	ldx #<LogEInv
	ldy #>LogEInv
	jsr LoadFr1IndXY
	jsr BCDMul		; multiply by log_10(e) in preparation for the next...
	bcs PowError
.endproc
	;; runs into the following
;;; *** BCDPow10	Must go to ddcc
;;; *** compute 10^fr0 -> fr0
	PlaceAt $ddcc
	.global BCDPow10
.proc	BCDPow10
	lda #0
	sta digrt		; later output shift
	lda fr0
	sta fchflg		; keep exponent
	and #$7f		; compute the absolute value
	sta fr0
	sec
	sbc #$40		; difference to 1.0
	bcc tiny		; |x|-1 small => use an exponential series
	jsr Fr0ToFr1		; keep as temporary
	jsr BCDToIntDown	; get integer part of |x|. Must round down, would ruin numerics
	bcs huge		; did not fit into an int
	stx digrt		; store low digit
	tya			; and high
	bne huge		; too large, either 0 or error
	jsr IntXYToBCD		; back to a floating point
	jsr NegFr0		; make this negative
	jsr BCDAdd		; and keep only the fractional part of it.
	;; number is now normalized between 0 and 1.
tiny:
	lda #ExpPolySize	; number of coefficients
	ldx #<ExpPoly
	ldy #>ExpPoly
	jsr EvalPoly		; approximate now sqrt(10^x) as polynomial around 1.0
	jsr Fr0ToFr1
	jsr BCDMul		; 10^x as result
	;; Now scale by the integer part:	
	;; The result is by one power of ten too small, for better rounding,
	;; now fix for that
	lsr digrt		; exponent to scale with
	bcc even		; even power of 100
	jsr TimesTen
even:	
	lda fr0			; get exponent and adjust by the scale factor
	sec			; +1-> * 100
	adc digrt		; adjust the exponent
	bvs huge		; if too large, nothing we can do
	sta fr0			; and keep the result
	;; now finally, keep care of negative values.
	lda fchflg		; if positive
	bpl exit		; we are done. Otherwise...
	jsr Fr0ToFr1
	ldx #<One
	ldy #>One
	jsr LoadFr0IndXY	; load 1.0 to fr0
	jsr BCDDiv		; compute 1.0/fr0
	bcc exit
huge:				; here |x| is huge. If x < 0, no problem, set the result zero
	sec
	lda fchflg		; restore sign
	bpl exit		; if positive, we do have a problem
	jsr ZeroFr0		; no problem
exitfine:
	clc
exit:
	rts
.endproc
;;; The following is the minimax polynomial for the exp approximation:
;;;
;;; m:= x-> 1.0 + 1.151292547*x + 0.662737239*x^2 + 0.254335151*x^3 + 0.07320133034*x^4 + 0.01686348089*x^5 + 0.00321714\
;;; 053*x^6+0.000555180562*x^7 + 0.00005792897*x^8 + 0.0000176618724*x^9;
;;; 
	ExpPolySize	=	10
ExpPoly:
        .byte $3d,$01,$76,$81,$87,$00 ; $00
        .byte $3d,$05,$79,$28,$97,$00
        .byte $3d,$55,$51,$80,$56,$11 ; $11
        .byte $3e,$03,$21,$71,$40,$00 ; $00
        .byte $3e,$16,$86,$34,$80,$00 ; $00
        .byte $3e,$73,$20,$13,$30,$95 ; $95
        .byte $3f,$02,$54,$33,$51,$32 ; $32
        .byte $3f,$06,$62,$73,$72,$39
	.byte $3f,$11,$51,$29,$25,$47
	.byte $3f,$10,$00,$00,$00,$00
One:	
	.byte $40,$01,$00,$00,$00,$00
;;; *** BCDFract	Must go to de95
;;; *** compute the rational function (fr0-A)/(fr0+A)
;;; *** where A is pointed to by (X,Y) 
	PlaceAt $de95
	.global BCDFract
.proc	BCDFract
	stx flpt2
	sty flpt2+1
	jsr LoadFr1IndXY	; address pointed to to fr1
	ldx #<fr2
	ldy #>fr2
	jsr StoreFr0IndXY	; keep fr0 in fr2
	jsr BCDAdd		; fr0+A->fr0
	ldx #5			; move fr2 back to fr0, and keep the result in fr2
loop:	ldy fr0,x
	lda fr2,x
	sta fr0,x
	tya
	sta fr2,x
	dex
	bpl loop
	ldx flpt2
	ldy flpt2+1
	jsr LoadFr1IndXY	; again, restore A
	jsr BCDSub		; fr0-A->fr0
	ldx #<fr2		; former result
	ldy #>fr2
	jsr LoadFr1IndXY	; load from fr2
	jmp BCDDiv		; and compute the result
.endproc
;;; *** BCDLog		Must go to decd
;;; *** compute the natural logarithm of fr0 -> fr0
	PlaceAt $decd
	.global BCDLog
.proc	BCDLog
	jmp BCDLogEntry		; jump around the next label, urgh.
.endproc
;;; *** BCDLog10	Must go to ded1
	PlaceAt $ded1
	.global BCDLog10
.proc	BCDLog10
	sec
	lda fr0			; exponent? The number must be nonzero, and positive
	beq exit
	bmi exit
	sbc #$40		; difference to zero exponent
	asl a			; exponent for the base 10
	sta digrt
	lda #$40		; and normalize the exponent
	sta fr0			; so that the number is now >= 1 and < 10
	lda fr0+1		; get the next digit
	and #$f0		; topmost digit nonzero? If zero, scale the number
	beq normal		; as requested
	jsr TimesTen		; upscale
	inc digrt
	dec fr0			; but I wanted to divide by 10
normal:	
	ldx #<LogCoeffA
	ldy #>LogCoeffA		; this is sqrt(10)
	jsr BCDFract		; make a rational approximation at first
	jsr Fr0ToFr1		; keep result in fr1
	ldx #<expbuff
	ldy #>expbuff
	jsr StoreFr0IndXY	; keep temporary
	jsr BCDMul		; square result
	lda #LogPolySize	; number of coefficients
	ldx #<LogPoly
	ldy #>LogPoly
	jsr EvalPoly		; approximate now log(((X-A)/(X+A))^2) as polynomial
	ldx #<expbuff
	ldy #>expbuff		; restore argument
	jsr LoadFr1IndXY	; and keep result in fr1
	jsr BCDMul		; *X again
	ldx #<LogCoeffB
	ldy #>LogCoeffB		; and the final part:	B=0.5 (would be log(sqrt(10)))
	jsr LoadFr1IndXY
	jsr BCDAdd		; in total:	p(F^2) * F + B where F = (X-A)/(X+A)

	;; must now add the exponent value again (using the functional equation of log)
	jsr Fr0ToFr1
	ldy #0			; hi-part
	lda digrt
	bpl ispos
	tya			; Zero again
	sec
	sbc digrt		; invert
ispos:	tax			; lo-part
	jsr IntXYToBCD		; into a BCD number
	lda digrt		; was negative?
	bpl ipo2
	jsr NegFr0
ipo2:
	jsr BCDAdd
exit:	
	rts
.endproc
;;; *** BCDLogEntry
;;; *** compute the natural logarithm of fr0 -> fr0
.proc	BCDLogEntry
	jsr BCDLog10		; First the regular Log(10)
	bcs exit
	ldx #<LogE
	ldy #>LogE
	jsr LoadFr1IndXY	; conversion factor
	jsr BCDMul
exit:
	rts
.endproc
LogCoeffA:
	.byte $40,$03,$16,$22,$77,$66 ; sqrt(10)
LogE:
	.byte $40,$02,$30,$25,$85,$09 ; ln(10)
LogEInv:
	.byte $3f,$43,$42,$94,$48,$19 ; 1/ln(10)
	
	PlaceAt $df6c		; basic requires this here...
	.global	OneHalf
LogCoeffB:
OneHalf:	
	.byte $3f,$50,$00,$00,$00,$00 ; 1/2
	

	
;;; The following is the minimax polynomial for the log approximation:
;;; 0.8685889625 + (0.2895298827
;;; + (0.1737063251 + (0.1243413535 + (0.09348240142 + (0.09879885753 + (-0.004411453333 + 0.1806407195
;;; x) x) x) x) x) x) x
;;;
;;; It causes an approximation error that is as small as 4^-11.
;;;
;;; Unfortunately, we can only represent it with less precision, see below. The 10th order
;;; original representation is overshooting, it is as precise as this one...
;;; 
	LogPolySize	=	8 ; number of coefficients
LogPoly:
	.byte $3f,$18,$06,$40,$71,$95
	.byte $be,$44,$11,$45,$33,$33
	.byte $3f,$09,$87,$98,$85,$75
	.byte $3f,$09,$34,$82,$40,$14
	.byte $3f,$12,$43,$41,$35,$36
	.byte $3f,$17,$37,$06,$32,$51
	.byte $3f,$28,$95,$29,$88,$27
	.byte $3f,$86,$85,$88,$96,$25
	;;
	;; More constants used by Basic
;;;
;;; The minimax polynomial for the BASIC atan approximation, using atan(x) = x*p(x^2)
;;; for x in (-1..1):
;;; 1. + x (-0.3333332767 + (0.1999967533 + (-0.1427971293 + (0.1105670842 + (-0.08803518380 + (
;;; 0.06721148457 + (-0.04436678161 + (0.02224071992 + (-0.007167583821 + 0.001082076940 
;;; 
;;; This is actually completely overshooting, but I can't change it - it's in the BASIC
;;; ROM. 8 terms would have been sufficient for a precision of 10^-9, which is the best
;;; we can hope for.
;;; 
	
	PlaceAt $dfae
	AtnPolySize	=	11 ; number of coefficients, not adjustable, since it's in the BASIC ROM.
	.global AtnPoly
AtnPoly:
	.byte $3e,$10,$82,$07,$69,$40
	.byte $be,$71,$67,$58,$38,$21
	.byte $3f,$02,$22,$40,$71,$99
	.byte $bf,$04,$43,$66,$78,$16
	.byte $3f,$06,$72,$11,$48,$46
	.byte $bf,$08,$80,$35,$18,$38
	.byte $3f,$11,$05,$67,$08,$42
	.byte $bf,$14,$27,$97,$12,$93
	.byte $3f,$19,$99,$96,$75,$33
	.byte $bf,$33,$33,$33,$27,$67
	PlaceAt $dfea
	.global	NearOne
NearOne:	
	.byte $3f,$99,$99,$99,$99,$99 ; also required for ATN
	PlaceAt $dff0
	.global PiOver4
PiOver4:
	.byte $3f,$78,$53,$98,$16,$34 ; Pi/4
	
