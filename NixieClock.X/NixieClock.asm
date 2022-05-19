
;    <Nixie Tube Clock>
;    Copyright (C) <2013>  <Jarrett R. Cigainero>
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses>

; CONFIG
__config 0x3F30


PROCESSOR	PIC16F628A
status		equ	0x03
rp0		equ	0x05
rp1		equ	0x06
c		equ	0x00
z		equ	0x02
endb		equ	0x07
eeadr		equ	0x9B	;bank1
eedata		equ	0x9A	;bank1
eecon1		equ	0x9C	;bank1
eecon2		equ	0x9D	;bank1
porta		equ	0x05
portb		equ	0x06
opt_reg		equ	0x81
trisa		equ	0x85
trisb		equ	0x86
vrcon		equ	0x9F
txsta		equ	0x98
ccp1con		equ	0x17
cmcon		equ	0x1F
tmr2		equ	0x11
t2con		equ	0x12
mset		equ	0x06				;Minute set button	PortA, RA6
hset		equ	0x07				;Hour set button	PortA, RA7
power		equ	0x04				;Power supply detect	PortA, RA4
TclkOut		equ	0x04				;Clock Pulse out for calibration    PortB RB4
t1con		equ	0x10	;bank0
pcon		equ	0x8E	;bank1
pie1		equ	0x8c	;bank1
pir1		equ	0x0c	;bank0
intcon		equ	0x0b	;bank0
tmr1l		equ	0x0e	;bank0
tmr1h		equ	0x0f	;bank0
pr2		equ	0x92	;bank1
dwt1		equ	0x21	;bank0
dwt2		equ	0x22	;bank0
d1		equ	0x23	;bank0		Digit number 1 (from right to left faceing the clock)
d2		equ	0x24	;bank0		Digit number 2 (from right to left faceing the clock)
d3		equ	0x25	;bank0		Digit number 3 (from right to left faceing the clock)
d4		equ	0x26	;bank0		Digit number 4 (from right to left faceing the clock)
sst		equ	0x28	;bank0		Part of our Status and W REGs temps
sw		equ	0x29	;bank0		Part of our Status and W REGs temps
second		equ	0x2a	;bank0		Counts to 60 before rolling over
dwtf		equ	0x2b	;bank0		Display timeing var
dwts		equ	0x2c	;bank0		Display timeing var
dwtcf		equ	0x2d	;bank0		Display timeing var
dwtcs		equ	0x2e	;bank0		Display timeing var
udstat1		equ	0x2f	;bank0
udstat2		equ	0x30	;bank0
do1		equ	0x31	;bank0		Digit number 1 "OLD data for display fadeing" (from right to left faceing the clock)
do2		equ	0x32	;bank0		Digit number 2 "OLD data for display fadeing" (from right to left faceing the clock)
do3		equ	0x33	;bank0		Digit number 3 "OLD data for display fadeing" (from right to left faceing the clock)
do4		equ	0x34	;bank0		Digit number 4 "OLD data for display fadeing" (from right to left faceing the clock)
nset		equ	0x35	;bank0
tnset		equ	0x36	;bank0
ftime		equ	0x37	;bank0
ftime1		equ	0x38	;bank0
Ltmr1l		equ	0x39	;bank0
Ltmr1h		equ	0x3A	;bank0
tcirqreg	equ	0x3B	;bank0
cd1		equ	0x3C	;bank0		Digit number 1 for displaying (from right to left faceing the clock)
cd2		equ	0x3D	;bank0		Digit number 2 for displaying (from right to left faceing the clock)
cd3		equ	0x3E	;bank0		Digit number 3 for displaying (from right to left faceing the clock)
cd4		equ	0x3F	;bank0		Digit number 4 for displaying (from right to left faceing the clock)
fadetime	equ	0x40	;bank0		Var to tell us what timeing sub to use
D1chk		equ	0x41	;bank0		Time digit Checkup. It's a copy of the time data.
D2chk		equ	0x42	;bank0		Time digit Checkup. It's a copy of the time data.
D3chk		equ	0x43	;bank0		Time digit Checkup. It's a copy of the time data.
D4chk		equ	0x44	;bank0		Time digit Checkup. It's a copy of the time data.
calDsp		equ	0x45	;bank0
calPrep		equ	0x46	;bank0
dspBright	equ	0x47	;bank0
calPlsOut	equ	0x48	;bank0
scsv		equ	0x49	;bank0		screen saver bit
dithcal		equ	0x4A	;bank0
dithcnt		equ	0x4B	;bank0
Ltmr1lHIGH	equ	0x4C	;bank0		Ltmr1lHIGH = Ltmr1l + 1
;****************  Address 0x00 in ROM  ****************
	org	0x00
	goto	init
;****************  IRQ vector *******************
	org	4					;Interrupt Vector
	movwf	sw			;we need to save our W and Status REGs before we begine our IRQ sub.
	movf	status, 0x00
	movwf	sst

;dither stuff for timeing adjustment.
	movf	dithcal, 0x00		;If (dithcal == 0) goto zroed
	addlw	d'5'			;Add 5 first then check to see if dithcal is equal to 5 to check if zero.
	sublw	d'5'			;
	btfsc	status, z		;We check to see if it's 0 like this because sublw'ing a 0 doesn't seem to actually do anything with the status bits. IDK why.
	goto	zroed			;Do not dither when 'dithcal' == 0
	decfsz	dithcnt			;Else decrease dithcnt, chk if zero, reset if it is.
	goto	dthchk
	movlw	d'10'
	movwf	dithcnt

;Do a comparison with 'dithcnt' and 'dithcal'
;acts as a sort of "PWM" that switches the duty cycle between two 'Ltmr1l' sets.
;'dithcal' - 'dithcnt' check to see if 'dithcnt' > 'dithcal'
;if yes, then C = 1 and don't go to "zroed"
dthchk	movf	dithcnt, 0x00		;W
	subwf	dithcal, 0x00		;F
	btfss	status, c		;F - W. If result is equal or positive C = 1
	goto	zroed			;Go here if dithcnt < dithcal
	movf	Ltmr1l, 0x00
	addwf	tmr1l, 0x01		;add the two together.
	goto	roed
;If 'dithcal' is = 0 then we don't dither between two setpoints.
zroed	movf	Ltmr1lHIGH, 0x00	;Ltmr1lHIGH = Ltmr1l + 1
	addwf	tmr1l, 0x01		;add the two together.
roed	btfsc	status, c		;check to see if the add caused a rollover.
	goto	addtime
	movf	Ltmr1h, 0x00
	movwf	tmr1h
	goto	count			;This routin down near the bottom of the source...
addtime	movf	Ltmr1h, 0x00
	addlw	d'1'
	movwf	tmr1h
	goto	count

;****************  INIT  ************************
;********* Prepar our inital variables **********
;		org	20
init		bsf	status,rp0			;go into bank1
		bsf	pcon, 0x03			;set to 4 MHZ
		movlw	0xF0
		movwf	trisa
		movlw	0xE0
		movwf	trisb
		movlw	b'00000001'
		movwf	pie1
		CLRF    vrcon
	        CLRF    txsta
		MOVLW	b'00100000'			;Set our timer0 to not run.
		MOVWF	opt_reg
		movlw	d'80'
		movwf	pr2
		bcf		status,rp0		;go back to bank0
		call	dwait2
		movlw	b'00000000'
		movwf	porta
		movlw	b'00001111'
		movwf	t1con
		movlw	b'11000000'
		movwf	intcon
		CLRF    ccp1con
		CLRF    cmcon
		bcf	calDsp, 0x00
		movlw	b'00000000'
		movwf	t2con
		clrf	calPlsOut
		movlw	d'10'
		movwf	dithcnt
		clrf	dithcal
;********** clock timeing adjust ***************
;--Moved to very bottom.
		call	clkupdt			    ;Gets clock calibration data from EEPROM on startup.
;************************************************
;display timeing.
		movlw	d'120'
		movwf	dwtcs
		movlw	d'1'
		movwf	dwtcf

;******** Initial time check on reset or powerup ***************************
;The last thing we want to do is unknowingly display the incorrect time.

		clrf	nset			;Blink our numbers on a CPU reset because we don't know if the time is correct.
		call	dtachk			;Check for time data or memory curruption.
;***************************************************************************
		movlw	d'6'
		movwf	ftime1			;how many times we scan through the diaplay before changing the dutycycle during a fade sequence. Changes how fast we fade.
		bcf	udstat1, 0x00

;************** Main Routine  *************
;******************************************
;******************************************
;variables "do1 - do4" is "data old"
;variables "d1 - d4" is "new data"

disp		call	scchk
		btfsc	porta, mset		;check to see if minute button has been pressed.
		goto	setm
		btfsc	porta, hset		;check to see if hour button has been pressed.
		goto	seth
		btfss   porta, power		;check to see if power has failed.
		call    psleep
;************** Time check *******************
;Look for currupted data to see if we need to reset the time
;and/or blink our numbers.
		movf	nset, 0x00		;time curruption check 1 for blinking stuff
		sublw	d'85'			;check to see if "nset" is 85 or not, we want to blink our numbers if it isin't 85.
		btfsc	status,z
		goto	dispnxt
; Blinking Display
		call	uod
		btfss	second, 0x00
		goto	disoff
		call	dsct			;we want to display our current set time even if we don't know if it's correct, but make it blink if we think it might not be correct.
		movlw	d'255'
		movwf	dwtcs
		movlw	d'255'
		movwf	dwtcf
		call	dct			;now display it!
		call	ddrset
		goto	disp

disoff		movlw	b'00000000'		;Blank the display.
		movwf	porta
		goto	disp
;**********************************************
;Screen Saver Check, Cycle through all numbers
;once at 2:00. Supposedly this helps Nixie Tubes last longer?
;IDK, but it is neat looking!
;Why 2:00? because it's easy to check for that time, and it's an odd time
;People are less likely to need to know that it's 2:00

scchk		movf	d4, 0x00
		sublw	d'1'
		btfsc	status, z
		return

		movf	d3, 0x00
		sublw	d'1'
		btfss	status, z
		goto	chktwo
		bcf	scsv, 0x00		;Clear this if it is 1:00, this enables the screen saver to run.
		return
;Only run it once.
chktwo		movf	d3, 0x00
		sublw	d'2'
		btfss	status, z
		return
		btfsc	scsv, 0x00		;Skip the screen save if it has already been run.
		return
		movlw	d'255'
		movwf	dwtcs
		movlw	d'255'
		movwf	dwtcf
		call	scrsav
		call	ddrset
		return

;***************************************
; ***** Low power shut down mode *******

psleep		CLRF    portb
		CLRF    porta
		bsf		status,rp0	;go into bank1
		movlw	0xFF
		movwf	trisa			;change all I/O to inputs. All of them.
		movwf	trisb
		;bcf	pcon, 0x03		;set main clock to 48 KHZ
		;pointless to set to 48KHZ, the same number of instructions runs regardless of speed and thuse the micro-controller will consume roughly the same amount of power each second that it updates the time while in low power mode.
		bcf		status,rp0	;go into bank0
psleep2		SLEEP				;Timer 1 is still running to keep the time.
		BTFSS   porta, power		;Test to see if the power is back on.
		GOTO    psleep2			;If power is still off then we need to go back to sleep.
;Wake back up and re-init what is needed.
		bsf		status,rp0	;go into bank1
		;bsf	pcon, 0x03		;set main clock back to 4 MHZ
		movlw	0xF0
		movwf	trisa			;change all I/O back to normal on wake up.
		movlw	0xE0
		movwf	trisb
		bcf	status,rp0	;go into bank0
		BTFSS	porta, mset		;When power is switched on check buttons for calibration mode.
		RETURN
		BTFSS	porta, hset
		RETURN
		GOTO	calibrate		;Go to calibration mode if both buttons are held on power up.

;*********** Calibration mode ************************
calibrate	bsf	calDsp, 0x00
		clrf	cd1
		clrf	cd2
		clrf	cd3
		clrf	cd4
		call	dct
;Loop until buttons are released.
calib2		call	calOput
		BTFSC	porta, mset
		GOTO	calib2
		BTFSC	porta, hset
		GOTO	calib2
;		bsf	t2con, 0x02	    ;turn on timer2
;		bsf	status, rp0	    ;goto bank1
;		bsf	pie1, 0x01	    ;enable timer2 IRQ
;		bcf	pie1, 0x00	    ;disable timer1 IRQ
;		bcf	status, rp0	    ;goto bank0
calib1		BTFSC	porta, mset	    ;Test for Down button
		call	caliDown
		BTFSC	porta, hset	    ;Test for Up button
		call	caliUp
		BTFSS	porta, power	    ;Test power, removing power ends calibration mode.
		GOTO	endCal
		call	dzchk		    ;check for roll over. If Ltmr1l is 255 then dithcal needs to be 0.
		call	calNumPrep	    ;prepair number to be displayed.
		movlw	d'20'
		movwf	dspBright
		movlw	d'255'
		movwf	dwtcs
		movlw	d'255'
		movwf	dwtcf
drepeat		call	dct		    ;now display it!
		call	ddrset
		decfsz	dspBright
		goto	drepeat
		goto	calib1

calNumPrep	movf	Ltmr1l, 0x00
		movwf	calPrep
		movf	dithcal, 0x00
		movwf	cd1
		clrf	cd2
		clrf	cd3
		clrf	cd4
calCntUp	movf	calPrep, 0x00
		sublw	d'0'
		btfsc	status, z
		return
		decf	calPrep
		incf	cd2
		movf	cd2, 0x00
		sublw	d'10'
		btfss	status,z
		goto	calCntUp
		clrf	cd2
		incf	cd3
		movf	cd3, 0x00
		sublw	d'10'
		btfss	status,z
		goto	calCntUp
		clrf	cd3
		incf	cd4
		movf	cd4, 0x00
		sublw	d'10'
		btfss	status,z
		goto	calCntUp
		return

;Write the data to the EEPROM
endCal		clrf	calPlsOut
;		bsf	status, rp0	    ;goto bank1
;		bcf	pie1, 0x01	    ;disable timer2 IRQ
;		bsf	pie1, 0x00	    ;enable timer1 IRQ
;		bcf	status, rp0	    ;goto bank0
;		bcf	t2con, 0x02	    ;turn off timer2
		bcf	calDsp, 0x00
		movf	Ltmr1l, 0x00	    ;Get the calibration data ready.
		bsf	status, rp0	    ;goto bank1
		movwf	eedata
		bsf	eecon1, 0x02	    ;enable write
		movlw	0x00
		movwf	eeadr		    ;address 0
		bcf	intcon, 0x07	    ;disable IRQs
		btfsc	intcon, 0x07	    ;make sure they are disabled!
		goto	$-2		    ;Cha Cha Real Smooth.
		movlw	0x55		    ;whats the pass code?
		movwf	eecon2
		movlw	0xAA
		movwf	eecon2
		bsf	eecon1, 0x01	    ;now write it
		bsf	intcon, 0x07	    ;re-enable our IRQs
wrCHK		btfsc	eecon1, 0x01	    ;make sure the write is compleat before disableing it!
		goto	wrCHK
		bcf	eecon1, 0x02	    ;disable write
		bcf	status, rp0	    ;goto bank0
;extended Calibration data write for dithering data. Displays in far right digit numbers 0 - 9.
		movf	dithcal, 0x00	    ;Get the calibration data ready.
		bsf	status, rp0	    ;goto bank1
		movwf	eedata
		bsf	eecon1, 0x02	    ;enable write
		movlw	0x01
		movwf	eeadr		    ;address 0
		bcf	intcon, 0x07	    ;disable IRQs
		btfsc	intcon, 0x07	    ;make sure they are disabled!
		goto	$-2		    ;Cha Cha Real Smooth.
		movlw	0x55		    ;whats the pass code?
		movwf	eecon2
		movlw	0xAA
		movwf	eecon2
		bsf	eecon1, 0x01	    ;now write it
		bsf	intcon, 0x07	    ;re-enable our IRQs
wrCHK2		btfsc	eecon1, 0x01	    ;make sure the write is compleat before disableing it!
		goto	wrCHK2
		bcf	eecon1, 0x02	    ;disable write
		bcf	status, rp0	    ;goto bank0
		return


;check for roll over. If Ltmr1l is 255 then dithcal needs to be 0.
dzchk		movf	Ltmr1l, 0x00
		sublw	0xFF
		btfss	status, z
		return
		movf	dithcal, 0x00
		sublw	0x00
		btfsc	status, z
		return
		clrf	dithcal
		return

;*******************************************************
;User input for calibration mode.
caliDown	call	dct
		btfsc	porta, mset
		goto	caliDown
		decf	dithcal
		movlw	d'10'
		subwf	dithcal, 0x00
		btfss	status, c
		return
		movlw	d'9'
		movwf	dithcal
		decf	Ltmr1l
		return

caliUp		call	dct
		btfsc	porta, hset
		goto	caliUp
		incf	dithcal
		movlw	d'10'
		subwf	dithcal, 0x00
		btfss	status, c
		return
		movlw	d'0'
		movwf	dithcal
		incf	Ltmr1l
		return
;********************************
;Pulse Output.
calOput		btfsc	tmr1l, 0x04	;this is the clock pulse output for calibration, the output should be 1024Hz
		goto	calpls
		bcf	portb, TclkOut
		return
calpls		bsf	portb, TclkOut
		return

;End of calibration Mode.
;*****************************************************
;*****************************************************
;Some display routin for fading the display.
dispnxt		call	dsot			;we want to display our Old time, this is what we fade from
		call	dct			;now display it!

		btfss	udstat1, 0x00		;has our time changed? if so then we need to start fading to our next number.
		goto	disp			;skip this if we are fading...
dispf		call	dsct			;we want to display our current set time, this is what we fade to
		call	dct			;now display it!

;************ now fade using pwm **************
		decfsz	ftime
		goto	disp			;scan through the display a few times before changing the dutycycle.
		movf	ftime1, 0x00		;move the contents of "ftime1" to "ftime"
		movwf	ftime
		incf	dwtcf			;this is our timeing var for "dwaitf"
		decfsz	dwtcs			;this is our timeing var for "dwaits" we keep fading until "dwaits" is 0
		goto	disp			;skip if "dwaits" is 0
		movlw	d'0'
		movwf	udstat1			;this is the end of our fade, reset "udstat1" to 0
		call	ddrset			;reset our duty cycle
		call	uod
		goto	disp

;******************************************
;Update old time data.
uod		movf	d1, 0x00		;move data from vars "d1 - d4" to vars "do1 - do4"
		movwf	do1
		movf	d2, 0x00
		movwf	do2
		movf	d3, 0x00
		movwf	do3
		movf	d4, 0x00
		movwf	do4
		return
;******************************************
;hour setting sub.

seth		bsf	scsv, 0x00
		movlw	b'00000000'
		movwf	intcon			;disable the IRQ so we stop couinting time.
		movlw	d'0'
		movwf	second			;reset our seconds var, for those people who like to keep clocks exactly synced
		bsf	tcirqreg, 0x00		;clear for IRQ, set for Setting the time!

		movf	nset, 0x00
		sublw	d'85'			;Just stop blinking the display on the first button press, do not change the time yet.
		btfsc	status,z
		call	tch			;Increase Hour
		movlw	d'85'
		movwf	nset			;move "85" into the var "nset" so that we know later to not blink our display


disp1		btfss	porta, hset		;check to see if we are still holding our hour button.
		goto	disp0			;loop this display sub until released, then goto disp0 down near the bottom.
		call	ddrset
		call	dsct			;we want to display our current set time
		call	dct
		goto	disp1

;******************************************
;minute setting sub.

setm		bsf	scsv, 0x00
		movlw	b'00000000'
		movwf	intcon			;disable the IRQ so we stop couinting time.
		movlw	d'0'
		movwf	second			;reset our seconds var, for those people who like to keep clocks exactly synced
		bsf	tcirqreg, 0x00		;clear for IRQ, set for Setting the time!

		movf	nset, 0x00
		sublw	d'85'			;Just stop blinking the display on the first button press, do not change the time yet.
		btfsc	status,z
		call	tcm			;Increase Minutes
		movlw	d'85'
		movwf	nset			;move "85" into the var "nset" so that we know later to not blink our display

disp2		btfss	porta, mset		;check to see if we are still holding our minute button.
		goto	disp0			;loop this display sub until released, then goto disp0 down near the bottom.
		call	ddrset
		call	dsct			;we want to set current data to display our current set time
		call	dct			;call display sub routine.
		goto	disp2

;******************************************
;** Data Selection to display our time ****

;Display Current Time.
dsct		bsf	fadetime, 0x00
		movf	d4, 0x00		;get our display info ready
		movwf	cd4
		movf	d3, 0x00
		movwf	cd3
		movf	d2, 0x00
		movwf	cd2
		movf	d1, 0x00
		movwf	cd1
		return

;Display Old Time.
dsot		bcf	fadetime, 0x00
		movf	do4, 0x00		;get our display info ready
		movwf	cd4
		movf	do3, 0x00
		movwf	cd3
		movf	do2, 0x00
		movwf	cd2
		movf	do1, 0x00
		movwf	cd1
		return


;******************************************
;******************************************
;display whats in the cdX REGs

;From left to right, 4 - 1
;Digit 4
dct		movf	cd4, 0x00
		movwf	portb			;get our info ready for display
		movf	cd4, 0x00
		sublw	d'1'			;check to see if it is a 1 or 2, if it isn't then we don't need to display anything (what nullc1 is for).
		btfsc	status, z
		goto	nullc2
		btfss	calDsp, 0x00		;If in calibration mode then display what ever number is in cd4.
		goto	nullc1
nullc2		movlw	b'00000001'
		movwf	porta			;enable then disable our anode output (you can invert these valuse for a common cathode drive.)
nullc1		call	dwait2			;the time on dwait2 can set the brightness.
		movlw	b'00000000'
		movwf	porta
		call	dwait
;Digit 3
		movf	cd3, 0x00
		movwf	portb			;get our info ready for display
		movlw	b'00000010'
		movwf	porta			;enable then disable our anode output (you can invert these valuse for a common cathode drive.)
		call	dwait2
		movlw	b'00000000'
		movwf	porta
		call	dwait
;Digit 2
		movf	cd2, 0x00
		movwf	portb			;get our info ready for display
		movlw	b'00000100'
		movwf	porta			;enable then disable our anode output (you can invert these valuse for a common cathode drive.)
		call	dwait2
		movlw	b'00000000'
		movwf	porta
		call	dwait
;Digit 1
		movf	cd1, 0x00
		movwf	portb			;get our info ready for display
		movlw	b'00001000'
		movwf	porta			;enable then disable our anode output (you can invert these valuse for a common cathode drive.)
		call	dwait2
		movlw	b'00000000'
		movwf	porta
		call	dwait
		return

;Screen Saver *****************************
;Cycles through all numbers on tubes to
;help prevent cathode poisoning.
;******************************************
scrsav	bsf	scsv, 0x00
	bsf	calDsp, 0x00			;Set this to display all digits.
	movf	second,	0x00			;Get seconds data to display on all digits.
	movwf	cd1
	movwf	cd2
	movwf	cd3
	movwf	cd4
	call	dct				;Cycle through display.
	call	dct				;Cycle through display.
	movf	second, 0x00
	sublw	d'9'
	btfsc	status,	c			;If seconds is greater than 9 exit this sub.
	goto	scrsav
	bcf	calDsp, 0x00			;go back to normal display mode.
	return

;******************************************
;******************************************
;******************************************
;******************************************
;*************** IRQ sub ******************


count		btfsc	pir1, 0x00
		goto	timeKeep
		btfsc	pir1, 0x01
		goto	calOput
		clrf	pir1		    ;do nothing on stray IRQs
		goto	irqend

;**** Calibration pulse output from clock crystal. *******************

		bcf	pir1, 0x01
		goto	irqend


;******** time keeping for seconds ********
timeKeep	bcf	pir1, 0x00
;		goto	dtest			;uncomment to test the display counting
		incf	second, 0x01
		movf	second, 0x00
		sublw	d'60'			;if the seconds are at 60 then we need to rollover to 0 and increase minute's first digit "d1"
		btfss	status, z
		goto	irqend			;if we aren't at 60 then we just end our IRQ sub.

dtest		bcf		tcirqreg, 0x00	;clear for IRQ, set for Setting the time!
		call	tcm
		goto	irqend

;********************************************
; The end of our IRQ, prepars the
; configuration of the micro for the
; next IRQ.

irqend		movf	sst, 0x00		;we need to get our W and Status REGs back!
		movwf	status
		movf	sw, 0x00
		retfie

;********************************************
;********** time calculator *****************
;******** time keeping for minutes **********
tcm		call	dtachk			;Verify our time data with a copy, reset the time to 12:00 if they aren't the same.
		movlw	d'1'
		movwf	udstat1			;move 1 into udstat1
		movlw	d'0'			;reset our seconds to 0 "our rollover"
		movwf	second
		incf	d1, 0x01		;increase d1 by 1
		movf	d1, 0x00
		movwf	D1chk
		sublw	d'10'			;check to see if d1 is a 10 "we can only count to 9 but we want it to roll over only after 9!"
		btfss	status, z
		return			;if d1 is not a 10 then we end the sub.

		movlw	d'0'			;set d1 to 0
		movwf	d1
		movwf	D1chk
		incf	d2, 0x01		;increase d2 1 time
		movf	d2, 0x00
		movwf	D2chk
		sublw	d'6'			;check to see if d2 is a 6 "we can only count to 5 but we want it to roll over only after 5!"
		btfss	status, z
		return
		movlw	d'0'		;reset d2 to 0, d1 should already be 0
		movwf	d2
		movwf	D2chk
		btfsc	tcirqreg, 0x00	;if we are setting the time then we shouldn't increase the hours every time we rollover the minutes.
		return

;********************************************
;******** time keeping for hours ***********
tch		nop
ttest		movf	d4, 0x00
		sublw	d'1'			;check to see if d4 is a 1 "for hours that are greater than 9"
		btfss	status, z
		goto	tlow			;if our hour is 9 or less then goto tlow

		incf	d3, 0x01	;increase d3 by 1
		movf	d3, 0x00
		movwf	D3chk
		sublw	d'3'		;check to see if d3 is a 3, we can only count to 2 here.
		btfss	status, z
		return				;end the sub if it isin't a 3

		movlw	d'0'
		movwf	d3			;reset d3 to 0 if it is a 3 "13:xx"
		movwf	D3chk
		incf	d4, 0x01	;increase d4 by 1
		movf	d4, 0x00
		movwf	D4chk
		sublw	d'2'		;check to see if d4 is a 2 "this part of the sub is included to make it easyer to impliment 12/24 switching in the future"
		btfss	status, z
		return
		movlw	d'1'
		movwf	d3
		movwf	D3chk
		movlw	d'0'
		movwf	d4
		movwf	D4chk
		btfsc	tcirqreg, 0x00	;if we are setting the time then we shouldn't reset the time every time we rollover the hours.
		return
		movlw	d'0'		;reset our time to "01:00"
		movwf	d1
		movwf	D1chk
		movlw	d'0'
		movwf	d2
		movwf	D2chk
		return

;********************************************
;************ if hours is 09:00 or less ********
tlow		incf	d3, 0x01	;increase d3 by 1
		movf	d3, 0x00
		movwf	D3chk
		sublw	d'10'		;check to see if d3 is now 10, we can only count to 9 if d4 is 0, if d4 is 1 then we can only coun to 2! "ttest" checks for this first!
		btfss	status, z
		return
		movlw	d'0'
		movwf	d3			;if d3 is a 10 then we reset it to 0 and increase d4 to a 1.
		movwf	D3chk
		incf	d4, 0x01
		movf	d4, 0x00
		movwf	D4chk
		return

;********************************************
;***********************************************
;***********************************************
;Move our new data from "d1 - d4" to "do1 -do4"
;also reset the time keeping IRQ
;this sub is used for when we are
;setting the time.
;this sub is called on the release
;of a button.

disp0		movf	d1, 0x00
		movwf	do1
		movf	d2, 0x00
		movwf	do2
		movf	d3, 0x00
		movwf	do3
		movf	d4, 0x00
		movwf	do4
		movf	Ltmr1l, 0x00
		movwf	tmr1l
		movf	Ltmr1h, 0x00
		movwf	tmr1h
		movlw	b'11000000'
		movwf	intcon			;reenable the IRQ
		goto	disp			;go back to our main routine.


;*****************************************
ddrset		movlw	d'120'
		movwf	dwtcs			;set var "dwtcs" to 120
		movlw	d'1'
		movwf	dwtcf			;set var "dwtcf" to 1
		return

;*****************************************
;Display timing, we do NOT keep our
;time here, thats what IRQs are for.

dwait		movlw	d'3'
		movwf	dwt1
dmt1		decfsz	dwt1
		goto	dmt1
		return

dwait2		btfss	fadetime, 0x00	;1 is for current time
		goto	dtm1
		call	dwaitf
		return
dtm1		call	dwaits
		return

dwaitf		movf	dwtcf, 0x00		;Old Time
		movwf	dwtf
dmtf		decfsz	dwtf
		goto	dmtf
		return

dwaits		movf	dwtcs, 0x00		;New Time
		movwf	dwts
dmts		decfsz	dwts
		goto	dmts
		return

;******** Time corruption check *****************
dtachk		movf	D1chk,	0x00
		subwf	d1, 0x00
		btfss	status,z
		clrf	udstat2
		movf	D2chk,	0x00
		subwf	d2, 0x00
		btfss	status,z
		clrf	udstat2
		movf	D3chk,	0x00
		subwf	d3, 0x00
		btfss	status,z
		clrf	udstat2
		movf	D4chk,	0x00
		subwf	d4, 0x00
		btfss	status,z
		clrf	udstat2
tcrptchk	movf	udstat2, 0x00
		sublw	d'85'			;85 is just a number in this case, we want to see if our clock is a fresh start or has been reset.
		btfsc	status,z		;check to see if we had a soft or hard reset
		return				;we want to skip reseting the time if its just a soft reset of the micro.
		movlw	d'0'			;load our "d1 - d4" vars with 12:00 if it is a hard reset
		movwf	d1
		movwf	D1chk
		movlw	d'0'
		movwf	d2
		movwf	D2chk
		movlw	d'2'
		movwf	d3
		movwf	D3chk
		movlw	d'1'
		movwf	d4
		movwf	D4chk
		call	uod
		movlw	d'85'
		movwf	udstat2			;load the number 85 into udstat2, used to tell if we are on a fresh start or a reset.
		clrf	nset			;Blink our numbers
		return


;************ Clock calibration update ************
;Get calibration data form EEPROM
clkupdt		bsf status, rp0		;goto bank1
		movlw	0x00
		movwf	eeadr
		bsf	eecon1, 0x00
		movf	eedata, 0x00
		bcf	status, rp0	;goto bank 0
		movwf	Ltmr1l		;clock timeing adjust, decrease to make clock slower. our timer counts up!
		movwf	Ltmr1lHIGH
		movwf	tmr1l		;Default is 254, adjusting by 1 is about 4 minutes per 3 months.
		movlw	d'127'		;clock timeing adjust, default is 127
		movwf	Ltmr1h
		movwf	tmr1h
		movf	Ltmr1lHIGH, 0x00
		sublw	0xFF
		btfss	status, z
		incf	Ltmr1lHIGH, 0x01

		bsf status, rp0		;goto bank1
		movlw	0x01
		movwf	eeadr
		bsf	eecon1, 0x00
		movf	eedata, 0x00
		bcf	status, rp0	;goto bank 0
		movwf	dithcal		;Dither between two different calibration sets to increse accuracy.
		return

;*******************************
;**** Device Configuration *****
;you may need to comment this
;out for MPLab to compile.
;		org		0x2007
;		data	0x2174

;*******************************

END
