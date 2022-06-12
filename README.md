A simple nixie tube clock with a few built in redundancies to keep from displaying the incorrect time. It also fades between numbers and has a calibration mode
that can be entered by bringing pin 3 low (remove power), holding M+H pins high, and then bringing pin 3 high again (apply power). Uses dithering for
crystal calibration adjustment (still don't remember if I ever got this exactly right, i might revisit this code again some day). It might work with LED digits,
but I've never tested this and it might not multiplex them at the right timing and there could be some flicker. I used a BCD to decimal chip for the output.

Pinout is as follows:

PIN 15 = minute set (active high)

PIN 16 = hour set (active high)

PIN 3 = power detect (active high)

PINS 6 - 9 = Multiplexed time BCD output.  (active high)

PINS 17,18,1,2 = Multipexed digit output in that order. From left to right.  (active high, I think...)

PIN 5 = Ground.

PIN 14 = 5V

PIN 4 = reset, pull up with resistor.

PINS 12,13 = 32.768khz crystal connection.

PIN 10 = clock pulse output for precision calibration (I don't think I ever got this working quite right).

PIN 11 = ??? spare?
