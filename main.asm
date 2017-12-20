; Nixie clock code   - Ryan Brooks / ryan@hack.net
; see www.hack.net/nixie

; This is all written in very simple, linear 6502 assembly.   It's designed to be very readable, and 
; is therefore inefficient, but who cares?   There's lots of kids now who don't know 6502 assembly!

; Timekeeping ram is at $0000
; EPROM (code below) is at $E000
; 6522 at $2000


; To Do: Should add a conditional clock_init if the DS isn't ticking.

; locations 

porta		=	$2000
portaddr	=	$2002
portb		=	$2001
portbddr	=	$2003		; <- Note that these are reversed; made wiring a bit easier.

year		= 	$7FF
month		=	$7FE
date		=	$7FD
day		=	$7FC
hour		=	$7FB
minutes		=	$7FA
seconds		=	$7F9
century		=	$7F8

hourten		=	$10
hourone		=	$11
minuteten	=	$12
minuteone	=	$13
secondten	=	$14
secondone	=	$15

counter			=	$20
temp		=	$21
mask		=	$22

; constants

long		=	$20			; long delay loops, used for slot timer

; RESET -- point reset vector here

		.org		$e000

boot		SEI					;disable interrupts
		CLD					;get out of decimal mode

		LDX		#$ff			;init stack
		TXS
		
		lda		#$ff
		sta		portaddr	;set all pins to putput
		lda		#$00
		sta		portbddr	;set all pins to input 

		lda		#$01		
		sta		mask		; for bit masks

;		jsr		clockinit	; NEW - commented out to allow clock to retain time when power off
						; *** may have to leave this in to initialize DS chip first time.


;MAIN LOOP

mainloop	jsr		copyclk
		jsr		shiftout	

		ldx		#%00000100			;output latch each nybble
		stx		porta
		nop
		nop
		nop
		nop
		nop
		nop
		ldx		#$00
		stx		porta	

		jsr		longdelay


		lda		portb		; Check switches - msb000EDCBAlsb, A=h+, B=h-, C=m+, D=m-, E=zs
		and		#%00000001
		cmp		#%00000000	; switches are active LOW now - this is a change
		bne		nobut1	
		jsr		longdelay
		jsr		longdelay
		jsr		longdelay	; slow things down when adding hours
		jsr		addhour
nobut1	lda		portb
		and		#%00000010
		cmp		#%00000000
		bne		nobut2
		jsr		longdelay
		jsr		longdelay
		jsr		longdelay
		jsr		subhour
nobut2	lda		portb
		and		#%00000100
		cmp		#%00000000
		bne		nobut3					
		jsr		longdelay
		jsr		longdelay
		jsr		addmin
nobut3	lda		portb
		and		#%00001000
		cmp		#%00000000
		bne		nobut4					
		jsr		longdelay
		jsr		longdelay
		jsr		submin
nobut4	lda		portb
		and		#%00010000
		cmp		#%00000000
		bne		nobuts					
		jsr		longdelay
		jsr		zerosec


nobuts	lda		$minuteone
		cmp		#$09			; let's do the rolls at xx:x9:30
                bne             jmainloop
		lda		$secondten
		cmp		#$3
                bne             jmainloop
		lda		$secondone
		cmp		#0
                bne             jmainloop
		jsr		rotclk			; lets not do them right now NEW


		lda		hourten
		cmp		#$00
                beq             jmainloop                ; hours digit isn't 1 or 2

		; put in other code here for flip flop 12/24

jmainloop        jmp             mainloop

		

; SUBROUTINES

; delay.  delays ffxsomethings

delay		pha
		txa
		pha
		ldx		#$00
d_loop		inx
		cpx		#$ff
		bne		d_loop
		pla
		tax
		pla
		rts

; longer delay. delays long

longdelay	pha
		txa
		pha
		ldx		#$00
ld_loop		jsr		delay
		inx	
		cpx		#long
		bne		ld_loop
		pla
		tax
		pla
		rts

; clockinit - no parameters.  Sets clock up

clockinit	pha
		lda	#%10000000
		sta	$7F8
	
		lda	#$5
		sta	$7ff				;2005 epoch
	
		lda	#%00010000			; 10th month - BCD of course
		sta	$7fe

		lda	#%00010011			; 13th day 
		sta	$7fd

		lda	#$5				; 5th day of the week
		sta	$7fc

		lda	#%00010111
		sta	$7fb				; 17:00
	
		lda	#$00
		sta	$7fa
		sta	$7f9				; 00 min, 00 sec, osc on!

		lda	#%10100001
		sta	$7f8				; 21st century, write active

		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		
		pla
		rts



; copyclk - no parameters.  loads zero page with time.

copyclk		pha
		lda		#%001100001		; Read please
		sta		$7F8			; tell clock to stop updates
		jsr		delay
		jsr 		delay
		jsr		delay
		jsr		delay
		jsr		delay			; wait for registers to become ready, really.
		lda		hour
		and		#%00110000
		clc
		ror		a				; ror's didn't work !!!!
		ror		a				; "ROR" in TASM assembles to ROR $0... this is an error
		ror		a
		ror		a
		sta		hourten
		lda		hour
		and		#%00001111
		sta		hourone

		lda		minutes
		and		#%01110000
		clc
		ror		a
		ror		a	
		ror		a
		ror		a
		sta		minuteten
		lda		minutes
		and		#%00001111
		sta		minuteone

		lda		seconds
;		sta		portb			;debug NEW <<<<  check port B if run
		and		#%01110000
		clc
		ror		a
		ror		a
		ror		a
		ror		a
		sta		secondten
		lda		seconds
		and		#%00001111
                sta             secondone
		lda		#%00100001
		sta		$7F8			;tell clock to resume updates
		jsr		delay
		pla
		rts

; rotclk. no parameters.   slotmachines the nixie to prevent poisoning.

rotclk		pha
		txa
		pha
		ldx		#$00
rotloop		stx		secondone
		stx		secondten
		stx		minuteone
		stx		minuteten
		stx		hourone
		stx		hourten
		jsr		shiftout
		lda		#%00000100	;output latch each nybble
		sta		porta

		jsr		longdelay
		jsr		longdelay
		jsr		longdelay
		jsr		longdelay

		inx	
		cpx		#$0b
		bne		rotloop
		pla
		tax
		pla
		rts

; shiftdig. put bcd digit in A, shifts out string into nixies.
; port a bits
; bit 0 - clk
; bit 1 - data
; bit 2 - latch

; note: currently bangs all other bits low.

shiftdig	sta		temp
		txa
		pha
		tya
		pha	
		
		
		lda		temp

		ldy		#$00
sd_loop		ldx		#$00
		lda		temp
		stx		porta
		and		#$08
		cmp		#$08
		beq		sd_notzero
		ldx		#%00000000
		stx		porta
		nop
		nop
		nop
		nop
		nop
		nop
		ldx		#%00000001
		stx		porta
		nop
		nop
		nop
		nop
		nop
		nop
		jmp		sd_ror
sd_notzero	ldx		#%00000010
		stx		porta
		nop
		nop
		nop
		nop
		nop
		nop
		ldx		#%00000011
		stx		porta
		nop
		nop
		nop
		nop
		nop
		nop
sd_ror		rol		$temp
		iny
		cpy		#$04
		bne		sd_loop

		pla
		tay
		pla
		tax
		lda		temp

		rts

; shiftout.  Shift out all the digits.  No parameters

shiftout	lda		secondone			; shift, hours leftmost, last.
		jsr		shiftdig
		lda		secondten
		jsr		shiftdig
		lda		minuteone
		jsr		shiftdig
		lda		minuteten
		jsr		shiftdig
		lda		hourone
		jsr		shiftdig
		lda		hourten
		jsr		shiftdig
		rts

; addhour - adds an hour to realtime clock

addhour		pha
		lda	hour
		and 	#%00111111
		cmp	#%00100011	
		bne	ah_not23
		lda	#%10000000			; Change 23h to 00h
		sta	$7f8
		lda	#$00
		sta	hour
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

ah_not23	lda	#%10000000			; BCD add to hour register
		sta	$7f8
		sed
		lda	hour
		adc	#$01
		cld
		sta	hour
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

; addmin - adds a minute to realtime clock

addmin	pha
		lda	minutes
		and 	#%01111111
		cmp	#%01011001			; 59?	
		bne	am_not59
		lda	#%10000000			; Change 59m to 00m
		sta	$7f8
		lda	#$00
		sta	minutes
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

am_not59	lda	#%10000000			; BCD add to Minutes register
		sta	$7f8
		sed
		lda	minutes
		adc	#$01
		cld
		sta	minutes
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts



zerosec	pha
		lda		#%10000000
		sta		$7f8
		lda		#$00
                sta             seconds
		sta		$7f8
		pla
		rts

; SUBTRACTION ROUTINES

; subhour - subtracts an hour from realtime clock

subhour	pha
		lda	hour
		and 	#%00111111
		cmp	#%00000000
		bne	sh_not0
		lda	#%10000000			; Change  00h to 23h
		sta	$7f8
		lda	#%00100011			; 23
		sta	hour
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

sh_not0	lda	#%10000000			; BCD sub from hour register
		sta	$7f8
		sed
		lda	hour
		sbc	#$01
		cld
		sta	hour
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

; submin - subtracts a minute from realtime clock

submin	pha
		lda	minutes
		and 	#%01111111
		cmp	#%00000000			; 0?	
		bne	sm_not0
		lda	#%10000000			; Change 00m to 59m
		sta	$7f8
		lda	#%01011001			; 59
		sta	minutes
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

sm_not0	lda	#%10000000			; BCD sub from Minutes register
		sta	$7f8
		sed
		lda	minutes
		sbc	#$01
		cld
		sta	minutes
		lda	#%00100001			; still 21st century, end write
		sta	$7f8
		pla
		rts

reset		.org		$FFFC

		.byte		$00,$e0		; boot to start in rom at $e000, little endian- of course.
		
		.END
