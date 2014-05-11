; SGPIOインターフェース
; 2014/03/20 Y.Sunazuka
;
; Specification
;  GP0 - OUT
;  GP1 - OUT
;  GP2 - OUT
;  GP3 - IN - SCLOCK
;  GP4 - IN - SLOAD
;  GP5 - IN - SDOUT
;
; ※本ソースのアセンブラはgputils推奨
;


;			processor	16F1827
			#include	p16f1827.inc


			__CONFIG		_FOSC_INTOSC & _WDTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
;;			__CONFIG		_FOSC_INTOSC & _WDTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_ON & _CLKOUTEN_OFF & _IESO_OFF & _FCMEN_OFF
			org	_CONFIG2
			DW		_WRT_OFF & _PLLEN_ON & _STVREN_OFF & _BORV_LO & _LVP_OFF
;;			DW		_WRT_OFF & _PLLEN_OFF & _STVREN_OFF & _BORV_LO & _LVP_OFF

;			#include		pic16macro.inc
movr	macro	fr,lit
		movlw	lit
		movwf	fr
		endm

djnz		macro	fr,addr
		decfsz	fr,1
		goto	addr
		endm




		

; Output Port 
#define		BDStatB		RB1				; Board Status
#define		Ch1ActB		RB2				; Ch.1 Activity
#define		Ch1LocB		RB3				; Ch.1 Locator
#define		Ch1FaiB		RB4				; Ch.1 Fail
#define		Ch2ActB		RB5				; Ch.2 Activity
#define		Ch2LocB		RB6				; Ch.2 Locator
#define		Ch2FaiB		RB7				; Ch.2 Fail
#define		Ch3ActB		RA6				; Ch.3 Activity
#define		Ch3LocB		RA7				; Ch.3 Locator
#define		Ch3FaiB		RA0				; Ch.3 Fail
#define		Ch4ActB		RA1				; Ch.4 Activity
#define		Ch4LocB		RA2				; Ch.4 Locator
#define		Ch4FaiB		RA3				; Ch.4 Fail

#define		BDStatP		LATB
#define		Ch1ActP		LATB			; Ch.1 Activity
#define		Ch1LocP		LATB			; Ch.1 Locator
#define		Ch1FaiP		LATB			; Ch.1 Fail
#define		Ch2ActP		LATB			; Ch.2 Activity
#define		Ch2LocP		LATB			; Ch.2 Locator
#define		Ch2FaiP		LATB			; Ch.2 Fail
#define		Ch3ActP		LATA			; Ch.3 Activity
#define		Ch3LocP		LATA			; Ch.3 Locator
#define		Ch3FaiP		LATA			; Ch.3 Fail
#define		Ch4ActP		LATA			; Ch.4 Activity
#define		Ch4LocP		LATA			; Ch.4 Locator
#define		Ch4FaiP		LATA			; Ch.4 Fail


#define		BDStat		BDStatP,BDStatB			; Board Status
#define		Ch1Act		Ch1ActP,Ch1ActB			; Ch.1 Activity
#define		Ch1Loc		Ch1LocP,Ch1LocB			; Ch.1 Locator
#define		Ch1Fai		Ch1FaiP,Ch1FaiB			; Ch.1 Fail
#define		Ch2Act		Ch2ActP,Ch2ActB			; Ch 2 Activity
#define		Ch2Loc		Ch2LocP,Ch2LocB			; Ch.2 Locator
#define		Ch2Fai		Ch2FaiP,Ch2FaiB			; Ch.2 Fail
#define		Ch3Act		Ch3ActP,Ch3ActB			; Ch.3 Activity
#define		Ch3Loc		Ch3LocP,Ch3LocB			; Ch.3 Locator
#define		Ch3Fai		Ch3FaiP,Ch3FaiB			; Ch.3 Fail
#define		Ch4Act		Ch4ActP,Ch4ActB			; Ch.4 Activity
#define		Ch4Loc		Ch4LocP,Ch4LocB			; Ch.4 Locator
#define		Ch4Fai		Ch4FaiP,Ch4FaiB			; Ch.4 Fail
	

; Input Port
#define		P_SDOUT		PORTA
#define		B_SDOUT		RA4
#define		P_SLOAD		PORTA
#define		B_SLOAD		RA5
#define		SDOUT		P_SDOUT,B_SDOUT		; SGPIO.SDOUT
#define		SLOAD		P_SLOAD,B_SLOAD		; SGPIO.SLOAD
#define		SCLOCK		PORTB,RB0			; SGPIO.SCLOCK (freq = 100KHz)
;							  76543210
#define		GPIODIR_PORTA	B'00110000' ;  RA7,6,3,2,1,0 .. output
#define		GPIODIR_PORTB	B'00000001' ;  RB7,6,5,4,3,2,1 .. output

; RAM Assignment (0x20 - 0x7F)
#define		cnt			0x20			; for wait counter
#define		cnt2		0x21			; for wait counter
#define		cnt3		0x22			; for wait counter
#define		work		0x23			;
#define		mode		0x24			; 0..demo/1..activity
#define		sdcnt		0x25			; counter for SDOUT
#define		lcnt0		0x28			; 50KHz counter
#define		lcnt1		0x29			; 195.3125Hz counter
											; 00000000
											; |||||||||
											; |||||||+--	195Hz
											; ||||||+---	97.5Hz
											; |||||+----	48.75Hz
											; ||||+-----	24.375Hz
											; |||+------	12.1875Hz
											; ||+-------	6.09375Hz
											; |+--------	3.046875Hz
											; +---------	1.5234375Hz
#define		lcnt2		0x2a			; 0.76299453Hz counter
#define		lcnt3		0x2b			; 0.002980232Hz counter
	   

#define		inttogo		0x2f

#define		sdoutH		0x70			; SDOUT shift register H
#define		sdoutL		0x71			; SDOUT shift register L
#define		sdNewH		0x72			; SDOUT
#define		sdNewL		0x73			; SDOUT


;Reference from SFF-8489
#define		status0		0x40				; 0..off/1..Drive  Present/2..Drive Present,Activity/3..Locate(Identify)
										; 4..Fail/5..Rebvild/6..Rebuild/7..PFA/8..Hotspare/9..In A Critical Array/10..In A Failed Array/11..undefined
#define		status1		0x41
#define		status2		0x42
#define		status3		0x42
										; ODn.0/ODn.1/ODn.2
#define		sdout0		0x43			; 000..Noactivity/100..activity/x00..No Fail, Locate or Rebuild/x10...Locate(Identify)/x01...Fail/x11..Rebuild
#define		sdout1		0x44
#define		sdout2		0x45
#define		sdout3		0x46



;
;
;
;
;
;
;
			org		0
			goto	start
			goto	start
			goto	start
			goto	start
   			org		4
			goto	interrupt
;			goto	loops

loops:
			sleep
			goto	loops

;; メインプログラム

start
			BANKSEL	INTCON
			clrf	INTCON 		; 割り込み禁止

			BANKSEL   OSCCON
; Clock Setting
;			movr	OSCCON,B'01011000'		; 1MHz/Internal
;			movr	OSCCON,B'01101000'		; 4MHz/Internal
;			movr	OSCCON,B'01110000'		; 8MHz/Internal
;			movr	OSCCON,B'01111000'		; 16MHz/Internal
			movr	OSCCON,B'11110000'		; 32MHz/Internal(8MHz 4xPLL)

; Port Setting
			BANKSEL	OPTION_REG
			movr	OPTION_REG,B'10001000'	; WPUEN off/INTEDG fall/T0CS Internal/T0SE lo-to-high/PSA 1:2
;;			movr	OPTION_REG,B'00001000'	; WPUEN on/INTEDG fall/T0CS Internal/T0SE lo-to-high/PSA 1:2
;;			movlw	B'10001000'	; WPUEN off/INTEDG fall/T0CS Internal/T0SE lo-to-high/PSA 1:2
;;			OPTION

			BANKSEL	WPUA
			clrf	WPUA
			clrf	WPUB
			BANKSEL	PORTA
			clrf	PORTA
			clrf	PORTB
			BANKSEL	LATA
			clrf	LATA
			clrf	LATB
			BANKSEL	ANSELA
			clrf	ANSELA					;digital I/O
			clrf	ANSELB					;digital I/O
			BANKSEL	TRISA
			movr	TRISA,GPIODIR_PORTA		; RA4,5..Input/RA7,6,3,2,1,0..Output
			movr	TRISB,GPIODIR_PORTB		; RB0..Input/RB7,6,5,4,3,2,1..Output

			BANKSEL	PORTA
; end of Chip Initialize.

; work memory initialize
			clrf	mode

			clrf	sdcnt
			clrf	sdoutH
			clrf	sdoutL
			clrf	sdNewH
			clrf	sdNewL

;			movlw	0x55
;			movwf	sdNewH
;			movlw	0xAA
;			movwf	sdNewL

			clrf	lcnt0
			clrf	lcnt1
			clrf	lcnt2
			clrf	lcnt3

			clrf	status0
			clrf	status1
			clrf	status2
			clrf	status3
; demo mode
			BANKSEL	LATA
			clrf	LATA		; 0
			clrf	LATB		; 0

;			BANKSEL	TRISA
;			movr	TRISA,GPIODIR_PORTA		; RA4,5..Input/RA7,6,3,2,1,0..Output
;			movr	TRISB,GPIODIR_PORTB		; RB0..Input/RB7,6,5,4,3,2,1..Output


demo_loop_s:
			BANKSEL	PORTA
			clrf	mode
			call	wait_65ms

			movr	work,0x10	;
demo_loop2
			BANKSEL	BDStatP
			bsf		BDStat 
			call	wait_20ms
			bcf		BDStat 
			call	wait_20ms
			BANKSEL	PORTA
			djnz	work,demo_loop2

			BANKSEL	PORTA
			movr	work,0x04	;
demo_loop
			BANKSEL		Ch1ActP
			bsf		Ch1Act
			call	wait_20ms
			bsf		Ch1Loc
			call	wait_20ms
			bsf		Ch1Fai
			call	wait_20ms
			bsf		Ch2Act
			call	wait_20ms
			bsf		Ch2Loc
			call	wait_20ms
			bsf		Ch2Fai
			call	wait_20ms
			BANKSEL	BDStatP
			bsf		BDStat 
			BANKSEL		Ch3ActP
			bsf		Ch3Act
			call	wait_20ms
			bsf		Ch3Loc
			call	wait_20ms
			bsf		Ch3Fai
			call	wait_20ms
			bsf		Ch4Act
			call	wait_20ms
			bsf		Ch4Loc
			call	wait_20ms
			bsf		Ch4Fai
			call	wait_20ms
			BANKSEL	BDStatP
			bcf		BDStat 

			BANKSEL		Ch1ActP
			bcf		Ch1Act
			call	wait_20ms
			bcf		Ch1Loc
			call	wait_20ms
			bcf		Ch1Fai
			call	wait_20ms
			bcf		Ch2Act
			call	wait_20ms
			bcf		Ch2Loc
			call	wait_20ms
			bcf		Ch2Fai
			call	wait_20ms
			BANKSEL	BDStatP
			bsf		BDStat 
			BANKSEL		Ch3ActP
			bcf		Ch3Act
			call	wait_20ms
			bcf		Ch3Loc
			call	wait_20ms
			bcf		Ch3Fai
			call	wait_20ms
			bcf		Ch4Act
			call	wait_20ms
			bcf		Ch4Loc
			call	wait_20ms
			bcf		Ch4Fai
			call	wait_20ms

			BANKSEL	BDStatP
			bcf		BDStat

			BANKSEL	PORTA
			djnz	work,demo_loop
;
;
;
			BANKSEL	INTCON
			movr	INTCON,B'10010000' 	; GIE on/INTE on/INTF clear

;			movlw	0x55
;			movwf	sdNewH
;			movlw	0x55
;			movwf	sdNewL

			BANKSEL	PORTA
			clrf	sdNewH
			clrf	sdNewL


signaldisp:
			BANKSEL	PORTA
;			BANKSEL	0
;			btfss	inttogo, 0
;			goto	signaldisp	;

			bcf		inttogo, 0

;;			movf	sdNewH, W			; W <- sdNewH
;;			iorwf	sdNewL, W			; W <- W or sdNewL
;;			btfsc	STATUS,Z			; 
;;			goto	nosignal			; IF Z == '1' THEN jump to no signal

ch1:
			BANKSEL	Ch1ActP
			btfsc	sdNewH, 3			; Ch.1 Activity
			bsf		Ch1Act				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewH, 3			;
			bcf		Ch1Act				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch1LocP
			btfsc	sdNewH, 2			; Ch.1 Activity
			bsf		Ch1Loc				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewH, 2			;
			bcf		Ch1Loc				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch1FaiP
			btfsc	sdNewH, 1			; Ch.1 Activity
			bsf		Ch1Fai				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewH, 1			;
			bcf		Ch1Fai				; IF Activity == '0' THEN Ch.1 Activity LED Off

ch2:
			BANKSEL	Ch2ActP
			btfsc	sdNewH, 0			; Ch.1 Activity
			bsf		Ch2Act				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewH, 0			;
			bcf		Ch2Act				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch2LocP
			btfsc	sdNewL, 7			; Ch.1 Activity
			bsf		Ch2Loc				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 7			;
			bcf		Ch2Loc				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch2FaiP
			btfsc	sdNewL, 6			; Ch.1 Activity
			bsf		Ch2Fai				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 6			;
			bcf		Ch2Fai				; IF Activity == '0' THEN Ch.1 Activity LED Off


ch3:
			BANKSEL	Ch3ActP
			btfsc	sdNewL, 5			; Ch.1 Activity
			bsf		Ch3Act				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 5			;
			bcf		Ch3Act				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch3LocP
			btfsc	sdNewL, 4			; Ch.1 Activity
			bsf		Ch3Loc				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 4			;
			bcf		Ch3Loc				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch3FaiP
			btfsc	sdNewL, 3			; Ch.1 Activity
			bsf		Ch3Fai				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 3			;
			bcf		Ch3Fai				; IF Activity == '0' THEN Ch.1 Activity LED Off

ch4:
#if		1
			BANKSEL	Ch4ActP
			btfsc	sdNewL, 2			; Ch.1 Activity
			bsf		Ch4Act				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 2			;
			bcf		Ch4Act				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch4LocP
			btfsc	sdNewL, 1			; Ch.1 Activity
			bsf		Ch4Loc				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 1			; Ch.1 Activity
			bcf		Ch4Loc				; IF Activity == '0' THEN Ch.1 Activity LED Off

;;			BANKSEL	Ch4FaiP
			btfsc	sdNewL, 0			; Ch.1 Activity
			bsf		Ch4Fai				; IF Activity == '1' THEN Ch.1 Activity LED On				
			btfss	sdNewL, 0			;
			bcf		Ch4Fai				; IF Activity == '0' THEN Ch.1 Activity LED Off
#endif

;;			goto	signaldisp

nosignal:
			BANKSEL	PORTA
			movf	lcnt0, W
			BANKSEL	BDStatP
			movwf	lcnt0
			btfsc	lcnt0, 0			;
			bsf		BDStat				; IF (lcnt3 & 8) == '1' THEN INT.Status LED On				
			btfss	lcnt0, 0			;
			bcf		BDStat				; IF (lcnt3 & 8) == '0' THEN INT.Status LED Off

			BANKSEL	PORTA
			goto	signaldisp


;
; Interupt Routine for RB0/INT(SCLOCK)
;
interrupt:
			BANKSEL	INTCON
			btfss	INTCON, INTF
			goto	intpass		; IF INTF = '0' goto intpass

; Read SDOUT bit
			BANKSEL	P_SDOUT
			lslf	sdoutL, F
			rlf		sdoutH, F
			btfsc	SDOUT			; 
			bsf		sdoutL, 0		; IF SDOUT = '1' THEN Set to 1

			incf	sdcnt, F		; number of sdout bit counter up

; Read SLOAD
			BANKSEL	P_SLOAD
			btfss	SLOAD			; Bit test, Skip if Set
			goto	continuous		; IF SLOAD is '0' GOTO continuous

		; sdout status update		
			movf	sdoutL, W		; W <- sdoutL
			movwf	sdNewL			; sdbitL <- W
			movf	sdoutH, W		; W <- sdoutH
			movwf	sdNewH			; sdbitH <- W

			clrf	sdcnt			; counter reset
			clrf	sdoutL			; clear work L
			clrf	sdoutH			; clear work H

;;;;			goto	intfinish

continuous:

intfinish:
;;			BANKSEL	INTCON
			bsf		inttogo, 0


; inclument system timer
			clrw				; Clear W
			bsf		STATUS, C	; Carry Set.
			addwfc	lcnt0, F	; lcnt0 = lcnt0 + W + C
			addwfc	lcnt1, F	; lcnt1 = lcnt1 + W + C
			addwfc	lcnt2, F	; lcnt2 = lcnt2 + W + C
			addwfc	lcnt3, F	; lcnt3 = lcnt3 + W + C

			BANKSEL	INTCON
			bcf		INTCON, INTF

intpass:

			BANKSEL	PORTA
			retfie				; interrupt return
;
;
;
;
; Looping Waits
;
wait_20us										; 18(us)
			movr	cnt,9						; 1(cycle)
loop_20us
			djnz	cnt,loop_20us				; 2(cycle) * 9 - 1(Cycle)
			return								; 2(cycle)
												; 3 + 17 = 20(cycle) = 80(clock) = 20us(@4MHz)


wait_1ms										; 998(us)
			movr	cnt3,8						; 1(cycle)
wait_1ms_sub									; 998(us)
			movr	cnt,100					; 1(cycle)
loop_1ms
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			nop									; 1(cycle)
			djnz	cnt,loop_1ms				; (8+2)(cycle)*100 - 1(cycle)
			djnz	cnt3,wait_1ms_sub			; (1002+2)(cycle)*8 - 1(cycle):8031(cycle):1003.875us:1.003ms:
			return								; 2(cycle)
												; 1 + 2 + 999 = 1002(cycle) = 4008(clock) = 1002us(@4MHz) 125us(@32MHz)*8 = 1ms

wait_5ms										; 5(ms)
			movr	cnt2,5
loop_5ms
			call	wait_1ms
			djnz	cnt2,loop_5ms
			return

wait_8ms										; 5(ms)
			movr	cnt2,8
loop_8ms
			call	wait_1ms
			djnz	cnt2,loop_8ms
			return


wait_10ms										; 10(ms)
			movr	cnt2,10
loop_10ms
			call	wait_1ms
			djnz	cnt2,loop_10ms
			return

wait_16ms										; 10(ms)
			movr	cnt2,16
loop_16ms
			call	wait_1ms
			djnz	cnt2,loop_16ms
			return

wait_20ms										; 20(ms)
			movr	cnt2,20
loop_20ms
			call	wait_1ms
			djnz	cnt2,loop_20ms
			return

wait_65ms:
			call	wait_20ms
			call	wait_20ms
			call	wait_10ms
;			call	wait_20ms
;			call	wait_5ms
			return

wait_250ms										; 0.25(s)
			movr	cnt2,250
loop_250ms
			call	wait_1ms
			djnz	cnt2,loop_250ms
			return

wait_1s											; 1.00(s)
			call	wait_250ms
			call	wait_250ms
			call	wait_250ms
			call	wait_250ms
			return

wait_05s										; 0.50(s)
			call	wait_250ms
			call	wait_250ms
			return

;;;;;;;;;;;;;;;;;;; おしまい ;;;;;;;;;;;;;;;;;;;;;;

; 内蔵EEPROM
			org		0x2100
			de	"SGPIO -SUN 2014 -"


		end

