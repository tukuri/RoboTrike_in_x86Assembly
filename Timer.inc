;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Timer.inc                                ;
;                             Timer related functions                        ;
;                            Glen Geroge, Sunghoon Choi                      ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: 
;  This file contains the definitions for the Timer.asm
;
; Revision History:
;    10/25/2016     Sunghoon Choi     Created
;    10/28/2016     Sunghoon Choi     Updated documentation(comments) for constants.
;    11/6/2016      Sunghoon Choi     Added timer1 related addresses and values.

; Timer Defintions

; Addresses
Tmr1Ctrl        EQU     0FF5EH          ;address of Timer 1 Control Register
Tmr1MaxCnt      EQU     0FF5AH          ;address of Timer 1 Max Count Register
Tmr1Count       EQU     0FF58H          ;address of Timer 2 Count Register

Tmr1CtrlVal     EQU     1110000000000001B



Tmr2Ctrl        EQU     0FF66H          ;address of Timer 2 Control Register
Tmr2MaxCnt      EQU     0FF62H          ;address of Timer 2 Max Count Register
Tmr2Count       EQU     0FF60H          ;address of Timer 2 Count Register

; Control Register Values
Tmr2CtrlVal     EQU     1110000000000001B ;Timer 2 Control Register value
                       ;1---------------  enable timer
                       ;-1--------------  write to control
                       ;--1-------------  enable interrupts
                       ;----000000-0000-  reserved
                       ;---0------0-----  read only
                       ;---------------1  continuous mode

; Interrupt Vectors
Tmr1Vec         EQU     18              ;interrupt vector for Timer 1
Tmr2Vec         EQU     19              ;interrupt vector for Timer 2


; Interrupt Controller

; Addresses
INTCtrlrCtrl    EQU     0FF32H         ;address of Timer interrupt Control Register

INTCtrlrEOI     EQU     0FF22H         ;address of End_of_Interrupt Register

; Register Values
INTCtrlrCVal    EQU     00001H         ;The value to be given to TCUCON Register.
                                       ;000000000000----  These are reserved bits.
                                       ;------------0---  enables the Timer Interrupt
                                       ;-------------001  sets timer priority to 1.
TimerEOI        EQU     00008H         ;Command for Timer EOI

; General Timing Definitions


KHZ_4_CNT       EQU     576            ;Counts for generating 4KHz timer interrupts.
                                       ;18.432 / 2 / 4 /4000
ONE_MS_CNT      EQU     2304           ;Counts for generating 1KHz timer interrupts.
                                       ;18.432/ 2 / 4 / 1000

