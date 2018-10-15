;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  Serial.inc                                ;
;                                  Homework 7                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 
;  This file contains the definitions for the Serial.asm
;
; Revision History:
;    11/16/2016    Sunghoon Choi    Created
;    11/18/2016    Sunghoon Choi    Documentation Updated


;Serial Definitions

;Addresses
SERIAL_BASE EQU     100H                   ;Base address of Serial

LCR_ADDR    EQU     SERIAL_BASE+03H        ;Address of Line Control Register
IER_ADDR    EQU     SERIAL_BASE+01H        ;Address of Interrupt Enable Register
IIR_ADDR    EQU     SERIAL_BASE+02H        ;Address of Interrupt Identification Register
LSR_ADDR    EQU     SERIAL_BASE+05H        ;Address of Line Status Register
THR_ADDR    EQU     SERIAL_BASE+00H        ;Address of Transmitter Holding Register
RBR_ADDR    EQU     SERIAL_BASE+00H        ;Address of Receiver Buffer Register
MSR_ADDR    EQU     SERIAL_BASE+06H        ;Address of Modem Status Register
DIV_LATCH_ADDR  EQU SERIAL_BASE+00H        ;Address of Divisor Latch


;Values
NO_INTERRUPT    EQU 00000001B   ;No Interrupt indicated by IIR    

LCR_VAL     EQU     00000011B   ;The value to be written to Line Control Register.
                   ;0------- DLAB bit off. Access RBR, THR
                   ;-0------ Break condition disbaled
                   ;--0----- Parity Off
                   ;---0---- Parity Off
                   ;----0--- Parity Off
                   ;-----0-- One Stop bit
                   ;------11 8 Bits Word length

IER_VAL     EQU     00001111B   ;The value to be written to Interrupt Enable Register.
                   ;0000---- Bits4-7 of IER are ALWAYS cleared
                   ;----1--- Modem Status Interrupt Enabled
                   ;-----1-- Receiver Line Status Interrupt Enabled
                   ;------1- THRE interrupt enabled
                   ;-------1 Received Data Availalbe Interrupt enabled

INT2_ICR_VAL   EQU  0000000000000001B ;The value to be written to I2CON Register.
                   ;00000000000-----  These are reserved bits.
                   ;-----------0----  Edge trigger
                   ;------------0---  enables the INT2 Interrupt
                   ;-------------001  sets INT2 priority to 1.
                             

DISABLE_PARITY      EQU 0         ;Argument of SetParity for disabling parity
EVEN_PARITY         EQU 2         ;Argument of SetParity for enabling even parity(not used)
ODD_PARITY          EQU 4         ;Argument of SetParity for enabling odd parity(not used)                        

KICKSTART_OFF       EQU 0         ;Turn off kickstart flag
KICKSTART_ON        EQU 1         ;Turn on kickstart flag

;Masks
ENABLE_DLAB_MASK    EQU 10000000B ;Mask used to set DLAB bit

DISABLE_THRE_MASK   EQU 11111101B ;Mask used to disable THRE interrupt

ERROR_MASK          EQU 00011110B ;Mask used to extract error 

ENABLE_PARITY_MASK  EQU 00001000B ;Mask used to enable parity bit.
EVEN_PARITY_MASK    EQU 00010000B ;Mask used to enable even parity 
TRANSMIT_CLR_MASK   EQU 00111000B ;Mask used to transmit the parity and check as cleared.
TRANSMIT_SET_MASK   EQU 11101111B ;Mask used to transmit the parity and check as set.
                                  ;Must be used in pair with TRANSMIT_CLR_MASK.
BREAK_CTRL_MASK     EQU 01000000B ;Mask used to force a break condition.
                                  
;Table Indices
BAUD_RATE_INDEX     EQU 1         ;Index used to obtain proper divisor value for 
                                  ;the desired baud rate.

;Interrupt Definitions

;Vectors                            
Int2Vec             EQU 14        ;Interrupt vector for INT2

;Addresses

PERIPHERAL_BASE EQU     0FF00H                ;Base Address of PCB
INT2_ICR_ADDR   EQU     PERIPHERAL_BASE+3CH   ;Address of INT2 Interrupt Control Register                                
INTCtrlrEOI     EQU     PERIPHERAL_BASE+22H   ;Address of End_of_Interrupt Register
        
;Values  
SerialEOI       EQU 000EH                     ;EOI to clear the In-Service bit for INT2