;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  INTFunc.inc                               ;
;                         Interrupt Controller Constants                     ;
;                           Glen George, Sunghoon Choi                       ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 
;   This file contains the definitions for general interrupts and the interrupt
;   controller for the Microprocessor-Based Clock. 
;
; Revision History:
;   12/10/2007  Glen George      initial revision
;   10/29/2016  Sunghoon Choi    Deleted unused constants.     



; Interrupt Controller

; Addresses
INTCtrlrCtrl    EQU     0FF32H          ;address of interrupt controller for timer
INTCtrlrEOI     EQU     0FF22H          ;address of interrupt controller EOI register

; Register Values
INTCtrlrCVal    EQU     00001H          ;set priority for timers to 1 and enable
                                        ;000000000000----  reserved
                                        ;------------0---  enable timer interrupt
                                        ;-------------001  timer priority

TimerEOI        EQU     00008H          ;Timer EOI command (all timers same)
NonSpecEOI      EQU     08000H          ;Non-specific EOI command



; General Interrupt Definitions

FIRST_RESERVED_VEC      EQU     1       ;reserve vectors 1-3
LAST_RESERVED_VEC       EQU     3
NUM_IRQ_VECTORS         EQU     32      ;number of interrupt vectors
