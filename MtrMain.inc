;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MtrMain.inc                                ;
;                                 Homework 10                                ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 
;  This file contains the definitions for the constants of MtrMain.asm
;
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;     12/02/2016    Sunghoon Choi      Initial Compilation
;     12/02/2016    Sunghoon Choi      Updated documentation   

MOTOR_TX_BUFFER_LEN     EQU      9  ;The length of MotorTxBuffer
STATUS_BUFFER_LEN       EQU      8  ;The length of StatusBuffer

LASER_CHAR          EQU     'L'
SPEED_CHAR          EQU     'S'
DIRECTION_CHAR      EQU     'D'