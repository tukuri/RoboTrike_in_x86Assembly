;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  InitPB.inc                                ;
;                                  Homework 6                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: 
;  This file contains the definitions for the InitPB.asm
;
; Revision History:
;    11/6/2016    Sunghoon Choi    Created
;    11/11/2016   Sunghoon Choi    Updated documentation(comments) for constants.

PARALLEL_B_CTRL     EQU 183H        ;The address of control word for parallel port B
PARALLEL_B_VAL      EQU 10000000B   ;1------- MODE SET FLAG
                                    ;-00----- MODE 0
                                    ;---0---- Port A Output
                                    ;----0--- Port C Output
                                    ;-----0-- MODE 0
                                    ;------0- Port B Output (Important)
                                    ;-------0 Port C Output
                                    
