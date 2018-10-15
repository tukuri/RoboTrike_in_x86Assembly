NAME InitPB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   InitPB                                   ;
;                                  Homework 6                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;  This file contains the functions for initializing parallel port B of RoboTrike.
;
; Table of Contents:
;    InitParallelB      - Initialize parallel port B
;
; Revision History:
;     11/6/2016      Sunghoon Choi        Created
;     11/8/2016      Sunghoon Choi        Initial Compilation
;     11/11/2016     Sunghoon Choi        Updated documentation

$INCLUDE(InitPB.inc)    ;Include the.inc file which contains constatns for InitPB.asm

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        
InitParallelB   PROC    NEAR
                PUBLIC  InitParallelB
                
MOV DX, PARALLEL_B_CTRL   ;Get the address of control word for parallel port B.
MOV AL, PARALLEL_B_VAL    ;Get the configuration value for parallel port B.
OUT DX, AL                ;Configure parallel port B with the prepared settings.
    
RET                       ;End of InitParallelB procedure.

InitParallelB ENDP
        
CODE ENDS

END