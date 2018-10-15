NAME    ChipSel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   ChipSel                                  ;
;                         Glen George, Sunghoon Choi                         ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;    This file contains the functions used to initialize the chip selects for
;    80188 based RoboTrike system.
;
; Table of Contents:
;    InitCS - Initialize the peripheral chip selects
;
; Revision History:
;    10/25/2016    Sunghoon Choi    Created
;    10/28/2016    Sunghoon Choi    Updated documentation


$INCLUDE (ChipSel.inc)  ;include the file which contains constants for 
                        ;ChipSel.asm

CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP



; InitCS
;
; Description:       Initialize the Peripheral Chip Selects on the 80188.
;
; Operation:         Write the initial values to the PACS and MPCS registers.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            None.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: AX, DX
; Stack Depth:       0 words
;
; Limitations:       None     
; Known bugs:        None 
; Special Notes:     None
; Author:            Glen George, Sunghoon Choi
;
; Revision History:  07/12/2010  Last modified by Glen Geroge
;                    10/28/2016  PCS and MPCS register variable's name 
;                                changed by Sunghoon Choi
InitCS  PROC    NEAR
        PUBLIC  InitCS


        MOV     DX, PCSCtrl     ;write to PACS register
        MOV     AX, PACSval
        OUT     DX, AL

        MOV     DX, MPCSctrl    ;write to MPCS register
        MOV     AX, MPCSval
        OUT     DX, AL


        RET                     ;done so return


InitCS  ENDP

CODE ENDS

END