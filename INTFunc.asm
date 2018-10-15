NAME  INTFunc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 INTFunc                                    ;
;                         Interrupt related functions                        ;
;                               Sunghoon Choi                                ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;     This file contains the interrupt initialization functions for LEDs.
; Table of Contents:
;     ClrIRQVectors       - clears the interrupt vector table
;     IllegalEventHandler - handler for illegal (uninitialized) interrupts
;
; Revision History:
;     10/25/2016    Sunghoon Choi    Created
;     10/29/2016    Sunghoon Choi    Updated Documentation



    

$INCLUDE(INTFunc.INC)


CGROUP GROUP CODE

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP



; ClrIRQVectors
;
; Description:      This functions installs the IllegalEventHandler for all
;                   interrupt vectors in the interrupt vector table.  Note
;                   that all 256 vectors are initialized so the code must be
;                   located above 400H.  The initialization skips  (does not
;                   initialize vectors) from vectors FIRST_RESERVED_VEC to
;                   LAST_RESERVED_VEC.
;
; Operation:        All vectors are initialized to IllegalEventHandler in a
;                   loop.  The vectors from FIRST_RESERVED_VEC to
;                   LAST_RESERVED_VEC are skipped.
;
; Arguments:        None.
; Return Value:     None.
;
; Local Variables:  CX    - vector counter.
;                   ES:SI - pointer to vector table.
; Shared Variables: None.
; Global Variables: None.
;
; Input:            None.
; Output:           None.
;
; Error Handling:   None.
;
; Algorithms:       None.
; Data Structures:  None.
;
; Registers Used:   flags, AX, CX, SI, ES
; Stack Depth:      1 word
;
; Limitations:      None  
; Known bugs:       None
; Special Notes:    None
; Author:           Glen George, Sunghoon Choi
; Revision History:     07/12/2010    last modified by Glen George
;                       10/28/2016    last modified by Sunghoon Choi

ClrIRQVectors   PROC    NEAR
                PUBLIC  ClrIRQVectors


InitClrVectorLoop:              ;setup to store the same handler 256 times

        XOR     AX, AX          ;clear ES (interrupt vectors are in segment 0)
        MOV     ES, AX
        MOV     SI, 0           ;initialize SI to the first vector

        MOV     CX, NUM_IRQ_VECTORS     ;number of vectors to initialize


ClrVectorLoop:                              ;loop clearing each vector
                                            ;check if should store the vector
        CMP     SI, 4 * FIRST_RESERVED_VEC
        JB      DoStore                     ;if before start of reserved field - store it
        CMP     SI, 4 * LAST_RESERVED_VEC
        JBE     DoneStore                   ;if in the reserved vectors - don't store it
        ;JA     DoStore                     ;otherwise past them - so do the store

DoStore:                                    ;store the vector
        MOV     ES: WORD PTR [SI], OFFSET(IllegalEventHandler)
        MOV     ES: WORD PTR [SI + 2], SEG(IllegalEventHandler)

DoneStore:                                  ;done storing the vector
        ADD     SI, 4                       ;update pointer to next vector

        LOOP    ClrVectorLoop               ;loop until have cleared all vectors
        ;JMP    EndClrIRQVectors;and all done


EndClrIRQVectors:                           ;all done, return
        RET


ClrIRQVectors   ENDP




; IllegalEventHandler
;
; Description:       This procedure is the event handler for illegal
;                    (uninitialized) interrupts.  It does nothing - it just
;                    returns after sending a non-specific EOI (in the 80188
;                    version).
;
; Operation:         Send a non-specific EOI (80188 version only) and return.
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
; Registers Changed: None
; Stack Depth:       2 words
;
; Limitations:       None 
; Known bugs:        None
; Special Notes:     None
; Author:            Glen George, Sunghoon Choi
; Revision History:  07/12/2010  Last modified by Glen George
;                    10/28/2016  Last modified by Sunghoon Choi

IllegalEventHandler     PROC    NEAR


        NOP                             ;do nothing (can set breakpoint here)

        PUSH    AX                      ;save the registers
        PUSH    DX

        MOV     DX, INTCtrlrEOI         ;send a non-specific EOI to the
        MOV     AX, NonSpecEOI          ;interrupt controller to clear out
        OUT     DX, AL                  ;the interrupt that got us here

        POP     DX                      ;restore the registers
        POP     AX


        IRET                            ;and return


IllegalEventHandler     ENDP



CODE ENDS



        END
