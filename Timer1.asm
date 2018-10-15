NAME  Timer1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                    Timer1                                  ;
;                            Timer1 related functions                        ;
;                                 Sunghoon Choi                              ; 
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;  This file contains the functions for handling the timer1 and timer1 events in
;  Robotrike.
;
; Table of Contents:
;    InitTimer1           - Initialize timer 1 interrupts and variables
;    InstallTimer1Handler - Install the timer 1 event handler
;    Timer1EventHandler   - Timer 1 Event Handler which calls MotorLaserEventHandler
;						    
;
; Revision History:
;    11/6/2016     Created                                      -     Sunghoon Choi
;    11/9/2016     Changed Timer1 interrupt frequency to 4KHz   -     Sunghoon Choi			
;    11/11/2016    Updated Documentation                        -   Sunghoon Choi





$INCLUDE(Timer.inc)		  ;include .inc file which contains constants for Timer

CGROUP GROUP CODE
CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        EXTRN MotorLaserEventHandler:NEAR	

		
; InitTimer1
;
; Description:       This function initializes timer 1
;
; Operation:         Timer 1 is initialized to generate interrupts at every 0.25ms.
;                    The interrupt controller is also initialized
;                    to allow the timer interrupts.  Timer #1 is used to scale
;                    the internal clock from 2.304 MHz to 4 KHz and generate the
;                    interrupts. 
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None.
; Global Variables:  None.
;
; Input:             None.
; Output:            Timer #1 and the Interrupt Controller are initialized.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: flags, AX, DX
; Stack Depth:       0 word
;
; Limitations:		None 
; Known bugs:       None
; Special Notes:    None
; Author:            Glen George, Sunghoon Choi
; Revision History:  10/11/1998  - Last modified   by Glen George	
;                    11/11/2016  - Last modified   by Sunghoon Choi
InitTimer1       PROC    NEAR
                 PUBLIC  InitTimer1



        MOV     DX, Tmr1Count   ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, Tmr1MaxCnt  ;setup max count for 0.25ms counts
        MOV     AX, KHZ_4_CNT   ;4KHz
        OUT     DX, AL

        MOV     DX, Tmr1Ctrl    ;setup the control register
        MOV     AX, Tmr1CtrlVal
        OUT     DX, AL


        MOV     DX, INTCtrlrCtrl;setup the interrupt control register
        MOV     AX, INTCtrlrCVal
        OUT     DX, AL

        MOV     DX, INTCtrlrEOI ;send an EOI to turn off any pending interrupts
        MOV     AX, TimerEOI
        OUT     DX, AL


        RET                     ;done so return


InitTimer1       ENDP





; InstallTimer1Handler
;
; Description:       Install the event handler for the timer1 interrupt.
;
; Operation:         The event handler address is written to the timer1
;                    interrupt vector.
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
; Registers Changed: flags, AX, ES
; Stack Depth:       0 words
;
; Limitations:		None 
; Known bugs:       None
; Special Notes:    None
; Author:            Glen George, Sunghoon Choi
; Revision History:  01/28/2002  - Last modified by Glen George
;		     11/11/2016  - Comments revised by Sunghoon Choi

InstallTimer1Handler     PROC    NEAR
                        PUBLIC  InstallTimer1Handler


        XOR     AX, AX                  ;clear ES 
					;(interrupt vectors are in segment 0)
        MOV     ES, AX
                                        ;store the vector
        MOV     ES: WORD PTR (4 * Tmr1Vec), OFFSET(Timer1EventHandler)
        MOV     ES: WORD PTR (4 * Tmr1Vec + 2), SEG(Timer1EventHandler)


        RET                             ;all done, return


InstallTimer1Handler     ENDP



; Timer1EventHandler
;
; Description:       This procedure is the event handler for the timer 1
;                    interrupt. It calls MotorLaserEventHandler to output to motors and laser.
;
; Operation:         First, it pushes all register and flags. 
;		     Then, it calls MotorLaserEventHandler to output to the motors and
;		     laser. When MotorLaserEventHandler is done, it sends EOI to the 
;		     interrupt controller.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None
; Global Variables:  None.
;
; Input:             
; Output:            Motors and laser (indirectly)
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: None
; Stack Depth:       None
;
; Limitations:		None 
; Known bugs:       None
; Special Notes:    None
; Author:            Glen George, Sunghoon Choi
; Revision History:     10/11/1998 - Last modified - by Glen George
;			11/6/2016  - Replaced the existing eventhandler with MotorLaserEventHandler. - by Sunghoon Choi
;			11/11/2016 - Updated documentation.  - by Sunghoon Choi

Timer1EventHandler   PROC    NEAR
                     
                     
        PUSHA                           ;save any registers that are used
CallEventHandlers:
     
     CALL MotorLaserEventHandler	;Output to motors and laser.
     
TimerEventEOI:                          ;send the timer EOI to the interrupt controller
        MOV     DX, INTCtrlrEOI         ;only in 80188 version
        MOV     AX, TimerEOI
        OUT     DX, AL     
        
EndTimerEventHandler:                   ;done taking care of the timer
        POPA                            ;restore the registers
        IRET
Timer1EventHandler ENDP        
       
        
CODE ENDS

END
