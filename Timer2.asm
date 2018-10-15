NAME  Timer2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                     Timer2                                 ;
;                            Timer2 related functions                        ;
;                                  Sunghoon Choi                             ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;  This file contains the functions for handling the timer2 and timer2 events in
;  Robotrike.
;
; Table of Contents:
;    InitTimer2           - Initialize timer 2 interrupts and variables
;    InstallTimer2Handler - Install the timer 2 event handler
;    Timer2EventHandler   - Timer 2 Event Handler which calls  DisplayEventHandler and KeypadEventHandler
;						   
;
; Revision History:
;    10/25/2016        Sunghoon Choi    Created
;    10/25/2016        Sunghoon Choi    Revised Timer2EventHandler by adding the call to DisplayEventHandler
;    10/26/2016        Sunghoon Choi    Updated documentation for Timer2EventHandler
;    10/31/2016        Sunghoon Choi    Added the call to KeypadEventHandler
;    11/4/2016         Sunghoon Choi    Changed the file and function names
;                                              InitTimer -> InitTimer2
;                                              InstallTimerHandler -> InstallTimer2Handler
;                                              Timer.asm -> Timer2.asm




$INCLUDE(Timer.inc)		  ;include .inc file which contains constants for Timer

CGROUP GROUP CODE
CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP
        EXTRN KeypadEventHandler:NEAR	;The eventhandler which reads the keys from
	                                ;keypad and enqueues the key events to EventBuf.
        EXTRN DisplayEventHandler:NEAR	;The eventhandler which outputs
                                        ;the patterns to 7-segments LED displays
		
; InitTimer2
;
; Description:       This function initializes timer 2
;
; Operation:         Timer 2 is initialized to generate interrupts every 1 ms.  
;					 The interrupt controller is also initialized
;                    to allow the timer interrupts.  Timer #2 is used to scale
;                    the internal clock from 2 MHz to 1 KHz and generate the
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
; Output:            Timer #2 and the Interrupt Controller are initialized.
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
;                    10/28/2016  - Last modified   by Sunghoon Choi
InitTimer2       PROC    NEAR
                 PUBLIC  InitTimer2



        MOV     DX, Tmr2Count   ;initialize the count register to 0
        XOR     AX, AX
        OUT     DX, AL

        MOV     DX, Tmr2MaxCnt  ;setup max count for 1ms counts
        MOV     AX, ONE_MS_CNT
        OUT     DX, AL

        MOV     DX, Tmr2Ctrl    ;setup the control register
        MOV     AX, Tmr2CtrlVal
        OUT     DX, AL


        MOV     DX, INTCtrlrCtrl;setup the interrupt control register
        MOV     AX, INTCtrlrCVal
        OUT     DX, AL

        MOV     DX, INTCtrlrEOI ;send an EOI to turn off any pending interrupts
        MOV     AX, TimerEOI
        OUT     DX, AL


        RET                     ;done so return


InitTimer2       ENDP





; InstallTimer2Handler
;
; Description:       Install the event handler for the timer2 interrupt.
;
; Operation:         The event handler address is written to the timer2
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
; Limitations:	    None 
; Known bugs:       None
; Special Notes:    None
; Author:            Glen George, Sunghoon Choi
; Revision History:  01/28/2002  - Last modified by Glen George
;                    10/28/2016  - Comments revised by Sunghoon Choi

InstallTimer2Handler     PROC    NEAR
                        PUBLIC  InstallTimer2Handler


        XOR     AX, AX                  ;clear ES 
			                ;(interrupt vectors are in segment 0)
        MOV     ES, AX
                                        ;store the vector
        MOV     ES: WORD PTR (4 * Tmr2Vec), OFFSET(Timer2EventHandler)
        MOV     ES: WORD PTR (4 * Tmr2Vec + 2), SEG(Timer2EventHandler)


        RET                             ;all done, return


InstallTimer2Handler     ENDP



; Timer2EventHandler
;
; Description:       This procedure is the event handler for the timer
;                    interrupt.  It calls KeypadEventHandler to read the keys
;		     and calls DisplayEventHandler to output patterns on LEDs. .
;
; Operation:         First, it pushes all register and flags. 
;		     Then, it calls KeypadEventHandler to read the keys from keypad and
;		     calls DisplayEventHandler to output the pattern values
;		     to 7-Segment LED digits.
;		     When KeypadEventHandler and DisplayEventHandler are done,
;		     it sends EOI to the interrupt controller.
;
; Arguments:         None.
; Return Value:      None.
;
; Local Variables:   None.
; Shared Variables:  None
; Global Variables:  None.
;
; Input:             Keypad
; Output:            7-Segment LED displays.
;
; Error Handling:    None.
;
; Algorithms:        None.
; Data Structures:   None.
;
; Registers Changed: None
; Stack Depth:       None
;
; Limitations:	    None 
; Known bugs:       None
; Special Notes:    None
; Author:            Glen George, Sunghoon Choi
; Revision History:     10/11/1998 - Last modified by Glen George
;                       10/29/2016 - changed the EventHandler being called  to DisplayEventHandler by Sunghoon Choi					
;                       10/31/2016 - added KeypadEventHandler by Sunghoon Choi
;                       11/4/2016  - Revised functional specification for keypad routines by Sunghoon Choi

Timer2EventHandler   PROC    NEAR
                     
                     
        PUSHA                     ;save any registers that are used
CallEventHandlers:
     
     CALL KeypadEventHandler		;reads the keys from keypad and enqueues the key events
									;to EventBuf.
     
     CALL DisplayEventHandler		;outputs the patterns to 7-segments LED displays
     
TimerEventEOI:                          ;send the timer EOI to the interrupt controller
        MOV     DX, INTCtrlrEOI         ;only in 80188 version
        MOV     AX, TimerEOI
        OUT     DX, AL     
        
EndTimerEventHandler:                   ;done taking care of the timer
        POPA                            ;restore the registers
        IRET
Timer2EventHandler ENDP        
       
        
CODE ENDS

END
