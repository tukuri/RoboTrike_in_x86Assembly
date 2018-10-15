     Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                Events.asm                                  ;
;                                 Homework9                                  ;
;                               Sunghoon Choi                                ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Description:
;     This file contains the functions necessary to initialize and handle EventQueue.
; Table of Contents:
;    InitEventQueue      -    Initializes the EventQueue.
;    EnqueueEvent        -    Enqueues an event to EventQueue.
;    DequeueEvent        -    Dequeues an event from EventQueue.
;    CheckSystemFail     -    Checks if system failure has occurred.
; Revision History:
;    12/01/2016      Sunghoon Choi         Created
;    12/02/2016      Sunghoon Choi        Initial Compilation
;    12/02/2016      Sunghoon Choi        Corrected minor syntax errors.
;    12/02/2016      Sunghoon Choi        Added comments


$INCLUDE(general.inc)        ;Include the .inc file which contains general constants
$INCLUDE(queue.inc)          ;Include the .inc file which contains constants for Queue.asm

EXTRN   QueueFull:NEAR       ;Import QueueFull to check if EventQueue is full.
EXTRN   QueueEmpty:NEAR      ;Import QueueEmpty to check if EventQueue is empty.
EXTRN   QueueInit:NEAR       ;Import QueueInit to initialize EventQueue.
EXTRN   Enqueue:NEAR         ;Import Enqueue to insert an event to EventQueue.
EXTRN   Dequeue:NEAR         ;Import Dequeue to pop an event from EventQueue.

CGROUP GROUP CODE        
DGROUP GROUP DATA
CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP
       

; InitEventQueue
;
; Description:
;     It initializes the EventQueue which contains Event Types and Event Values.
; Operation:
;    It sets SI to the address of EventQueue and set BL to WORD_SIZE. Then, it calls 
;    QueueInit to initialize the EventQueue. Finally, it resets SystemFailFlag and exits.
; Arguments:  
;    None
; Return Value:   
;   None
; Local Variables:   
;   SI(EventQueueAddress)    -    The address of the EventQueue.
;    BL(EventQueueType)      -    The size of the elements of EventQueue.
; Shared Variables:  
;    EventQueue        -    [Write]    -    The queue which contains event type and event value for
;                                           each events
;    SystemFailFlag    -    [Write]    -    Indicates whether system failure has occurred or not.
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   SI, BX
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;     12/02/2016    Sunghoon Choi      Initial Compilation
;     12/02/2016    Sunghoon Choi      Updated documentation    

InitEventQueue  PROC    NEAR
                PUBLIC  InitEventQueue

InitializeEventQueue:                
    MOV SI, OFFSET(EventQueue)            ;Sets SI to the offset of EventQueue to initialize it
    MOV BL, WORD_SIZE                     ;Set the EventQueue to a WORD queue.

    CALL QueueInit                        ;Call QueueInit to initialize the EventQueue.
ResetSystemFailFlag:
    MOV SystemFailFlag, 0                 ;Reset SystemFailFlag.
EndInitEventQueue:
    RET                                   ;End of InitEventQueue.
InitEventQueue  ENDP




; EnqueueEvent
;
; Description:
;     It enqueues an event to EventQueue.
; Operation:
;    It first sets SI to the offset of the EventQueue. Then, it calls QueueFull to check if
;    the EventQueue is full. If it is not full, it inserts the argument event to the 
;    EventQueue by calling Enqueue. If it is full, set the SystemFailFlag and exit.
; Arguments:  
;    AH(Event Type)    -    The type of the event to be enqueued
;    AL(Event Value)   -    The value of the event to be enqueued   
; Return Value:   
;   None
; Local Variables:   
;    SI(EventQueueAddress)  -    The address of EventQueue
; Shared Variables:  
;    EventQueue      -   [Write]  -  The queue which contains event type and event value for
;                                    each events
;    SystemFailFlag  -   [Write]  -  Indicates whether a system failure has occurred.
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, BX, SI, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;     12/02/2016    Sunghoon Choi      Initial Compilation
;     12/02/2016    Sunghoon Choi      Updated documentation 
    
EnqueueEvent    PROC    NEAR
                PUBLIC  EnqueueEvent
    PUSH SI                              ;Save the address of EventQueue
CheckEventQueueFull:          
    MOV SI, OFFSET(EventQueue)          ;Set SI to the offset of EventQueue to check if it
                                        ;is full.
    PUSHA                               ;push registers since QueueFull changes them.
    CALL QueueFull                      ;Check if EventQueue is full.
    POPA                                ;Retrieve registers since QueueFull is done.
    JNZ  InsertEvent                    ;If EventQueue is not full, go insert the event.
    ;JZ  SetSystemFailFlag              ;If it is full, set the system failure flag.
SetSystemFailFlag:
    MOV SystemFailFlag, TRUE            ;Set the SystemFailFlag since the EventQueue is full.
    JMP EndEnqueueEvent                 ;Exit EnqueueEvent procedure.
InsertEvent:
    CALL Enqueue                        ;Insert the argument event on EventQueue.
EndEnqueueEvent:
    POP SI                              ;Retrieve the address of EventQueue.
    RET                                 ;End of EnqueueEvent.
EnqueueEvent    ENDP


; DequeueEvent
;    
; Description:
;     It dequeues an event from EventQueue.
; Operation:
;    It first sets SI to the offset of the EventQueue. Then, it calls QueueEmpty to check if
;    the EventQueue is empty. If EventQueue is empty, it sets the carry flag and exit. If 
;    EventQueue is not empty, it dequeues an event from EventQueue and clear the carry flag, 
;    and exits.    
; Arguments:  
;   None
; Return Value:   
;   AH(Event Type)    -    The type of the event dequeued from EventQueue.
;    AL(Event Value)  -    The value of the event dequeued from EventQueue.
;    Carry Flag       -    Set if EventQueue is empty.
;                          Reset if EventQueue is not empty.
; Local Variables:   
;    SI(EventQueueAddress)   -    The address of EventQueue    
; Shared Variables:  
;    EventQueue      -   [Read]  -  The queue which contains event type and event value for
;                                    each events
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, BX, SI, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;     12/02/2016    Sunghoon Choi      Initial Compilation
;     12/02/2016    Sunghoon Choi      Updated documentation     
    
DequeueEvent    PROC    NEAR
                PUBLIC  DequeueEvent
    
CheckEventQueueEmpty:          
    MOV SI, OFFSET(EventQueue)    ;Set SI to the offset of EventQueue to check if it is empty
    CALL QueueEmpty               ;Check if EventQueue is empty.
    JZ  SetCarryFlag              ;If EventQueue is empty, go set the carrfy flag and exit.
   ;JNZ PopEvent                  ;If EventQueue is not empty, dequeue an event.
PopEvent:
    CALL Dequeue                  ;CALL Dequeue to dequeue an event from the EventQueue.
    CLC                           ;If an event was dequeued, clear the carry flag.
    JMP EndDequeueEvent           ;Exit the procedure.
SetCarryFlag:
    STC                           ;Since EventQueue is empty, set the carry flag
EndDequeueEvent:
    RET                           ;End of DequeueEvent
DequeueEvent    ENDP


; CheckSystemFail
;
; Description:
;     It checks if a system failure has occurred
; Operation:
;    It returns the value of SystemFailFlag in AX.
; Arguments:  
;   None
; Return Value:   
;   AX(SystemFailFlag)    -    Indicates whether a system failure has occurred or not.
; Local Variables:   
;    None
; Shared Variables:  
;    SystemFailFlag       -  [Read] -  Indicates whether a system failure has occurred or not.
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;     12/02/2016    Sunghoon Choi      Initial Compilation
;     12/02/2016    Sunghoon Choi      Updated documentation     

CheckSystemFail     PROC    NEAR
                    PUBLIC  CheckSystemFail
                    
    MOV AX, SystemFailFlag        ;Return the value of SystemFailFlag in AX
    RET                           ;End of CheckSystemFail

CheckSystemFail ENDP


CODE ENDS
     
    
DATA    SEGMENT PUBLIC  'DATA'

EventQueue  QueueModule     <>  ;The queue which contains event type and event value for
                                ;each events
SystemFailFlag  DW  ?           ;Flag that indicates whether a system failure has occurred 
                                ;or not.

DATA  ENDS
    
END    