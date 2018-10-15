NAME MTRMAIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 MtrMain.asm                                ;
;                                 Homework 10                                ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Table of Contents
;   MotorMain               - Main function for Motor Module
;   ResetMotorMain          - It initializes(resets) all configurations and shared variables
;                             related to Motors, Serial, queues, timers, and interrupts.
;   SerialReceivedHandler   - It parses the received command and send back the status of the
;                             motor module.
;   SerialErrorHandler      - It transmits the serial error message to the remote module 
;                             when a serial error happens.
;   
;   MotorEventHandlerTable  - The table of handling routines for each type of events
;   SystemFailMsg           - The string to be transmitted when system failure occurs
;   SerialErrMsg            - The string to be transmitted when serial error occurs.
;   ParserErrMsg            - The string to be transmitted when parser error occurs.
; Description:
;   This file contains the main function which initializes all hardware/software
;   configurations of motor module and executes functionalities of the motors board.
;   It receives commands from the remote moudle through serial channel and executes 
;   appropriate actions. After executing an action, it sends back the information of the
;   changed status to the remote module through the serial channel. If a serial error occurs,
;   it sends a serial error message to the remote module so that it can display the message.
;   If a parser error occurs, it sends a parser error message to the remote module.
;   If a system failure occurs, it sends the system failure message to the remote module
;   and reset all variables and configurations of the motors module.
;   Refer to the Functional Specification document to see more detailed version of 
;   description.
; Input:  
;   Serial          
; Output: 
;   Motors, Serial      
;
; User Interface: 
;   The user can check the changed status of the motors module by reading 
;   the LED digits on the remote module. All motors and laser are controlled 
;   by the Remote module.
; Error Handling: 
;   It transmits serial error message, parser error message, and system failure message to
;   the remote module when corresponding error occurs.
; Algorithms:       None.
; Data Structures:  None
;
; Known Bugs:       None.
; Limitations:      It does not have a feedback control.
;
; Revision History:
;     12/07/2016    Sunghoon Choi      Created
;     12/08/2016    Sunghoon Choi      Initial Compilation
;     12/08/2016    Sunghoon Choi      Updated documentation   



CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK


$INCLUDE(general.inc) ;Include the .inc file which contains general constants
$INCLUDE(Queue.inc)   ;Include the .inc file which contains constants for Queue.asm
$INCLUDE(Events.inc)  ;Include the .inc file which contains the list of events for RoboTrike
$INCLUDE(MtrMain.inc) ;Include the .inc file which contains constatns for MtrMain.asm
$INCLUDE(Parser.inc)  ;Include the .inc file which contains constatns for Parser.asm

CODE    SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
        
        ;external function declarations
        EXTRN CheckSystemFail:NEAR      ;Import CheckSystemFail to check system failure
        EXTRN InitEventQueue:NEAR       ;Import InitEventQueue to initialize the EventQueue.
        EXTRN InitSerial:NEAR           ;Import InitSerial to initialize serial communication.
        EXTRN InitParallelB:NEAR        ;Import InitParallelB to initialize parallel port B.
        EXTRN InitMotorLaser:NEAR       ;Import InitMotorLaser to initialize motors.
        EXTRN GetLaser:NEAR             ;Import GetLaser to get the laser status.
        EXTRN GetMotorSpeed:NEAR        ;Import GetMotorSpeed to get the motor speed.
        EXTRN GetMotorDirection:NEAR    ;Import getMotorDirection to get the current direction.
        EXTRN ParseSerialChar:NEAR      ;Import ParseSerialChar to parse commands.
        EXTRN Dec2String:NEAR           ;Import Dec2String to convert decimal numbers to strings
        EXTRN UnsignedDec2String:NEAR   ;Import UnsignedDec2String to convert decimal numbers
                                        ;to strings in unsigned format.
        EXTRN InitParser:NEAR           ;Import InitParser to initialize parser routine.
        EXTRN InitTimer1:NEAR           ;Import InitTimer1 to initialize timer1 interrupts.
        EXTRN InstallTimer1Handler:NEAR ;Import InstallTimer1Handler to install the 
                                        ;Timer1Handler on the interrupt vector.
        EXTRN SerialPutString:NEAR      ;Import SerialPutString to send string through serial.
        EXTRN EnqueueEvent:NEAR         ;Import EnqueueEvent to enqueue an event to EventQueue
        EXTRN DequeueEvent:NEAR         ;Import DequeueEvent to dequeue an event from EventQueue
        EXTRN InitCS:NEAR               ;Initializes the chip select    
        EXTRN ClrIRQVectors:NEAR        ;Installs IllegalEventHandler for all interrupt vectors.


; MotorMain
;
; Description:
;   It first initializes(resets) all configurations and shared variables related to queues,
;   serials, motors, timers, and interrupts. Then, it dequeus an event from EventQueue and
;   call a proper handler for the event.
; Operation:
;   It calls ResetMotorMain to initialize all configurations for Motors routine.
;   Then, it checks the system failure. If the system failure has occurred, it transmits the
;   system failure message to the Remote Module and call ResetMotorMain to reset the 
;   configurations. If the system failure has not occurred, it dequeues an event from the 
;   EventQueue and uses the MotorEventHandlerTable to jump to a proper handler of the 
;   dequeued event. After handling the event, it goes back to the beginning of the loop.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;   AH(Event Type)              -    The type of the event
;    AL(Event Value)            -    The value of the event                            
; Shared Variables:  
;    None
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   It transmits System Failure message to the RemoteModule if the system failure occurs.
;   It transmits Serial Error message to the RemoteModule if a serial error occurs.
;   It transmits Parser Error message to the RemoteModule if a parser error occurs.
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, BX, CX, DX, SI, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     12/07/2016    Sunghoon Choi      Created
;     12/08/2016    Sunghoon Choi      Initial Compilation
;     12/08/2016    Sunghoon Choi      Updated documentation   

START:

MAIN:
        MOV     AX, DGROUP                    ;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP                    ;initialize the data segment
        MOV     DS, AX
        
        CALL InitCS                           ;Initializes the chip select
    
        CALL ClrIRQVectors                    ;Installs IllegalEventHandler for all
                                              ;interrupt vectors.
        
debugpoint1:        
        CALL ResetMotorMain                   ;Initializes EventQueue, Serial, Parser, Motors,
                                              ;Timer1, INT2 and shared variables for
                                              ;Motor Module's main routine.
debugpoint2:                                            
        STI                                   ;Since we are done with configuring the 
                                              ;interrupt settings, enable the interrupts
                                              ;to run RoboTrike.


CheckSystemFailure:                           ;The first thing to do is to check the 
                                              ;critical error.
        CALL  CheckSystemFail                 ;CALL CheckSystemFFail to check if a system
                                              ;failure has occurred.
        CMP   AX, TRUE                        ;Has system failure occurred?
        JNE   CheckEventQueueEmpty            ;No. Go check if the EventQueue is empty.
       ;JE    SendSystemFailure               ;Yes. Display the system failure message on
                                              ;LED digits.
       
SendSystemFailure:                   
        MOV   AX, CS                          ;Since SystemFailMsg is in the CS segment,
        MOV   ES, AX                          ;set CS=ES to call SerialPutString function.
        MOV   SI, OFFSET(SystemFailMsg)       ;Set SI to the address of SystemFailMsg so that
                                              ;SerialPutString function can output the string.
        CALL  SerialPutString                 ;Call SerialPutString to send the system
                                              ;failure message to the remote module.
        CALL  ResetMotorMain                  ;reset the remote module.
                                            
        JMP   CheckSystemFailure              ;Now that we've handled the systemfailure, go 
                                              ;back and check system failure again.

CheckEventQueueEmpty:
        CALL  DequeueEvent                    ;Dequeue an event from the EventQueue.
        JC    CheckSystemFailure              ;If the EventQueue is empty, we cannot dequeue 
                                              ;an event. Thus, go back to the beginning of
                                              ;the loop.
       ;JNC   HandleRoboEvents                ;If the EventQueue is not empty, go handle the 
                                              ;dequeued event.

CheckMotorsEvents:       

        CMP AH, SERIAL_RECEIVED_EVENT         ;Is the event SERIAL_RECEIVED_EVENT?
        JE  HandleMotorsEvents                ;Yes, go to the MotorEventHandlerTable to 
                                              ;handle it.
        
        CMP AH, SERIAL_ERROR_EVENT            ;Is the event SERIAL_ERROR_EVENT?
        JE  HandleMotorsEvents                ;Yes, go to the MotorEventHandlerTable to 
                                              ;handle it.
       JNE  CheckSystemFailure                ;If the event is an invalid event,
                                              ;go back to the beginning of the loop and check 
                                              ;the system failure.
    
HandleMotorsEvents:
       XOR  BX, BX                            ;Clear BX since we are going to update BL.
       MOV  BL, AH                            ;BL = Event Type of the dequeued event.
       SHL  BX, MULT_BY_2                     ;We use the event type as index for the 
                                              ;MotorEventHandlerTable. Since it is an word
                                              ;table, double the index.
       CALL CS:MotorEventHandlerTable[BX]     ;Using the MotorEventHandlerTable, handle
                                              ;the event by using an appropriate handler.
       JMP CheckSystemFailure                 ;Since we're done with handling the current 
                                              ;event, go back to the beginning of the loop 
                                              ;and check the system failure.
       
       
       
; MotorEventHandlerTable
;
; Description:      
;        This is the jump table used for executing appropriate handlers for each type of 
;        events.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016    Sunghoon Choi    Created
;                     12/02/2016    Sunghoon Choi    Updated documentation    
MotorEventHandlerTable LABEL   WORD
    DW  BLANK_EVENT                       ;No event is assigned for this event type.
    DW  BLANK_EVENT                       ;No event is assigned for this event type
    DW  SerialReceivedHandler             ;Go handle the serial received event
    DW  SerialErrorHandler                ;Go handle the serial error event.

    
; SystemFailMsg
;
; Description:      
;        This is the string to be shown on LED displays when a system failure occurs.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016    Sunghoon Choi    Created
;                     12/02/2016    Sunghoon Choi    Updated documentation    
SystemFailMsg   LABEL   BYTE
    DB 'SYS_FAIL', 0


; SerialErrorMsg
;
; Description:      
;        This is the string to be shown on LED displays when a serial error occurs.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016    Sunghoon Choi    Created
;                     12/02/2016    Sunghoon Choi    Updated documentation        
    

SerialErrorMsg  LABEL   BYTE
    DB  'SEri_Err', 0

    
; ParserErrorMsg
;
; Description:      
;        This is the string to be shown on LED displays when a parser error occurs.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016    Sunghoon Choi    Created
;                     12/02/2016    Sunghoon Choi    Updated documentation        
    

ParserErrorMsg  LABEL   BYTE
    DB  'PArS_Err', 0    

    

; ResetMotorMain
;
; Description:
;     It initializes(resets) all configurations and shared variables related to queues, 
;    display, keypad, serials, timers, and interrupts. Also, it initializes 
;    the RemoteDisplayIndex.
; Operation:
;    It calls InitEventQueue, InitDisplay, InitKeypad, InitSerial, InitTImer2, 
;    InstallTimer2Handler. Then, it initializes RemoteDisplayBufferIndex to
;    DISPLAY_START_INDEX.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;    None
; Shared Variables:  
;    RemoteDisplayBufferIndex    -    [Write]   -  The index used to take a value from 
;                                                  RemoteDisplayBuffer
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
;   AX, BX, CX, DX, SI, Flags
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
ResetMotorMain      PROC    NEAR
                    PUBLIC  ResetMotorMain   
                    
    CALL    InitEventQueue          ;Initializes the EventQueue.


    CALL    InitSerial              ;Initializes serial communication.
                                    ;InitSerial includes InitINT2, InstallINT2EventHandler.

    CALL    InitTimer1              ;Initializes Timer1

    CALL    InstallTimer1Handler    ;Installs Timer1Handler on the interrupt vector.
    
    CALL    InitMotorLaser        
    
    CALL    InitParser
    
    CALL    InitParallelB


    RET                             ;End of ResetMotorMain
ResetMotorMain         ENDP





; SerialReceivedHandler
;
; Description:
;     
; Operation:
;    
; Arguments:  
;   AH(Event Type)                     -    The type of the event to be enqueued 
;                                           (unused in this function)
;   AL(Event Value, ReceivedCharacter) -    The value of the event to be enqueued
; Return Value:   
;   None
; Local Variables:   
;    ReceivedCharacter(AL)             -    The event value of SERIAL_RECEIVED_EVENT.
;                                           It will be enqueued to RemoteDisplayBuffer
; Shared Variables:  

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


SerialReceivedHandler      PROC    NEAR
                           PUBLIC  SerialReceivedHandler

                           
SaveCurrentStatus:                  ;Save current status to check what's changed in future.

    PUSH AX
    XOR BX, BX
    CALL GetLaser
    MOV StatusBuffer[BX], AX        ;StatusBuffer[0] = LaserStatus
    
    ADD BX, WORD_SIZE
    CALL GetMotorSpeed
    MOV StatusBuffer[BX], AX        ;StatusBuffer[2] = Speed
    
    ADD BX, WORD_SIZE
    CALL GetMotorDirection
    MOV StatusBuffer[BX], AX        ;StatusBuffer[4] = Direction
    POP AX    
    
ParseCommand:
    
    CALL  ParseSerialChar
    CMP   AX, PARSER_SUCCESS        ;Has parsing succeeded?
    JNE   SendParserError           ;No. Go send parser error
   ;JE      FindChangedStatus       ;Yes. Find what's changed.
   
FindChangedStatus:                  ;Find what's been changed.

    XOR BX, BX
    CALL GetLaser
    CMP AX, StatusBuffer[BX]        ;Has laser status been changed?
    JNE GetChangedLaserVal
    
    ADD BX, WORD_SIZE
    CALL GetMotorSpeed
    CMP AX, StatusBuffer[BX]        ;Has speed been changed?
    JNE GetChangedSpeedVal
    
    ADD BX, WORD_SIZE
    CALL GetMotorDirection
    CMP AX, StatusBuffer[BX]        ;Has direction been changed?
    JNE GetChangedDirectionVal
    
    JMP EndSerialReceivedHandler    ;If nothing has changed, exit SerialReceivedHandler.
   
GetChangedLaserVal:
                                    ;update the laser in MotorTxBuffer
    MOV MotorTxBuffer, LASER_CHAR
    MOV SI, OFFSET(MotorTxBuffer)
    INC SI
    CALL GetLaser
    CALL Dec2String                 ;MotorTxBuffer is filled with 'L0000'
    JMP SendChangedStatus

GetChangedSpeedVal:                 ;update the speed in MotorTxBuffer
    MOV MotorTxBuffer, SPEED_CHAR
    MOV SI, OFFSET(MotorTxBuffer)
    INC SI
    CALL GetMotorSpeed
    CALL UnsignedDec2String         ;MotorTxBuffer is filled with 'S0000'
    JMP SendChangedStatus    
    
GetChangedDirectionVal:             ;update the direction in MotorTxBuffer
    MOV MotorTxBuffer, DIRECTION_CHAR
    MOV SI, OFFSET(MotorTxBuffer)
    INC SI
    CALL GetMotorDirection
    CALL Dec2String                 ;MotorTxBuffer is filled with 'D0000'
    JMP SendChangedStatus    
    
SendChangedStatus:
    MOV AX, DS
    MOV ES, AX
    MOV SI, OFFSET(MotorTxBuffer)
    CALL SerialPutString            ;Send the changed status to the remote module.

    JMP EndSerialReceivedHandler
    
SendParserError:

    MOV AX, CS                      ;Since ParserErrorMsg is in the code segment,
    MOV ES, AX                      ;we have to set CS=ES to call SerialPutString.
    MOV SI, OFFSET(ParserErrorMsg)  
    
    CALL SerialPutString            ;Send Parser Error Message to Retmote.       
    
EndSerialReceivedHandler:
    RET                             ;End of SerialReceivedHandler.

SerialReceivedHandler    ENDP



; SerialErrorHandler
;
; Description:
;     It displays SerialErrorMsg on LED digits when received a serial error.
; Operation:
;    After setting SI to the offset of SerialErrorMsg, it calls Display function to display
;   the serial error message on LED digits when a serial error is received.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;    None
; Shared Variables:  
;   None
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
;   AX, SI, Flags
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

SerialErrorHandler      PROC    NEAR
                        PUBLIC  SerialErrorHandler                        

DisplaySerialErrorMsg:
    MOV AX, CS                      ;Since SerialErrorMsg is in the code segment,
    MOV ES, AX                      ;we have to set CS=ES to call Display.
    MOV SI, OFFSET(SerialErrorMsg)  ;Set SI to the offset of SerialErrorMsg to call 
                                    ;Display to display the error message on LED digits.
    CALL SerialPutString            ;Call Display to display the error message on LED.
   
EndSerialErrorHandler:    
    RET                             ;End of SerialErrorhandler

SerialErrorHandler   ENDP

        
CODE ENDS






DATA SEGMENT PUBLIC 'DATA'            
    StatusBuffer               DW  STATUS_BUFFER_LEN   DUP (?)                                    
    MotorTxBuffer              DB  MOTOR_TX_BUFFER_LEN DUP (?)
                                   ;The buffer which contains the characters to be sent.
    
DATA ENDS




STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS
        
END    START