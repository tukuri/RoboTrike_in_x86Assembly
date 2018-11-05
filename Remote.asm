NAME REMOTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Remote.asm                                 ;
;                                 Homework 9							     ;
;							     Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Table of Contents
;	RemoteMain				-	Main function for Remote module
;	ResetRemote				-	It initializes(resets) all configurations and shared 
;                               variables related to queues, display, keypad, 
;                               serials, timers, and interrupts.
;	KeyInputHandler		 	-	It handles the inputs from keypad and send command to Motors 
;								module through serial.
;	SerialReceivedHandler 	- 	It displays the strings received from the motors module
;								through serial channel.
;	SerialErrorHandler    	- 	It displays serial error message when a serial error occurs.
;
;	RemoteEventHandlerTable - The table of handling routines for each type of events
;	SystemFailMsg			- The string to be displayed when system failure occurs.
;	SerialErrorMsg			- The string to be displayed when serial error occurs.
;	KeyCommandTable			- The table of commands for each keys.
;
; Description:
;	This file contains the main function which initializes all hardware configurations 
;	of the remote module and executes functionalities of the remote board.
;	It receives input from keypad and send an appropriate command to the Motors module.
;   Also, if it receives a string from the Motors module through serial channel, it
;	displays the received string on the LED display. If a serial error occurs, it displays
;	a serial error message on LED. If a system failure(critical error) occurs, it displays
;	system failure message on LED. Refer to the Functional Specification document to 
;	see more detailed version of description.
;
; Input:    Keypad, Serial        
; Output:   Display, Motors, Serial        
;
; User Interface:   User can use the keypad to send desired commands to Motors module.
;					Keypad(key indices)------------------------------
;					0	1	2	3
;					4 	5   6   7
;					8	9	10	11
;					12	13	14	15
;					Commands(for each key)----------------------------		
;					0:S65534  1:S0  2:D+45 3:D-45 4:V+1000 5:V+5000 6:V-1000 7:V-5000
;					8:T+45  9:T-45  10:E+30  11:E+60  12:E-30  13:E-60  14:F  15:O
;					[S: Set Speed] [D:Set Direction] [V:Change velocity]
;					[T:Set Turret direction] [E:Set Turret Elevation]
;					[F: Turn laser on]	[O:Turn laser off]
;					The 7-digit LED display shows the string received from Motors module.
;					It displays error message if a system failure or serial error occurs.
;					To see more detailed version of user interface, refer to the 
;					Functional Specification.	
; Error Handling:   It displays error message on LED for serial errrors and system failure.
;
; Algorithms:       None.
; Data Structures:  None
;
; Known Bugs:       None.
; Limitations:      None.
;
; Revision History:
;     12/01/2016    Sunghoon Choi      Created
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation   



CGROUP  GROUP   CODE
DGROUP  GROUP   DATA, STACK


$INCLUDE(general.inc) ;Include the .inc file which contains general constants
$INCLUDE(Queue.inc)	  ;Include the .inc file which contains constants for Queue.asm
$INCLUDE(Events.inc)  ;Include the .inc file which contains the list of events for RoboTrike
$INCLUDE(Remote.inc)  ;Include the .inc file which contains constatns for Remote.asm
$INCLUDE(Display.inc) ;Include the .inc file which contains constants for Display.asm
$INCLUDE(Parser.inc)  ;Include the .inc file which contains constatns for Parser.asm

CODE    SEGMENT PUBLIC 'CODE'


        ASSUME  CS:CGROUP, DS:DGROUP
		
        ;external function declarations
        EXTRN CheckSystemFail:NEAR	;Import CheckSystemFail to check system failure
        EXTRN InitDisplay:NEAR		;Import InitDisplay to initialize display routine
        EXTRN Display:NEAR			;Import Display to output digits(characters) on LED.
        EXTRN InitKeypad:NEAR		;Import InitKeypad to initialize keypad routine
        EXTRN InitEventQueue:NEAR	;Import InitEventQueue to initialize the EventQueue.
        EXTRN InitSerial:NEAR		;Import InitSerial to initialize serial communication.
        EXTRN InitTimer2:NEAR		;Import InitTimer2 to initialize timer2 interrupts.
        EXTRN InstallTimer2Handler:NEAR ;Import InstallTimer2Handler to install the 
										;Timer2handler on the interrupt vector.
        EXTRN SerialPutString:NEAR	;Import SerialPutString to send string through serial.
        EXTRN EnqueueEvent:NEAR		;Import EnqueueEvent to enqueue an event to EventQueue
        EXTRN DequeueEvent:NEAR		;Import DequeueEvent to dequeue an event from EventQueue
		EXTRN InitCS:NEAR      		;Initializes the chip select	
        EXTRN ClrIRQVectors:NEAR	;Installs IllegalEventHandler for all interrupt vectors.


; RemoteMain
;
; Description:
; 	It first initializes(resets) all configurations and shared variables related to queues, 
;	display, keypad, serials, timers, and interrupts. Then, it dequeues an event from 
;	EventQueue and call a proper handler for the event.
; Operation:
;	It calls ResetRemote to initialize all configurations for Remote routine.
;	Then, it checks the system failure. If the system failure has occurred, it displays
;	System Failure message on the LED and call ResetRemote to reset the configurations.
;	If the system failure has not occurred, it dequeues an event from the EventQueue
;	and uses the RemoteEventHandlerTable to jump to a proper handler of the dequeued event.
;	After handling the event, it goes back to the beginning of the loop.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;   AH(Event Type)			   -	The type of the event
;	AL(Event Value. Key Index) -	The value of the event							
; Shared Variables:  
;	None
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   None
; Error Handling:   
;   It displays System Failure message on LED if system failure occurs.
;	It displays serial error message on LED if a serial error occurs.  
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
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation 

START:

MAIN:
        MOV     AX, DGROUP            	  	;initialize the stack pointer
        MOV     SS, AX
        MOV     SP, OFFSET(DGROUP:TopOfStack)

        MOV     AX, DGROUP            		;initialize the data segment
        MOV     DS, AX
		
        CALL InitCS							;Initializes the chip select
    
        CALL ClrIRQVectors					;Installs IllegalEventHandler for all
											;interrupt vectors.
        
        
        CALL ResetRemote                    ;Initializes EventQueue, Display, Keypad,
											;Serial, Timer2, and shared variables for
											;Remote routine.
											
        STI									;Since we are done with configuring the 
											;interrupt settings, enable the interrupts
											;to run RoboTrike.


CheckSystemFailure:                		 	;The first thing to do is to check the 
											;critical error.
        CALL  CheckSystemFail				;CALL CheckSystemFFail to check if a system
											;failure has occurred.
        CMP   AX, TRUE						;Has system failure occurred?
        JNE   CheckEventQueueEmpty			;No. Go check if the EventQueue is empty.
       ;JE    DisplaySystemFailure			;Yes. Display the system failure message on
											;LED digits.
       
DisplaySystemFailure:                   
        MOV   AX, CS						;Since SystemFailMsg is in the CS segment,
        MOV   ES, AX        				;set CS=ES to call Display function.
        MOV   SI, OFFSET(SystemFailMsg)		;Set SI to the address of SystemFailMsg so that
											;Display function can output the string.
        CALL  Display						;Call Display to display SystemFailMsg on LED
        CALL  ResetRemote					;To handle the system failure, we have to 
											;reset the remote module.
        JMP   CheckSystemFailure			;Now that we've handled the systemfailure, go 
											;back and check system failure again.

CheckEventQueueEmpty:
        CALL  DequeueEvent					;Dequeue an event from the EventQueue.
        JC    CheckSystemFailure			;If the EventQueue is empty, we cannot dequeue 
											;an event. Thus, go back to the beginning of
											;the loop.
       ;JNC   HandleRoboEvents				;If the EventQueue is not empty, go handle the 
											;dequeued event.

CheckRemoteEvents:       
        CMP AH, KEY_EVENT					;Is the event KEY_EVENT?
        JE  HandleRemoteEvents				;Yes, go to the RemoteEventHandlerTable to 
											;handle it.
        
        CMP AH, SERIAL_RECEIVED_EVENT		;Is the event SERIAL_RECEIVED_EVENT?
        JE  HandleRemoteEvents				;Yes, go to the RemoteEventHandlerTable to 
											;handle it.
        
        CMP AH, SERIAL_ERROR_EVENT			;Is the event SERIAL_ERROR_EVENT?
        JE  HandleRemoteEvents				;Yes, go to the RemoteEventHandlerTable to 
											;handle it.
       JNE  CheckSystemFailure      		;If the event is an invalid event,
											;go back to the beginning of the loop and check 
											;the system failure.
    
HandleRemoteEvents:
       XOR  BX, BX							;Clear BX since we are going to update BL.
       MOV  BL, AH 							;BL = Event Type of the dequeued event.
       SHL  BX, MULT_BY_2       			;We use the event type as index for the 
											;RemoteEventHandlerTable. Since it is an word
											;table, double the index.
       CALL CS:RemoteEventHandlerTable[BX]	;Using the RemoteEventHandlerTable, handle
											;the event by using an appropriate handler.
       JMP CheckSystemFailure				;SInce we're done with handling the current 
											;event, go back to the beginning of the loop 
											;and check the system failure.
       
       
       
; RemoteEventHandlerTable
;
; Description:      
;		This is the jump table used for executing appropriate handlers for each type of 
;		events.
; Notes:            
;		This table is declared PRIVATE to prevent other codes accessing the table. 
;		Also, READ ONLY tables should always be in the code segment so that in a standalone 
;		system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016	Sunghoon Choi	Created
;					  12/02/2016    Sunghoon Choi	Updated documentation	
RemoteEventHandlerTable LABEL   WORD
    DW  BLANK_EVENT						;No event is assigned for this event type.
    DW  KeyInputHandler					;Go handle the key pressed event
    DW  SerialReceivedHandler			;Go handle the serial received event
    DW  SerialErrorHandler				;Go handle the serial error event.

	
; SystemFailMsg
;
; Description:      
;		This is the string to be shown on LED displays when a system failure occurs.
; Notes:            
;		This table is declared PRIVATE to prevent other codes accessing the table. 
;		Also, READ ONLY tables should always be in the code segment so that in a standalone 
;		system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016	Sunghoon Choi	Created
;					  12/02/2016    Sunghoon Choi	Updated documentation	
SystemFailMsg   LABEL   BYTE
    DB 'SYS_FAIL', 0


; SerialErrorMsg
;
; Description:      
;		This is the string to be shown on LED displays when a serial error occurs.
; Notes:            
;		This table is declared PRIVATE to prevent other codes accessing the table. 
;		Also, READ ONLY tables should always be in the code segment so that in a standalone 
;		system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016	Sunghoon Choi	Created
;					  12/02/2016    Sunghoon Choi	Updated documentation		
	

SerialErrorMsg  LABEL   BYTE
    DB  'SEri_Err', 0


; KeyCommandTable
;
; Description:      
;		This is the table of commands assigned for each keys.
;		The map of key indices:
;		0	1	2	3
;		4   5   6   7
;		8	9	10	11
;		12	13	14	15
; Notes:            
;		This table is declared PRIVATE to prevent other codes accessing the table. 
;		Also, READ ONLY tables should always be in the code segment so that in a standalone 
;		system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   12/01/2016	Sunghoon Choi	Created
;					  12/02/2016    Sunghoon Choi	Updated documentation	
    
KeyCommandTable  LABEL  BYTE
DB  'S65534',0,0            ;key 0
DB  'S0',0,0,0,0,0,0        ;key 1
DB  'D+45',0,0,0,0          ;key 2
DB  'D-45',0,0,0,0          ;key 3

DB  'V+1000',0,0            ;key 4
DB  'V+5000',0,0            ;key 5
DB  'V-1000',0,0            ;key 6
DB  'V-5000',0,0            ;key 7

DB  'T+45',0,0,0,0          ;key 8
DB  'T-45',0,0,0,0          ;key 9
DB  'E+30',0,0,0,0          ;key 10
DB  'E+60',0,0,0,0          ;key 11

DB  'E-30',0,0,0,0          ;key 12
DB  'E-60',0,0,0,0          ;key 13
DB  'F',0,0,0,0,0,0,0       ;key 14
DB  'O',0,0,0,0,0,0,0       ;key 15

    
; ResetRemote
;
; Description:
; 	It initializes(resets) all configurations and shared variables related to queues, 
;	display, keypad, serials, timers, and interrupts. Also, it initializes 
;	the RemoteDisplayIndex.
; Operation:
;	It calls InitEventQueue, InitDisplay, InitKeypad, InitSerial, InitTImer2, 
;	InstallTimer2Handler. Then, it initializes RemoteDisplayBufferIndex to
;	DISPLAY_START_INDEX.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;	None
; Shared Variables:  
;	RemoteDisplayBufferIndex	-	[Write]	-	The index used to take a value from
;												RemoteDisplayBuffer
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
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation      
ResetRemote         PROC    NEAR
                    PUBLIC  ResetRemote   
                    
    CALL    InitEventQueue			;Initializes the EventQueue.

    CALL    InitDisplay				;Initializes display routine

    CALL    InitKeypad				;Initializes keypad routine

    CALL    InitSerial  			;Initializes serial communication.
									;InitSerial includes InitINT2, InstallINT2EventHandler.

    CALL    InitTimer2  			;Initializes Timer2

    CALL    InstallTimer2Handler	;Installs Timer2Handler on the interrupt vector.

    MOV     RemoteDisplayBufferIndex, DISPLAY_START_INDEX
									;Reset RemoteDisplayBufferIndex.

    RET             				;End of ResetRemote
ResetRemote         ENDP


; KeyInputHandler
;
; Description:
; 	It uses the Event Value of Keypad event to find an appropriate command string in
;	KeypadCommandTable and send the string to Motor module through serial channel.
; Operation:
;	It multiplies the Event Value (= Key Value) with the length of each command string to 
;	find the index in KeypadCommandTable. Then, it adds the calculated index to the offset
;	of KeypadCommandTable to find the exact address of the target command. Finally, it 
;	calls SerialPutString to send the command string to Motor module through serial channel.
; Arguments:  
;   AH(Event Type)			   -	The type of the event to be enqueued
;	AL(Event Value. Key Index) -	The value of the event to be enqueued.
;									Here, it means the index of the pressed key.
; Return Value:   
;   None
; Local Variables:   
;	KeyCommandAddr(SI)	-	The address of the command string for current key input.
; Shared Variables:  
;	None
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
;   AX, BX, DX, SI, Flags
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
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation    

KeyInputHandler         PROC    NEAR
                        PUBLIC  KeyInputHandler
                        
    XOR AH,AH         			  ;AL = Key's index.
								  ;To multiply AX with COMMAND_LENGTH, we have to clear AH.
    MOV BX, COMMAND_LENGTH		  ;It multiplies Key_Index with COMMAND_LENGTH to find the 
								  ;Key Command's index in KeyCommandTable.
    XOR DX, DX					  ;Clear DX for multiplication.
    MUL BX          			  ;DX:AX = KeyCommand's Index inside KeyCommandTable

    ADD AX, OFFSET(KeyCommandTable)	;KeyCommand's Address
									; = OFFSET(KeyCommandTable) + KeyCommand index in table

    MOV SI, AX      			    ;SI = KeyCommand's address
    MOV AX, CS						;Since the key command table is in CS segment,
    MOV ES, AX      				;CS=ES must be performed to call SerialPutString.
    CALL SerialPutString        	;Send the Key Command to Motor module through the
									;serial channel.

    RET								;End of KeyInputHandler
 
KeyInputHandler     ENDP


; SerialReceivedHandler
;
; Description:
; 	It displays the characters received from the serial channel on LED digits.
; Operation:
;	It first checks if the received character is Carriage Return. If it is, display the 
;	characters in RemoteDisplayBuffer by attaching NULL in the end of the string. After 
;   calling Display function to display the string, it resets RemoteDisplayIndex to
;   the starting index of RemoteDisplayBuffer.
;   If the received character is not the Carriage Return, it stores the received character in 
;   RemoteDisplayBuffer at RemoteDisplayIndex. Then, it increments RemoteDisplayIndex. 
;	If the incremented RemoteDisplayIndex is larger than or equal to the length of 
;   RemoteDisplayBuffer, fix RemoteDisplayIndex at DISPLAY_BUFFER_LEN-1 so that it can 
;   display the truncated characters when a carriage return is received later.
; Arguments:  
;   AH(Event Type)	-	The type of the event to be enqueued (unused in this function)
;   AL(Event Value, ReceivedCharacter) -	The value of the event to be enqueued
; Return Value:   
;   None
; Local Variables:   
;	ReceivedCharacter(AL)	-	The event value of SERIAL_RECEIVED_EVENT.
;								It will be enqueued to RemoteDisplayBuffer
; Shared Variables:  
;   RemoteDisplayBuffer	- [Write] - The buffer which contains the characters to be displayed
;	RemoteDisplayIndex  - [R/W] -   The index for RemoteDisplayBuffer.
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
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation    

SerialReceivedHandler       PROC    NEAR
                           PUBLIC  SerialReceivedHandler

CheckCarriageReturn: 
    CMP AL, CARRIAGE_RETURN         ;Is the character Carriage Return?
    JE  DisplayReceivedStr          ;Yes. Display the string in RemoteDisplayBuffer.
    ;JNE  UpdateDisplayBuffer       ;No. Store the current character in RemoteDisplayBuffer.
UpdateDisplayBuffer:
    MOV BX, RemoteDisplayBufferIndex ;Save RemoteDisplayBufferIndex in BX so that we can 
                                     ;store the character in a proper index of 
                                     ;RemoteDisplayBuffer.
    MOV RemoteDisplayBuffer[BX], AL  ;Save the character in RemoteDisplayBuffer.
IncrementBufferIndex:
    INC RemoteDisplayBufferIndex     ;Proceed to storing next character by incrementing 
                                     ;RemoteDisplayBufferIndex.
    CMP RemoteDisplayBufferIndex, REMOTE_BUFFER_LEN-1   
                                    ;Is RemoteDisplayBufferIndex >= REMOTE_BUFFER_LEN ?
    JNA EndSerialReceivedHandler     ;No. We still have rooms to store characters in 
                                    ;RemoteDisplayBuffer.
    ;JA WaitUntilCR                 ;Yes. We we have to fix the index at the last character
                                    ;of RemoteDisplayBuffer and wait until it Carriage
                                    ;Return arrives.
WaitUntilCR:
    MOV RemoteDisplayBufferIndex, REMOTE_BUFFER_LEN-1
                                    ;Fix the index at the last index of RemoteDisplayBuffer
                                    ;and wait until it receives Carriage Return.
    JMP EndSerialReceivedHandler     ;Exit the procedure without displaying the string 
                                    ;since we haven't received Carriage Return.
DisplayReceivedStr:
    MOV BX, RemoteDisplayBufferIndex ;Save RemoteDisplayBufferIndex in BX so that we can 
                                     ;store the character in a proper index of 
                                     ;RemoteDisplayBuffer.
    MOV RemoteDisplayBuffer[BX], 0   ;String must end with NULL.
    MOV AX, DS                       ;Since RemoteDisplayBuffer is in Data segment,
    MOV ES, AX                       ;we have to set DS = ES to call Display.
    MOV SI, OFFSET(RemoteDisplayBuffer) ;Set SI to the offset of RemoteDisplayBuffer
                                        ;to call display to print the string in
                                        ;RemoteDisplayBuffer.
    CALL Display                        ;Display the string stored in RemoteDisplayBuffer.
    MOV RemoteDisplayBufferIndex, STARTING_INDEX     
                                       ;Since we displayed the string in RemoteDisplayBuffer,
                                       ;reset the index to the beginning index of the buffer.
EndSerialReceivedHandler:
    RET                                 ;End of SerialReceivedHandler.

SerialReceivedHandler    ENDP

; SerialErrorHandler
;
; Description:
; 	It displays SerialErrorMsg on LED digits when received a serial error.
; Operation:
;	After setting SI to the offset of SerialErrorMsg, it calls Display function to display
;   the serial error message on LED digits when a serial error is received.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;	None
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
;	  12/02/2016	Sunghoon Choi	   Initial Compilation
;	  12/02/2016    Sunghoon Choi      Updated documentation     

SerialErrorHandler      PROC    NEAR
                        PUBLIC  SerialErrorHandler                        

DisplaySerialErrorMsg:
    MOV AX, CS                      ;Since SerialErrorMsg is in the code segment,
    MOV ES, AX                      ;we have to set CS=ES to call Display.
    MOV SI, OFFSET(SerialErrorMsg)  ;Set SI to the offset of SerialErrorMsg to call 
                                    ;Display to display the error message on LED digits.
    CALL Display                    ;Call Display to display the error message on LED.
    
EndSerialErrorHandler:    
    RET                             ;End of SerialErrorhandler

SerialErrorHandler   ENDP

		
CODE ENDS






DATA SEGMENT PUBLIC 'DATA'			
										
    RemoteDisplayBufferIndex    DW  ?     
                                    ;The index of current character for RemoteDisplayBuffer.
    RemoteDisplayBuffer         DB  REMOTE_BUFFER_LEN DUP (?)
                                    ;The buffer which contains the characters to be displayed
    
DATA ENDS




STACK   SEGMENT STACK  'STACK'

                DB      80 DUP ('Stack ')       ;240 words

TopOfStack      LABEL   WORD

STACK   ENDS
		
END	START
