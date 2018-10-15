NAME Serial

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Serial                                   ;
;                                  Homework 7                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Description:
;    This file contains the functions necessary to initialize and enable serial communication
;    of the RoboTrike. 
; Table of Contents:
;    InitSerial         -   Initializes shared variables related to Serial routine
;                           and initializes settings of serial transmission.
;    InitINT2           -   Initializes INT2 interrupt.
;    SetBaudRate        -   Set the baud rate of serial communication.
;    SetParity          -   Set the parity of serial communication.
;    SerialPutChar      -   Stores the passed character in TxQueue.
;   SerialEventHandler  -   Identify the interrupt occured at serial channel and execute
;                           appropriate actions for each type of interrupt.
;                           It is called by INT2 interrupt.
;    SerialPutString    -   Stores the passed string in TxQueue.
; Revision History:
;     11/16/2016      Sunghoon Choi     Created
;     11/16/2016      Sunghoon Choi     Initial Compilation
;     11/16/2016      Sunghoon Choi     Corrected LCR configuration error
;     11/18/2016      Sunghoon Choi     Added SetParity function          
;     12/02/2016      Sunghoon Choi     Added SerialPutString function  

CGROUP GROUP CODE
DGROUP GROUP DATA

$INCLUDE(Serial.inc)     ;Include the .inc file which contains constatns for Serial.asm
$INCLUDE(Events.inc)     ;Include the .inc file which contains the list of events for RoboTrike
$INCLUDE(Queue.inc)      ;Include the .inc file which contains constants for Queue.asm
$INCLUDE(general.inc)    ;Include the .inc file which contains general constants for RoboTrike

EXTRN QueueInit:NEAR     ;Import QueueInit to initialize TxQueue.
EXTRN Enqueue:NEAR       ;Import Enqueue to enqueue characters to TxQueue.
EXTRN Dequeue:NEAR       ;Import Dequeue to dequeue characters from Dequeue.
EXTRN QueueFull:NEAR     ;Import QueueFull to check if TxQueue is full.
EXTRN QueueEmpty:NEAR    ;Import QueueEmpty to check if TxQueue is empty.
EXTRN EnqueueEvent:NEAR  ;Import EnqueueEvent to enqueue serial events to EventBuf.


CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

; BaudRateTable
;
; Description:      
;        This is the table of divisors to enable desired baud rates of serial communication 
;        for RoboTrike.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:             Sunghoon Choi
; Revision history:   11/16/2016    Sunghoon Choi    Created
;                     11/18/2016    Sunghoon Choi    Updated documentation    
        
BaudRateTable   LABEL   WORD    
DW         120  ;divisor for 4800 baud rate.  9.216MHz/16/4800       
DW          60  ;divisor for 9600 baud rate.  9.216MHz/16/9600
DW          40  ;divisor for 14400 baud rate. 9.216MHz/16/14400
DW          30  ;divisor for 19200 baud rate. 9.216MHz/16/19200
DW          15  ;divisor for 38400 baud rate. 9.216MHz/16/38400



; ParityTypeTable
;
; Description:      
;        This is the jump table for setting parity.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   11/16/2016    Sunghoon Choi    Created
;                     11/18/2016    Sunghoon Choi    Updated documentation
ParityTypeTable LABEL   WORD
DW  DisableParity          ;Jump to disable parity
DW  EnableEvenParity       ;Jump to enable even parity
DW  EnableOddParity        ;Jump to enable odd parity
DW  TransmitParityAndClear ;Jump to transmit parity and check as cleared.(Stick Parity)
DW  TransmitParityAndSet   ;Jump to transmite parity and check as set.(Stick Parity)
DW  EnableBreak            ;Jump to force a break condition.(Break control)



; SerialINTTypeTable
;
; Description:      
;        This is the jump table used for executing appropriate actions for each type of
;        serial interrupts.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:           Sunghoon Choi
; Revision history:   11/16/2016    Sunghoon Choi    Created
;                     11/18/2016    Sunghoon Choi    Updated documentation            
        
SerialINTTypeTable  LABEL   WORD
DW  ModemStatusInterrupt            ;Jump to handle the Modem Status Interrupt
DW  TransmitterEmptyInterrupt       ;Jump to handle Transmitter Empty Interrupt
DW  ReceivedDataAvailInterrupt      ;Jump to handle Received Data Available Interrupt
DW  ReceiverLineStatusInterrupt     ;Jump to handle Reciever Line Status Interrupt




; InitSerial
;
; Description:
;    Initializes the TxQueue, kickstart, and registers of the serial chip.
;    Initialization of serial chip's registers includes baud rate setting and parity setting.
;    Installs interrupt vector and enable the interrupt for serial I/O and send SerialEOI.  
; Operation:  
;    First, it initializes the TxQueue by calling QueueInit.  When this is 
;    done, it sets LCR register's value and enables the serial hardware interrupt by
;    setting the bits of IER register. Next, it calls SetBaudRate to
;    set the baud rate of serial I/O and calls SetParity to set the parity.
;   SetBaudRate and SetParity do not change any bits other than baud bits and 
;   parity bits since they use AND, OR instructions with masks to prevent other
;   bits being changed. Thus, the initialized values for LCR and IER are safe.
;   Finally, it installs SerialEventHandler in INT2 vector and enables INT2 by 
;   calling InitINT2 function.
; Arguments:  
;     None
; Return Value:   
;     None
; Local Variables:   
;     TxQueueAddr(SI) -  The address of TxQueue
; Shared Variables:  
;      TxQueue(DS)    -  [Write] - The queue which contains the characters to be transmitted
;                                  to serial channel.
;      kickstart(DS)  -  [Write] - Indicates whether kickstart is needed to reactivate 
;                                  THRE interrupt to conitnue data transmission.  
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     LCR(Line Control Register), IER(Interrupt Enable Register), 
;      EOI(End of Interrupt) Register  
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Queue(TxQueue)  
; Registers Changed: 
;     AX, BX, DX, SI, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation        

InitSerial  PROC    NEAR
            PUBLIC  InitSerial
            
InitTxQueue:
    MOV SI, OFFSET(TxQueue) ;We have to initialize TxQueue before using it as a
                            ;transmission queue which contains characters to be 
                            ;transmitted. Thus, get the address of it.
    MOV BL, FALSE           ;Initialize TxQueue to a byte-type queue.
                            ;BL is an argument for QueueInit. If it is FALSE, TxQueue
                            ;gets initialized to a byte queue. Otherwise, it will get
                            ;intialized to a word queue.
    PUSHA                   ;Save all register values since QueueInit function changes
                            ;the values of registers.
    CALL QueueInit          ;Initialize TxQueue by using QueueInit function.
    POPA                    ;Retrieve all register values since we are back from QueueInit

InitLineControlRegister:    
    MOV DX, LCR_ADDR        ;We have to control the formate of asynchronous data 
                            ;communication exchange through the LCR.
                            ;Thus, get the address of LCR register.
    MOV AL, LCR_VAL         ;Prepare the value to be written to LCR register.
                            ;0------- DLAB bit off. Access RBR, THR
                            ;-0------ Break condition disbaled
                            ;--0----- No Parity
                            ;---0---- No Parity
                            ;----0--- No Parity
                            ;-----0-- One Stop bit
                            ;------11 8 Bits Word length
    OUT DX, AL              ;Set up the LCR by writing the prepared configuration value
                            ;to LCR register's address.

InitINTEnableRegister:
    MOV DX, IER_ADDR        ;We have to enable the interrupts of serial communication.
                            ;Thus, get the address of IER register.
    MOV AL, IER_VAL         ;Prepare the value to be written to IER register.
                            ;0000---- Bits4-7 of IER are ALWAYS cleared
                            ;----1--- Modem Status Interrupt Enabled
                            ;-----1-- Receiver Line Status Interrupt Enabled
                            ;------1- THRE interrupt enabled
                            ;-------1 Received Data Availalbe Interrupt enabled
    OUT DX, AL              ;Set up the IER by writing the prepared configuration value
                            ;to IER reigster's address.                            
InitBaudRate:    
    MOV BX, BAUD_RATE_INDEX ;Choose a desired baud rate so that SetBaudRate function can 
                            ;get a proper divisor value from BaudRateTable and set 
                            ;the proper baud rate.
    CALL SetBaudRate        ;Sets baud rate of serial communication to the desired value
                            ;Note that SetBaudRate does not change any bits except
                            ;those bits related to baud rate.

InitParity:
    MOV BX, DISABLE_PARITY  ;We disable parity for now.
                            ;Thus, give DISABLE_PARITY as an argument of SetParity.
    CALL SetParity          ;Disables parity by calling SetParity with DISABLE_PARITY 
                            ;as an argument.
                            ;Note that SetParity does not change any bits except
                            ;those bits related to parity setting.
InitINT2Interrupt:
    CALL InitINT2           ;Installs SerialEventHandler in INT2 vector and
                            ;enables INT2 interrupt by calling InitINT2.
InitKickstart:
    MOV kickstart, KICKSTART_OFF  ;Resets the kickstart flag for intialization.
    
EndInitSerial:
    RET
InitSerial ENDP


  
; InitINT2
;
; Description:
;   This function initializes INT2 interrupt.
; Operation:
;   Installs SerialEventHandler in INT2 vector.
;   Then, Interrupt Control Register of INT2 is initialized to enable INT2
;   interrupt with INT2_PRIORITY_LEVEL priority.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;   None
; Shared Variables:  
;   None  
; Global Variables:  
;   None  
; Input:    
;   None          
; Output:            
;   INT2 and the Interrupt Controller are initialized.
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, DX, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation    


InitINT2    PROC    NEAR
            PUBLIC  InitINT2

InstallINT2:
        
    XOR     AX, AX          ;clear ES                                 
    MOV     ES, AX          ;(interrupt vectors are in segment 0                   
                            ;store the INT2 vector
    MOV     ES: WORD PTR (4 * Int2Vec), OFFSET(SerialEventHandler)                       
    MOV     ES: WORD PTR (4 * Int2Vec + 2), SEG(SerialEventHandler)    

EnableINT2:    
    MOV DX, INT2_ICR_ADDR   ;We have to enable INT2 interrupt and set its priority level.
    MOV AL, INT2_ICR_VAL
    OUT DX, AL              ;Write value to I2CON Register to set up the INT2 interrupt.

EndInitINT2:
    RET
InitINT2  ENDP    




    
; SetBaudRate
;
; Description:
;    Sets the baud rate of Serial communication for RoboTrike.
; Operation:
;    Before the procedure starts, it saves the value stored in LCR register.
;    Next, it sets the DLAB bit of LCR register to enable access to Divisor Latches of the
;    Baud Generator. Then, it writes the desired baud rate divisor to Divisor Latch by
;    refering to the baud rate table. When setting baud rate is done, it retrieves the
;    original value of LCR register and write in LCR register.
; Arguments:  
;   BaudRateIndex(BX)n - The index used to get the divisor value for desired baud rate from
;                        BaudRateTable.
; Return Value:   
;   None
; Local Variables:   
;   LCRValue(AL) n     - The value to be written to LCR.
;    BaudRateIndex(BX) - The index used to get the divisor value for desired baud rate from
;                        BaudRateTable.
; Shared Variables:  
;    None  
; Global Variables:  
;   None  
; Input:    
;   LCR(Line Control Register)            
; Output:            
;   LCR(Line Control Register) 
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, BX, CX, DX, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation    
  
SetBaudRate   PROC    NEAR
              PUBLIC  SetBaudRate

BaudGetLCRVal:              
    MOV DX, LCR_ADDR        ;We should save the original value of LCR register
    IN AL, DX               ;to prevent other bits being changed.
    MOV CL, AL              ;Save the value in CL so that we can use it when
EnableDLAB:
    PUSHF                   ;Critical code starts. Save flags.
    CLI                     ;Disable interrupts.
    OR AL, ENABLE_DLAB_MASK ;Do the OR instruction with a mask to set DLAB bit of LCR to
                            ;enable access to Divisor Latch.
                            ;1-------: Enables access to Divisor Latch.
    OUT DX, AL              ;Sets DLAB bit.
SetBaudVal:
    SHL BX, MULT_BY_2       ;Double the BaudRateIndex since BaudRateTable is a WORD table.
    MOV DX, DIV_LATCH_ADDR  ;Get the address of Divisor Latch to set baud rate.
    MOV AX, CS:BaudRateTable[BX]  ;Get the divisor value for desired baud rate.
    OUT DX, AL                    ;Writes the divisor value to Divisor Latch to set baud rate

RetrieveOriginalLCR:
    MOV AL, CL                    ;Retrieve the original LCR value.
    AND AL, NOT(ENABLE_DLAB_MASK) ;Disable access to divisor latch by resetting DLAB bit.
                                  ;DLAB bit must be cleared to access receiver buffer,
                                  ;THR, or the IER.
    
    MOV DX, LCR_ADDR              ;Get the address of LCR to set LCR value to its original 
                                  ;value.
    OUT DX, AL                    ;Write the original LCR value to LCR.
    POPF                          ;End of Critical code. Retrieve flags and enable interrupt
    
EndSetBaudRate:
    RET                           ;End of SetBaudRate
SetBaudRate ENDP


; SetParity
;
; Description:
;   Sets the parity of Serial communication for RoboTrike.
; Operation:
;   Before the procedure starts, it saves the value of LCR register.
;   Then, it uses a jump table(ParityTypeTable) to set the desired parity.
;   It uses OR and AND instructions to keep other bits unchanged.
;   Finally, it outputs the value to LCR and exits.
; Arguments:  
;   parityIndex(BX) - Index for desired parity setting. It will be used with
;                     ParityTypeTable to set the desired parity setting.         
; Return Value:   
;   None
; Local Variables:   
;   LCRValue(AL)   - The value to be written to LCR.
;   parity(BX)     - Desired parity setting. It will be used as an index for ParityTypeTable
;                    to set the desired parity setting.   
; Shared Variables:  
;    None  
; Global Variables:  
;   None  
; Input:    
;   LCR(Line Control Register)            
; Output:            
;   LCR(Line Control Register) 
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX, BX, DX, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation   

SetParity   PROC    NEAR
            PUBLIC  SetParity

    PUSHF                           ;Critical code starts. Save all flags
    CLI                             ;Disable Interrupt
SelectParityType:
    MOV DX, LCR_ADDR                ;Save the original value of LCR register.
    IN AL, DX                       ;We do not want to change bits not related to parity.
    JMP CS:ParityTypeTable[BX]      ;Jump to the proper parity setting procedure.
DisableParity:          
    AND AL, NOT(ENABLE_PARITY_MASK) ;disable parity by clearing Parity Enable bit.
    JMP EndSetParityTable           ;Go write the value in LCR.
EnableEvenParity:    
    OR AL, ENABLE_PARITY_MASK       ;Enable parity
    OR AL, EVEN_PARITY_MASK         ;Enable even parity
    JMP EndSetParityTable           ;Go write the value in LCR.
EnableOddParity:
    OR AL, ENABLE_PARITY_MASK       ;Enable parity
    AND AL, NOT(EVEN_PARITY_MASK)   ;Enable odd parity
    JMP EndSetParityTable           ;Go write the value in LCR.
TransmitParityAndClear:
    OR AL, TRANSMIT_CLR_MASK        ;Transmit parity bit and check as cleared.
    JMP EndSetParityTable
TransmitParityAndSet:
    OR AL, TRANSMIT_CLR_MASK        ;bit 3 and bit5 set and bit 4 clear is the condition
    AND AL, TRANSMIT_SET_MASK       ;to transmit parity bit and check as set.
    JMP EndSetParityTable
EnableBreak:
    OR AL, BREAK_CTRL_MASK          ;Force a break condition
    JMP EndSetParityTable
EndSetParityTable:
    OUT DX, AL                      ;Write the configuration value in LCR.
    POPF                            ;Critical code ends.
    RET                             ;End of SetParity procedure.
SetParity ENDP    
    
    


; SerialPutChar
;
; Description:
;    It outputs the passed character to the serial channel although what it actually does
;    is putting the character in TxQueue. It returns with the carry flag reset if the
;    character has been output(put in the TxQueue, not necessarily sent over the serial
;    channel) and set otherwise (TxQueue is full). The character is passed by value in AL.
; Operation:
;    It checks if TxQueue is full by calling Queuefull. If TxQueue is full, set the carry 
;    flag and exit the procedure. If it is not full, the procedure enqueues TxQueue with the
;    given character. Then, it checks if kickstart flag is set. If it is, disable THRE 
;    interrupt and enable THRE interrupt to reactivate THRE interrupt.
;    If Transmitter Empty Interrupt happened but the system could not write
;    the character to THR, the system cannot resolve the Transmitter Empty Interrupt. Thus,
;    THRE interrupt will not be generated unless THRE interrupt gets kickstarted by 
;    "reset interrupt-and-set interrupt" procedure. 
;    When kickstarting THRE interrupt is done,  it resets the kickstart flag.
;    If kickstart flag was not set, there's no extra procedure to be done.
;    Finally, it resets the carry flag and exits.
; Arguments:  
;   Character(AL)     - character to be passed to serial channel.
; Return Value:   
;   Carry Flag        - Set if TxQueue is full.
;                     - Reset if TxQueue is not full and character has been output
;                       (put in the TxQueue)
; Local Variables:   
;    TxQueueAddr(SI)  -  The address of TxQueue
;    IERValue(AL)     -  The value of IER(Interrupt Enable Register)
; Shared Variables:  
;    TxQueue(DS)       -  [R/W] - The queue which contains the characters to be transmitted
;                                 to serial channel.
;    kickstart(DS)     -  [R/W] - Indicates whether kickstart is needed to reactivate 
;                                 THRE interrupt to conitnue data transmission.   
; Global Variables:  
;   None  
; Input:    
;   None            
; Output:            
;   IER(Interrupt Enable Register)
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   None  
; Registers Changed: 
;   AX(If the character has been output), BX, DX, SI, Flags
; Limitations:
;   None       
; Known bugs:        
;   None   
; Special Notes: 
;   None  
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation        
    
SerialPutChar   PROC    NEAR
                PUBLIC  SerialPutChar

CheckTxQueueFull:
    PUSH AX                    ;Save the argument(character) since it should be enqueued
                               ;to TxQueue later.
    MOV SI, OFFSET(TxQueue)    ;Get the address of TxQueue to check if it is full.
    
    CALL QueueFull             ;Check if TxQueue is full by calling QueueFull.
    JNZ EnqueueTx              ;if TxQueue is not full, enqueue the character to TxQueue.
   ;JZ SetCarry                ;If TxQueue is full, set the carry flag.
SetCarry:
    POP AX                     ;Retrieve the character.
    STC                        ;Set the carry flag as a return value.
    JMP EndSerialPutChar       ;Exit the procedure.
EnqueueTx:
    POP AX                     ;Retreive the character.
    PUSHA                      ;Save all registers since calling Enqueue changes registers' values.
    
    CALL Enqueue               ;Enqueue the character to TxQueue
    POPA                       ;Retreive all registers' values.
    CMP  kickstart, KICKSTART_ON  ;Is kickstart flag set?
    JNE  ResetCarry            ;If it is not set, we don't need to kickstart(reactivate)
                               ;THRE interrupt. Thus, go reset the carry flag and exit.
                            
   ;JE   ReActivateTHRE        ;If it is set, we have to kickstart(reactivate) THRE interrupt.
                               ;If Transmitter Empty Interrupt happened but the system could
                               ;not write the character to THR, the system cannot resolve the 
                               ;Transmitter Empty Interrupt. Thus, THRE interrupt will not be 
                               ;generated unless THRE interrupt gets kickstarted by 
                               ;"reset interrupt-and-set interrupt" procedure. 
ReActivateTHRE:
    MOV DX, IER_ADDR                ;We should keep the original value of IER. So,
                                    ;Get the address of IER to obtain its current value.
    IN AL, DX                       ;Get original IER value from IER_ADDR.
    AND AL, DISABLE_THRE_MASK       ;Disable THRE interrupt
    OUT DX,AL                 
    
    OR AL, NOT(DISABLE_THRE_MASK)   ;Enable THRE interrupt
    PUSHF
    CLI
    OUT DX, AL
ClearKickstart:

    MOV kickstart, KICKSTART_OFF    ;Reset kickstart flag since we kickstarted(reactivated)
                                    ;THRE interrupt.
    POPF
ResetCarry:
    CLC                             ;Reset the carry flag as a return value.
EndSerialPutChar:
    RET                             ;End of SerialPutChar procedure.
SerialPutChar ENDP






; SerialEventHandler
;
; Description:
;    Identify the interrupt occured at serial channel and execute appropriate actions for
;    each type of interrupt. SerialEventHandler is called by INT2 interrupt.
; Operation:
;    It reads a value from IIR(Interrupt Identification Register) and refer to the 
;    SerialINTTypeTable. It will go to a label corresponding to the type of interrupt.
;
;    1. Receiver Line Status Interrupt:    A serial error has occured. Read the LSR register
;                                          to reset the interrupt. Enqueue the event by
;                                          calling EnqueueEvent with event type and event 
;                                          value.
;
;    2. Received Data Available Interrupt: Receiver data is available. Read the receiver
;                                          buffer to reset the interrupt. Enqueue the event
;                                          by calling EnqueueEvent with event type and 
;                                          event value.
;
;    3. Transmitter Empty Interrupt:       THR is empty. Check if TxQueue is empty. If it is,
;                                          set the kickstart flag. If it is not empty, 
;                                          dequeue a value from TxQueue and write it in THR.
;
;    4. Modem Status Interrupt:            Read MSR(Modem Status Register)to reset the 
;                                          interrupt.                  
;
;    The program flow should not exit from SerialEventHandler until all serial interrupts 
;    are reset. When all serial interrupts are handled and reset, SerialEventHandler sends
;    SerialEOI and exit.
; Arguments:  
;   None
; Return Value:   
;   None
; Local Variables:   
;    InterruptIdentity(AL)
; Shared Variables:  
;   TxQueue(DS)   -  [Read]  -  The queue which contains the characters to be transmitted
;                               to serial channel.
;   kickstart(DS) -  [Write] -  Indicates whether kickstart is needed to reactivate 
;                               THRE interrupt to conitnue data transmission.       
; Global Variables:  
;   None  
; Input:    
;   IIR(Interrupt Identification Register), LSR(Line Status Register)
;   RBR(Receiver Buffer Register)            
; Output:            
;   THR(Transmitter Empty Register)
; Error Handling:   
;   None  
; Algorithms:        
;   None  
; Data Structures:   
;   Queue(TxQueue)  
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
;     11/16/2016    Sunghoon Choi      Created
;     11/16/2016    Sunghoon Choi      Initial Compilation
;     11/18/2016    Sunghoon Choi      Updated documentation   


SerialEventHandler  PROC    NEAR
                    PUBLIC  SerialEventHandler


    PUSHA                   ;Save all register values since it's an interrupt event handler
InitSerialEventHandler:                    
    
    XOR AX, AX              ;Clear AX since we will move AX's value to BX and use it as an
                            ;index for SerialINTTypeTable.

    MOV DX, IIR_ADDR        ;Read a value from IIR(Interrupt Identification Register) to
    IN AL, DX                ;identify current serial interrupt.
                            ;-----001: No Interrupt
                            ;-----110: Priority 1, Receiver line status interrupt                
                            ;-----100: Priority 2, Received data available interrupt
                            ;-----010: Priority 3, Transmitter holding register empty
                            ;                       interrupt
                            ;-----000: Priority 4, Modem status interrupt         
    CMP AL, NO_INTERRUPT    ;Was there no interrupt?
    JE SendSerialEOI        ;There was no interrupt. Go send SerialEOI to announce the end 
                            ;of SerialEventHandler
    ;JNE JumpINTTypeHandler ;There was an interrupt. Go handle each type of interrupt.

JumpINTTypeHandler:                     
    MOV BX, AX                    ;Move interrupt type into BX to use it as an index for
                                  ;jump table.
    JMP CS:SerialINTTypeTable[BX] ;Go to the corresponding interrupt handling routine of
                                  ;current interrupt.
                                  
ReceiverLineStatusInterrupt:      ;Interrupt: Overrun error, parity error, framing error
                                  ;or break interrupt has occured.
    MOV DX, LSR_ADDR              ;Get the address of LSR register and
    IN AL, DX                     ;read the value of LSR to identify the error type.
    
    AND AL, ERROR_MASK            ;Extract the error type from LCR value by using ERROR_MASK
    MOV AH, SERIAL_ERROR_EVENT    ;Save the event type(SERIAL_ERROR_EVENT) in AH
    CALL EnqueueEvent             ;Enqueue the event value(error type) and 
                                  ;event type(error event) to EventBuf.
    JMP InitSerialEventHandler    ;Since we reset Receiver Line Status Interrupt,
                                  ;go back and check if there's any other interrupts.
    
ReceivedDataAvailInterrupt:       ;Interrupt: Receiver data is available.
    MOV DX, RBR_ADDR              ;Read the receiver buffer register to reset
    IN AL, DX                     ;the Received Data Available Interrupt.
    MOV AH, SERIAL_RECEIVED_EVENT ;AL = Read data(Event value)
                                  ;AH = SERIAL_RECEIVED_EVENT(Event type)
                                  ;These two arguments(event value and event type) will be
                                  ;used to enqueue the event to EventBuf.
    CALL EnqueueEvent             ;Enqueue the event value and event type to EventBuf
    JMP InitSerialEventHandler    ;Since we reset Received Data Available Interrupt,
                                  ;go back and check if there's any other interrupts.

TransmitterEmptyInterrupt:        ;Interrupt: Transmitter holding register is empty.
    MOV SI, OFFSET(TxQueue)       ;Get the address of TxQueue to dequeue a value from it
                                  ;and fill it in THR(Transmitter Holding Register)
    
    PUSHF                         ;Critical code starts. Save all flags.
    CLI                           ;Disable interrupts
    
    CALL QueueEmpty               ;Check if TxQueue is empty.
    JNZ WriteTHR                  ;If TxQueue is not empty, go take a value from it
                                  ;and write it in THR.
    ;JZ SetKickstart              ;If TxQueue is empty, we cannot write any value in THR.
                                  ;So now, although Transmitter Empty Interrupt has occured,
                                  ;no value can be written in THR. Thus, Transmitter Empty
                                  ;Interrupt will not be generated unless we kickstart it
                                  ;or reactivate it. Thus, go set the kickstart flag.
SetKickStart:
    MOV kickstart, KICKSTART_ON   ;Set the kickstart flag to reactivate THRE interrupt.
    
    POPF                          ;Critical code ends. Retrieve all flags and enable
                                  ;interrupts.
    
    JMP InitSerialEventHandler    ;Since we reset the Transmitter Empty Interrupt,
                                  ;go back and check if there's any other interrupts.

WriteTHR:
    POPF                          ;Critical code ends. Retrieve all flags and enable
                                  ;interrupts.
    CALL Dequeue                  ;Dequeue a character from TxQueue to write it in THR.
    MOV DX, THR_ADDR              ;Get the address of THR to write a character.
    OUT DX, AL                    ;Write the dequeued character in THR.
    JMP InitSerialEventHandler    ;Since we reset the Transmitter Empty Interrupt,
                                  ;go back and check if there's any other interrupts.

ModemStatusInterrupt:             ;Interrupt: Modem Status Interrupt
    MOV DX, MSR_ADDR              ;Read modem status register to reset the
    IN  AL, DX                    ;Modem Status Interrupt.            
    JMP InitSerialEventHandler    ;Since we reset the Modem  Interrupt,
                                  ;go back and check if there's any other interrupts.              
   
SendSerialEOI:    
   MOV DX, INTCtrlrEOI            ;Send SerialEOI to announce the end of SerialEventHandler
   MOV AL, SerialEOI              
   OUT DX, AL           
   
EndSerialEventHandler:   
   POPA                            ;Retrieve all original register values
   IRET                            ;End of SerialEventHandler
   
SerialEventHandler ENDP   
    

; SerialPutString
;
; Description:
;    It sends a string to the serial channel although it actually does is storing a string in
;    TxQueue by repeatedly calling SerialPutChar.
; Operation:
;    It gets passed the address of the string to be sent(through the serial channel) in SI.
;    It checks every character that belongs to the string.
;    First, it checks if the character is NULL. If it is NULL, it sends Carriage Return by 
;    calling SerialPutChar after setting AL to CarriageReturn. If it is not NULL, send the 
;    character by calling SerialPutChar. Then, it checks if TxQueue is full by checking the
;    carry flag which is returned from SerialPutChar.  If the carry flag is set, exit 
;    SerialPutString. If the carry flag is not set, increment the index of the character(in 
;    the string) and go back to fetch the next character.
; Arguments:  
;   SI(StringAddress)    -    The address of the string to be sent.
; Return Value:   
;   None
; Local Variables:   
;    Character(AL)       -    The current character to be enqueued to TxQueue.
;    CharAddrees(SI)     -    The address of the character to be sent.
;                             It starts from the starting address of the string but gets 
;                             incremented every loop to handle all characters.
; Shared Variables:  
;   
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
    
    
SerialPutString     PROC     NEAR
                    PUBLIC  SerialPutString
                    
   
FetchCharacter:
    MOV AL, ES:[SI]         ;fetch a character from the source string address.
    CMP AL, 0               ;Is the character NULL?
    JNE SendChar            ;No, send the character
    ;JE SendCarriageReturn  ;Yes, send carriage return
SendCarriageReturn:
    MOV AL, 13              ;Let's send Carriage Return. Insert CR in AL.
    PUSH SI                 ;Save the address of the current character.
    CALL SerialPutChar      ;Call SerialPutChar to store Carriage Return in TxQueue.
    POP SI                  ;Retrieve the address of the current character to conitnue
                            ;fetching characters.
    JMP EndSerialPutString  ;Since we've sent Carriage Return, SerialPutString ends.
SendChar:
    PUSH SI                 ;Save the address of the current character.
    CALL SerialPutChar      ;Call SerialPutChar to store the character in TxQueue.
    POP SI                  ;Retrieve the address of the current character to continue
                            ;fetcing characters.
FindTxQueueFull:
    JC  EndSerialPutString  ;Check if TxQueue is full. If it is, exit the process.
   ;JNC SendNextChar        ;Since TxQueue is not full, proceed to send the next character.
SendNextChar:
    INC SI                  ;increment the index to fetch the next character in the string.
    JMP FetchCharacter      ;repeat fetching characters until it gets a Carriage Return
                            ;or the TxQueue is full.
EndSerialPutString:
    RET                     ;End of SerialPutString
SerialPutString ENDP

   
    
CODE ENDS    
    

                    

    
DATA SEGMENT PUBLIC 'DATA'
    TxQueue     QueueModule     <>  ;The queue which contains the characters to be 
                                    ;transmitted to serial channel.
    kickstart   DB              ?   ;Indicates whether kickstart is needed to reactivate 
                                    ;THRE interrupt to conitnue data transmission. 
DATA ENDS

END