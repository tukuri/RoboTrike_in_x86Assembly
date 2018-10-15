NAME Keypad

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Keypad.asm                                 ;
;                                 Homework5                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description:
;     This file contains the functions necessary for handling keypad inputs.
;
; Table of Contents:
;    InitKeypad          - Initializes the necessary buffers and constants for
;                          Keypad routine.
;    KeypadEventHandler  - Reads keys from the keypad and enqueues the key event
;                          to EventBuf.
;                          It is called by Timer2 at every 1ms.
;
;
; Revision History:
;    10/30/2016     Sunghoon Choi         Created
;    10/31/2016     Sunghoon Choi         Initial Compilation
;    11/3/2016      Sunghoon Choi         Revised Comments
;    11/3/2016      Sunghoon Choi         Revised the DS value from DATA to DGROUP



$INCLUDE(Keypad.inc)        ;Include the .inc file which contains constants for keypad.asm
$INCLUDE(Events.inc)        ;Include the .inc file which contains the list of events for
                            ;Robotrike system.


EXTRN EnqueueEvent:NEAR     ;import EnqueueEvent for enqueueing the keypad events
                            ;to EventBuf

CGROUP GROUP CODE        
DGROUP GROUP DATA
CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP
       

       
; InitKeypad
;
; Description:
;        Initializes the KeyDebounceCounter array with KEY_COUNTER_MAX          
; Operation:  
;        It inserts KEY_COUNTER_MAX to each element of KeyDebounceCounter array by
;        incrementing the array index every loop. It repeats the loop until
;        the index reaches NUM_KEYS.
; Arguments:  
;      None
; Return Value:   
;       None
; Local Variables:   
;      KeyArrayIndex(DI)   -  The array index used for initializing KeyDebounceCounter array.
; Shared Variables:  
;       KeyDebounceCounter - [Write] -  An array of size NUM_KEYS which contains the
;                                       debounce counters for NUM_KEYS keys. Elements of
;                                       the array has the counter for each key.
; Global Variables:  
;       None
; Input:    
;       None         
; Output:            
;       None
; Error Handling:   
;       None
; Algorithms:        
;       None
; Data Structures:   
;       None
; Registers Changed: 
;       DI, Flags
; Limitations:
;       None     
; Known bugs:        
;       None 
; Special Notes: 
;       None
; Author:           
;       Sunghoon Choi
; Revision History:
;        10/30/2016   Sunghoon Choi    Created
;        10/31/2016   Sunghoon Choi    Initial Compilation
;        11/3/2016    Sunghoon Choi    Revised Comments
   
InitKeypad  PROC    NEAR
            PUBLIC  InitKeypad    

InitKeypadIndex:
    MOV DI, FIRST_KEY        ;We begin initializing the array from its first element
    
InitKeyCounter:
    MOV KeyDebounceCounter[DI], KEY_COUNTER_MAX ;Initialize the KeyDebounceCounter array
                                                ;to the state of no buttons having been 
                                                ;pressed. Thus, each counter will have
                                                ;its maximum value.
    INC DI                                      ;Go to the next element for initialization
    CMP DI, NUM_KEYS                            ;Is this the end of this array?
    JB InitKeyCounter                           ;No, continue initialization.
;    JGE EndInitKeypad                          ;Yes, finish initialization.
EndInitKeypad:
     RET                                        ;End of InitKeypad procedure.
     
InitKeypad  ENDP     
    

    
    
; KeypadEventHandler
;
; Description:       
;        This eventhandler reads values from the keypad and debounces the current key if it
;        is pressed. It sets a counter for each key. The counter indicates the number of 
;        interrupts(KeypadEventHandler) the system need to call for enqueuing a key pressed
;        event. If a button is pressed, the counter will be decremented at every interrupt
;        call(1 ms) and when the counter reaches zero, the key-pressed event will be enqueued
;        and the counter will be set to Auto-Repeat counter value.
;        If a certain key was found to be not pressed, it will reset the counter to its
;        maximum value. KeypadEventHandler is called by Timer2EventHandler every 1ms.
; Operation:  
;        It receives a value from the current keypad row.Our keypad has NUM_ROWS rows and each 
;        row has KEYS_IN_ROW keys. Although the value read from the row is originally 
;        BITS_IN_BYTE bits, it extracts only the KEYS_IN_ROW bits of current key row by using
;        mask bits to remove the high (BITS_IN_BYTE - KEYS_IN_ROW) bits of AL. 
;        Next, it checks if the first key of the current row is pressed by using a column mask
;        If it is not pressed, reset the key's debounce counter to the original maximum value. 
;        If the key is pressed, decrement the debounce counter of the current key. 
;        The debounce counter is stored in an array of NUM_KEYS elements. Each element 
;        contains the counter for each key. 
;        Now, if the debounce counter of the current key has reached zero, it calls 
;        EnqueueEvent with the arguments of KEYPAD_EVENT and the current key's identification 
;        number. Also, the debounce counter for the pressed key will be set to AUTO_REPEAT
;        Counter value. After a key is checked(whether debounced or reset), it checks the next 
;        key by shifting the column mask to left. When it is done with checking the 
;        current row, it goes to the next row by incrementing the Keypad address since 
;        the address of each row is separated by one byte.
;        After incrementing the keypad address, it goes back to check the first key of the row
;        by retreiving and using the initial column mask.
;        The eventhandler ends when it has gone through all NUM_KEYS keys
;        (until the last key of NUM_ROWth row)     
; Arguments:  
;      None
; Return Value:   
;       None
; Local Variables:   
;      RowAddr(DX)            -    The address of the current key row
;       CurrentRowValue(AL)   -    The values read from the RowAddr
;       CurrentKeyValue(AL)   -    The value of the current key
;       KeypadColMask(BL)     -    A mask used for selecting the current key inside the current row.                              
;       KeyIndex(DI)          -    The index for current key(=column index of the key)
; Shared Variables:  
;        KeyDebounceCounter   - [R/W] - An array of size NUM_KEYS which contains the
;                                       debounce counters for NUM_KEYS keys. 
;                                       Elements of the array has the counter for each key.
; Global Variables:  
;       None
; Input:    
;       Keypad of NUM_KEYS keys.          
; Output:            
;       None
; Error Handling:   
;       None
; Algorithms:        
;       None
; Data Structures:   
;       None
; Registers Changed: 
;       AX, BX, CX, DI, Flags
; Limitations:
;       None     
; Known bugs:        
;       None 
; Special Notes: 
;       None
; Author:           
;       Sunghoon Choi
; Revision History:
;        10/30/2016   Sunghoon Choi    Created
;        10/31/2016   Sunghoon Choi    Initial Compilation
;        11/3/2016    Sunghoon Choi    Revised comments

    
KeypadEventHandler  PROC    NEAR
                    PUBLIC  KeypadEventHandler


InitKeyRowAndCol:                    
    MOV DX, KEY_FIRST_ROW_ADDR      ;The KeypadEventHandler scans the keypad row by row.
                                    ;First, it scans the first row.
    MOV BL, KEYPAD_COLUMN_MASK      ;Column mask will be used to indicate a specific key
                                    ;inside a row.
    XOR DI, DI                      ;The eventhandler will scan the keypad and each key has
                                    ;its own index. The eventhandler handles the first key
                                    ;(For an array, index 0 is the first element) at the
                                    ;beginning.

GetKeyRowVal:
    IN AL, DX                       ;Read a byte from the current row.
    AND AL, KEYPAD_ROW_MASK         ;Remove unnecessary high bits and leave only the
                                    ;bits of the current row.
    MOV CL, AL                      ;Need to save the value of current row since 
                                    ;AL register will be altered by the loop beginning with
                                    ;GetAKeyVal.
GetAKeyVal:
    MOV AL, CL                      ;Retrieve the bits of current row to continue 
                                    ;handling the current row.
    AND AL, BL                      ;Get a specific key inside current row by using
                                    ;the column mask.
    CMP AL, KEY_PRESSED             ;Is the key pressed?
    JE StartDebounce                ;Yes, the key is pressed. Start deboucing
    ;JNE ResetCounter               ;No, the key is not pressed. Reset the debounce counter.
ResetCounter:    
    MOV KeyDebounceCounter[DI], KEY_COUNTER_MAX ;The key is not pressed. So we need to 
                                                ;reset the counter and start the debouncing
                                                ;from the very beginning again.
    JMP GetNextKey                              ;Done with handling an unpressed key.
                                                ;So go to handle the next key.
    
StartDebounce:
    DEC KeyDebounceCounter[DI]                  ;Since the key was pressed, we need to
                                                ;decrement the debounce counter of the
                                                ;current key. The key will be considered
                                                ;"pressed" when the counter reaches zero.
    JNZ GetNextKey                              ;If the debounce counter has not reached zero 
                                                ;yet, do not enqueue the key event and go 
                                                ;check the next key.
                                                
    ;JZ    EnqueueKeyEvent                      ;The debounce counter has reached zero.
                                                ;Thus, we should consider this key "pressed"
                                                ;and enqueue this event to EventBuf.
EnqueueKeyEvent:
    MOV KeyDebounceCounter[DI], REPEAT_COUNTER  ;Since we confirmed that the current key is
                                                ;pressed, we need to check if the key is
                                                ;pressed consistently. If the key is pressed
                                                ;for REPEAT_COUNTER ms, it treats the key
                                                ;as being pressed again and enqueue the 
                                                ;event again.
    MOV AX, DI                                  ;AL is an argument for EnqueueEvent.
                                                ;It contains the key index, or key value.
    MOV AH, KEY_EVENT                           ;AH is also an argument for EnqueueEvent.
                                                ;It contains the KEY_EVENT.
    
    PUSHA                                       ;save all general-purpose registers since
                                                ;calling EnqueueEvent may change the values
                                                ;of them.
    CALL EnqueueEvent                           ;Enqueue the event that the current key has
                                                ;been pressed to EventBuf.
    POPA                                        ;Retrieve all general-purpose registers
                                                ;since the program has returned from 
                                                ;EnqueueEvent procedure.

GetNextKey:
    INC DI                                      ;Get ready to save the next key's 
                                                ;debounce counter in the next element of
                                                ;KeyDebounceCounter array(If pressed).
    SHL BL, COL_INTERVAL                        ;We have to check the next key in the 
                                                ;current row. So shift the column index 
                                                ;to left to get the key of next column of
                                                ;same row.
                            
    AND BL, KEYPAD_ROW_MASK                     ;Limit the column index to the number of
                                                ;keys in a row. The column index will 
                                                ;become zero if it exceeds the number of
                                                ;keys in a row.
    CMP BL, COL_OVER_LIMIT                      ;Was the key handled on the last loop 
                                                ;the last key of current row?
    JNE GetAKeyVal                              ;No. There's still keys to be checked in
                                                ;the current row. Go handle the next key
                                                ;in the same row.
;   JE  GoNextRow                               ;Yes, we're done with this row.
                                                ;Go handle the next row.
GoNextRow:
    INC DX                                      ;Get the next keypad row's address
                                                ;to read from next row.
    MOV BL, KEYPAD_COLUMN_MASK                  ;Since we are going to the next row,
                                                ;We should begin checking from the first 
                                                ;column again.
CheckLastRow:
    CMP DX,  KEY_LAST_ROW_ADDR                  ;Was the previous loop hnadling the last row? 
    JBE GetKeyRowVal                            ;No. Go read the next row.
    ;JE EndKeypadEventHandler                   ;Yes. We've handled all rows and all keys.

EndKeypadEventHandler:
    RET                                         ;Since we've handled all keys, 
                                                ;finish the KeypadEventHandler.
KeypadEventHandler ENDP
    

                              

    
CODE ENDS            



DATA    SEGMENT PUBLIC  'DATA'
      KeyDebounceCounter    DB  NUM_KEYS    DUP (?)    ;KeyDebounceCounter contains
                                                    ;the debounce counters for each keys.
DATA    ENDS            



END