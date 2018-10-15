NAME QUEUE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Queue.asm                                  ;
;                                 Homework3                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Description:
;        This file contains a number of functions to initialize, enqueue, and 
;        dequeue the queue. Also, it contains functions for checking the status 
;        of the queue.

; Table of Contents: 
;    QueueInit(a,l,s) - Initialize the queue, HeadIndex, CurrentElemNum, ElemSize
;    QueueEmpty(a)    - Checks if the queue is empty. Sets the zero flag if the 
;                       queue is empty and resets the zero flag otherwise.
;    QueueFull(a)     - Checks if the queue is full. Sets the zero flag if the queue 
;                       is full. Resets the zero flag otherwise.
;    Enqueue(a,v)     - Waits until there's a slot in the queue and add a byte or
;                       a word
;    Dequeue(a)       - Waits until there's a value to take from the queue and
;                       return a byte or a word.                
;
;
; Revision History:
;        10/19/2016    Sunghoon Choi  Started writing functional specifications
;        10/20/2016    Sunghoon Choi  Initial Compilation
;        10/20/2016    Sunghoon Choi  Corrected Stack Pointer error
;        10/21/2016    Sunghoon Choi  Corrected Head Index derivation error
;        10/21/2016    Sunghoon Choi  Updated documentations
;        10/31/2016    Sunghoon Choi  Revised the length initialization
;        11/31/2016    Sunghoon Choi  Changed to fixed length queue.


$INCLUDE(Queue.inc)      ;include file for queue structure and related constants
$INCLUDE(general.inc)    ;include file for general constants 

CGROUP GROUP CODE


CODE SEGMENT PUBLIC 'CODE'
    
     ASSUME CS:CGROUP 

          

; QueueInit
;
; Description:       
;            Initializes the queue of element size(s) at the passed address(a). 
;            After calling this procedure, the queue must
;            be empty and ready to accept the values. It initializes the 
;            head index of the queue to zero. Note that the element 
;            size(s) specifies whether each entry in queue is a byte or a word. 
;            If s is TRUE(Non-zero), the entries are words.
;            If s is FALSE(EQU 0), the entries are bytes. The address(a) is passed
;            in by SI. It means that the queue starts at DS:SI. The element 
;            size(s) is passed from BL. The length of the queue is fixed to
;            MAX_QUEUE_SIZE(EQU 2^8) bytes.
;            
;
; Operation:  Initialize the HeadIndex to the starting location of the queue.
;             Initialize the CurrentElemNum to zero since there's no elements
;             when a queue is just created.
;             Check if the arguemtn BL, the size of the queue's element, is
;             equal to WORD_SIZE(EQU 2) or BYTE_SIZE(EQU 1). Set the elemNum
;             to WORD_SIZE if BL is WORD_SIZE, and sets the elemNum to BYTE_SIZE
;             if BL is BYTE_SIZE.
;                     
; Arguments:  SI - address of the queue
;             BL - size of the queue's element.   
;
; Return Value:    None  
;
; Local Variables:   a  -  (SI)  -  address of the queue
;                    s  -  (BL)  -  size of the queue's element
;                        (Non-zero(TRUE) value means the elements are words)
;                        (0(FALSE) value means the elements are bytes)
; 
; Shared Variables:  
;    QueueModule                - (SI) - [Write] - The structure for the queue module
;    QueueModule.HeadIndex      - (SI) - [Write] - Current Head Index of the queue 
;    QueueModule.CurrentElemNum - (SI) - [Write] - Current number of elemnts in the queue.
;                                                                                                        
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    None
;
; Algorithms:        None
;
; Data Structures:   Queue (MyQueue struct in Queue.inc)
;
; Registers Changed: Zero Flag
;
; Limitations:       The queue can only have a length of a power of 2.
; Known bugs:        None 
; Special Notes:     None


; Author:            Sunghoon Choi
; Revision History:
;        10/14/2016  17:00 Sunghoon Choi Started writing functional specifications
;        10/14/2016  18:00 Sunghoon Choi initial compilation
;        10/14/2016  21:00 Sunghoon Choi Corrected stack pointer error
;        10/15/2016  04:00 Sunghoon Choi updated documentation
;        10/31/2016  10:13 Sunghoon Choi revised the length initialization
;        11/15/2016  21:00 Sunghoon Choi changed CMP BL, TRUE to CMP BL, FALSE
                         
         
QueueInit       PROC    NEAR
                PUBLIC QueueInit


InitializeQueue:                    
    MOV [SI].HeadIndex, QUEUE_INIT_CON            
                                    ;when queue is created, the Head Index and
                                    ;tail index are on the very front.
                                    ;tail index is not included in the 
                                    ;queueModule as an element since it can 
                                    ;be calculated by
                                    ;(HeadIndex+Num of Elements) mod length                               
                                    
                                    
    MOV [SI].CurrentElemNum, QUEUE_INIT_CON
                                    ;When queue is created, there's no element
                                    ;in the queue.
                                    
    CMP BL, FALSE                   ;Check if the queue to be initialized is
                                    ;a byte queue or a word queue
                                    
    JNZ InitWordQueue               ;if the queue is a word queue, go set
                                    ;the size of element to the size of a word
                                    ;in bytes, which is WORD_SIZE(EQU 2).
    ;JZ InitByteQueue               ;if the queue is a byte queue, go set
                                    ;the size of element to the size of a byte
                                    ;in bytes, which is BYTE_SIZE(EQU 1).

InitByteQueue:
    MOV [SI].ElemSize, BYTE_SIZE    ;initialize the ElemSize to the size of
                                    ;a byte in bytes, which is 
                                    ;BYTE_SIZE(EQU 1)
                                        
    MOV AX, 256                            
    MOV [SI].len, AX 
    
    JMP EndQueueInit

InitWordQueue:
    MOV [SI].ElemSize, WORD_SIZE    ;Set the element's size to the size of
                                    ;a word in bytes, which is 
                                    ;WORD_SIZE(EQU 2)

    MOV AX, 256
    MOV [SI].len, AX                                      
        
EndQueueInit:                       ;finish the intialization.
    
    RET

QueueInit ENDP


; QueueEmpty
;
; Description:      
;        Checks if the queue is empty. It sets zero flag if the queue is empty.
;        It resets zero flag if queue is not empty. The address (a) of the queue
;        is passed by SI.
;            
; Operation:  Check if QueueModule.CurrentElemNum is zero. If so, set ZF.
;             Otherwise, reset ZF.
;                     
; Arguments:  SI - address of the queue
;              
; Return Value: Zero Flag - set if the queue is empty
;                         - reset if the queue is not empty.  
;
; Local Variables: a  -  (SI) -  address of the queue
; 
; Shared Variables:  
;    QueueModule                - (SI) - [Read] - The structure for the queue module
;    QueueModule.CurrentElemNum - (SI) - [Read] - Current number of elemnts in the 
;                                                 queue.
;                                                        
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    None
;
; Algorithms:        None
;
; Data Structures:   Queue    (MyQueue struct in Queue.inc)
;
; Registers Changed: Zero Flag
;
; Limitations:       The queue can only have a length of a power of 2.
; Known bugs:        None 
; Special Notes:     None


; Author:            Sunghoon Choi
; Revision History:
;        10/14/2016   Sunghoon Choi Started writing functional specifications
;        10/14/2016   Sunghoon Choi initial compilation
;        10/14/2016   Sunghoon Choi Corrected stack pointer error
;        10/15/2016   Sunghoon Choi updated documentation



QueueEmpty      PROC    NEAR        
                PUBLIC    QueueEmpty
            
CheckEmpty:
    CMP [SI].CurrentElemNum, QUEUE_INIT_CON    
                                        ;check if the current number elements 
                                        ;in the queue is zero.
                                        ;if so, set zero flag.
                                        ;Otherwise, reset zero flag.

EndQueueEmpty:
    
    RET
    
QueueEmpty ENDP


; QueueFull
;
; Description:      
;        Checks if the queue is full. If the queue is full, set the zero flag.
;        Otherwise, reset the zero flag.
;            
; Operation:  
;        Compare the length of the queue and current number of elements.
;        If they are equal, it means the queue is full. So, set ZF.
;        If they are not equal, it means the queue is not full. So, reset ZF.
;                     
; Arguments:  SI - address of the queue
;              
; Return Value:  Zero Flag - set if the queue is full
;                          - reset if the queue is not full.  
;
; Local Variables:   a            -  (SI)  -  address of the queue
;                    currElemNumb -  (AX)   - Current Number of elements in the queue
; 
; Shared Variables:  
;    QueueModule               - (SI) - [Read] - The structure for the queue module
;
;   QueueModule.CurrentElemNum - (SI) - [Read] - Current number of elemnts in the 
;                                                queue.
;   QueueModule.len            - (SI) - [Read] - The length of the queue
;                                                        
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    None
;
; Algorithms:        None
;
; Data Structures:   Queue    (MyQueue struct in Queue.inc)
;
; Registers Changed: AX, ZF
;
; Limitations:       The queue can only have a length of a power of 2.
; Known bugs:        None 
; Special Notes:     None


; Author:           Sunghoon Choi
; Revision History:
;        10/14/2016  Sunghoon Choi Started writing functional specifications
;        10/14/2016  Sunghoon Choi initial compilation
;        10/14/2016  Sunghoon Choi Corrected stack pointer error
;        10/15/2016  Sunghoon Choi updated documentation


QueueFull   PROC    NEAR
            PUBLIC    QueueFull
    
CheckFull:
    MOV AX, [SI].CurrentElemNum     ;AX <- Current number of elements in the 
                                    ;queue
    CMP AX, [SI].len                ;current number of elements in the 
                                    ;queue is being equal to the queue's length
                                    ;means that the queue is full.
                                    ;If queue is full, set zero flag. 
                                    ;otherwise, reset zero flag.
EndQueueFull:
        
        RET
QueueFull ENDP        





; Dequeue
;
; Description:
;       The function removes a byte(80bit) or a word(16-bit) from the head of 
;       queue and returns it in AL(if it's a byte) or AX(if it's a word). If the 
;       queue is empty, it blocks until the queue receives a value to be removed
;       and returned. It will not return until a value is taken from the queue.
;       Note that the address of the queue is passed by SI.
;       
; Operation:
;       It first checks if the queue is empty by calling QueueEmpty.
;       If the queue is empty, repeat calling QueueEmtpy.
;       If the queue is not empty, go check the size of the queue.
;       If the queue is a byte queue, go take a byte from the queue.
;       The function takes a byte value from the location of head index in the 
;       queue. 
;       If the queue is a word queue, it must first translate the head index 
;       into the head index of word version. It multiplies the HeadIndex by
;       WORD_SIZE(EQU 2) and take a modulus of length for wrapping.
;       Now that we have the HeadIndex for word-version, we take the word value
;       from the location of the translated Head Index.   
;       Once the function is done with taking a value from the queue, it
;       updates the HeadIndex. It adds BYTE_VALUE(EQU 1) to the HeadIndex
;       and take modulus of length for wrapping to get the new Head_ndex.
;       Update the QueueModule's HeadIndex to our new HeadINdex.
;       Finally, it decrements the current number of elements in the Queue by
;       the size of the queue's element. If the queue is a word queue, it 
;       decrements the number of elements by WORD_SIZE(EQU 2). If the queue
;       is a byte queue, it decrements the number of elements by BYTE_SIZE.
;        
;                     
; Arguments:      SI - address of the queue
;              
; Return Value:   AL - a dequeued value (If byte)
;                 AX - a dequeued value (If word)
;
; Local Variables:  HeadIndex_WordVer - BX     - The HeadIndex which is translated  
;                                                for a word queue
;                   UpdatedHeadIndex -  DX     - The new HeadIndex after dequeue operation.
;                   a                -  SI     - The address of the queue
;                   DeqValue         -  AL(AX) - The dequeued value to be returned.
; 
; Shared Variables:  
;   QueueModule                - (SI) - [R/W]  - The structure for the queue module
;   QueueModule.ElemSize       - (SI) - [Read] - The size of the queue's entries
;   QueueModule.CurrentElemNum - (SI) - [W]    - Current number of elemnts in the 
;                                                queue.
;   QueueModule.len            - (SI) - [Read] - The length of the queue
;   QueueModule.HeadIndex      - (SI) - [R/W]  - The head index of the queue.
;
                                             
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    If the queue is empty, stay in the loop and wait.
;
; Algorithms:        None
;
; Data Structures:   Queue (MyQueue struct in Queue.inc)
;
; Registers Changed: AX, BX, CX, DX, Zero Flag
;
; Limitations:       The queue can only have a length of a power of 2.
; Known bugs:        None 
; Special Notes:     None


; Author:            Sunghoon Choi
; Revision History:
;        10/14/2016  Sunghoon Choi Started writing functional specifications
;        10/14/2016  Sunghoon Choi initial compilation
;        10/15/2016  Sunghoon Choi updated documentation




Dequeue     PROC     NEAR
            PUBLIC Dequeue
    
CheckQueueEmpty:            ;checks if the queue is empty
    CALL QueueEmpty         ;call QueueEmpty function to check if the queue
                            ;is empty
    JZ CheckQueueEmpty      ;if the queue is empty, wait until the queue 
                            ;has a value to be taken. It's a blocking function
    ;JNZ DeqCheckElemSize   ;If the queue is not empty, go check the queue's 
                            ;type
                            
DeqCheckElemSize:
    MOV AL, BYTE_SIZE       ;Checks if the queue is a byte queue or a word queue
    CMP [SI].ElemSize, AL   ;Is the element size BYTE_SIZE(EQU 1)?
    JE GetByteValue         ;Yes. Let's go take a byte value
    ;JNE GetWordValue       ;No. Let's go take a word value
    
GetWordValue:
    XOR AX, AX              ;We are going to insert HeadIndex value in AL.
                            ;Thus, for division operation, we need to clear
                            ;the high bits (AH)
    XOR BX, BX              ;Clear BX to prevent unexpected error, just in case
    MOV AL, [SI].HeadIndex  ;Save HeadIndex value in AL for division operation
    SHL AX,  MULT_BY_2      ;We have to double the HeadIndex since
                            ;it's a word queue. The head index should move 
                            ;with a step of WORD_SIZE(EQU 2), since it's a word
                            ;queue.
    MOV BX, [SI].len        ;Save the length of the queue in BX for division.
                            ;Since the length is in a word, we have to save it
                            ;in BX, not BL.
    XOR DX,DX               ;We have to clear DX because when the divisor size 
                            ;is 2 byte, the dividend is DX:AX.
    DIV BX                  ;HeadIndex for Word = WORD_SIZE*HeadIndex mod length
                            ; "mod length" is needed since multiplying the 
                            ;Head Index may lead to exceeding the maximum 
                            ;array index. 
                            ;The head index for word is stored in DX.
    MOV BX, DX              ;move the head index for word to BX to access array.
    MOV AX, WORD PTR [SI].Queue[BX]    ;Head's location = OFFSET of QueueModule+ 
                                       ;Offset of queue + new head index
                                       ;We take a word from head's location
                                        
    
    PUSH AX                            ;Save the dequeued word value since we have to
                                       ;use AX register for updating the head index.
    JMP UpdateHeadIndex
    
    
GetByteValue:
    XOR BX,BX                    ;Since we are going to store HeadIndex in BL, we have
                                 ;to clear the high bits(BH) for future array accessing.
    MOV BL, [SI].HeadIndex       ;Store the Head Index in BL as an array pointer.

    MOV AL, BYTE PTR [SI].Queue[BX]    ;Head's location = OFFSET of QueueModule+ 
                                       ;Offset of queue + head index
                                       ;We take a byte from head's location
    PUSH AX                      ;save the dequeued value since we are going to 
                                 ;use AX for updating head index.
    
UpdateHeadIndex:
    XOR AX, AX                  ;Since we are going to store HeadIndex in AL,
                                ;we clear the high bits(AH).
    MOV AL, [SI].HeadIndex      ;AL is the current HeadIndex
    ADD AL, BYTE_SIZE           ;New HeadIndex = HeadIndex+BYTE_SIZE mod length
    XOR DX, DX                  ;Clear DX since the dividend for 2byte division
                                ;is DX:AX.
    MOV BX, [SI].len            ;BX = length of the queue.
    DIV BX                      ;DX = HeadIndex + BYTE_SIZE mod length 
                                ;= new HeadIndex
    MOV [SI].HeadIndex, DL      ;Update the HeadIndex with the new HeadIndex      
    POP AX                      ;retrieve the dequeued value
DecCurrentElemNum:
    XOR DX, DX                  ;clear the high bits since we are going to 
                                ;store ElemSize in DL.
    MOV DL, [SI].ElemSize       ;The Current Element Number should be decreased
                                ;by WORD_SIZE if the queue is a word queue and
                                ;should be decreased by BYTE_SIZE if the queue
                                ;is a byte queue.
    SUB [SI].CurrentElemNum, DX ;Decrease the current element number
                                ;by BYTE_SIZE if the queue is a byte queue.
                                ;Decrease the current element number by 
                                ;WORD_SIZE if the queue is a word queue.
                                
EndDequeue:
    
    RET
    
Dequeue ENDP




; Enqueue
;
; Description:
;       The function adds the passed byte or word to the tail of the queue.
;       If the queue is full, it waits until the queue has a slot to add the
;       value. It does not return until it adds the value to the queue. Note
;       that the address of the queue is passed by SI while the value to add
;       is passed by AL(if the queue's entries are bytes) or AX(if the queue's
;       entries are words.
;       
; Operation:
;       The function checks if the queue is full. If the queue is full,
;       stay in the loop and repeatedly call QueueFull until there is a slot
;       to enqueue.
;       If the queue is not full, it checks the size of the value to be added.
;       If it's a byte, go calculate the tail index of the queue.
;       The tail index is calculated by TailIndex=HeadIndex+CurrentElemNum mod
;       length for wrapping. Then, it adds the byte value to the tail of the
;       queue. If the arguemnt is a word, go calculate the tail index of the 
;       queue with the headIndex for word version. It multiplies the 
;       headIndex by WORD_SIZE(EQU 2) and takes a modulus of length for wrapping
;       to get the tail index of the word queue. Then, it adds the word value
;       to the tail of the queue.
;       Once adding the values to the queue is done, it increments the current
;       number of elements in the queue by the size of the entries.
;       It increments the number of elements by BYTE_SIZE if the argument was
;       a byte, and increments the number of elements by WORD_SIZE if the 
;       argument was a word.
;        
;        
; Arguments:    SI - address of the queue
;               AX - the value to add(if word)
;               AL - the value to add(If byte)
;              
; Return Value: None  
;               
; Local Variables:  
;           HeadIndex                - AL    - The head index of the queue
;           TailIndexWithoutWrapping - AX    - Calculated Tail Index
;                                              wihtout wrapping
;           TailIndex                - DX,BX - Calculated Tail Index with wrapping
;           a                        - SI    - address of the queue
;           EnqueueVal               - AX,AL - The value to be added to the queue
;
; Shared Variables:  
;   QueueModule                 - (SI) - [R/W] - The structure for the queue module
;   QueueModule.ElemSize        - (SI) - [Read]- The size of the queue's entries
;   QueueModule.CurrentElemNum  - (SI) - [W]   - Current number of elemnts in the 
;                                                queue.
;   QueueModule.len             - (SI) - [Read]- The length of the queue
;   QueueModule.HeadIndex       - (SI) - [R/W] - The head index of the queue.
;
;                            
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:   If the queue is full, stay in the loop and wait.
;
; Algorithms:        None
;
; Data Structures:   Queue (MyQueue struct in Queue.inc)
;
; Registers Changed: AX, BX, CX, DX, Zero Flag
;
; Limitations:      The queue can only have a length of a power of 2.
; Known bugs:       None 
; Special Notes:    None


; Author:            Sunghoon Choi
; Revision History:
;        10/14/2016  Sunghoon Choi Started writing functional specifications
;        10/14/2016  Sunghoon Choi initial compilation
;        10/15/2016  Sunghoon Choi updated documentation




Enqueue     PROC NEAR
            PUBLIC Enqueue
    
    PUSH AX         ;Save AX so that we can pop it later 
                    ;and add to the queue later.    
     
CheckQueueFull:
    CALL QueueFull          ;Checks if the queue is full.
    JZ CheckQueueFull       ;if the queue is full, stay in the loop and wait.
                            ;this is a blocking function
    ;JNZ EnqCheckElemSize   ;if the queue is not full, go check the element size.
    
EnqCheckElemSize:      
    MOV AL, BYTE_SIZE       ;Checks if the queue is a byte queue or a word queue
    CMP [SI].ElemSize, AL   ;Is the element size BYTE_SIZE(EQU 1)?
    JNE GetWORDTailIndex    ;No, go find the tail index of the word queue.
    ;JE GetByteTailIndex    ;Yes, go find the tail index of the byte queue.


GetByteTailIndex:
     XOR AX,AX              ;clear the high bits of AX since we are going to 
                            ;store the HeadIndex in AL.
    MOV AL, [SI].HeadIndex  ;Get the head index of the queue to find the tail 
                            ;index.
    ADD AX, [SI].CurrentElemNum ;HeadIndex+CurrentElemNum is the tail index
                                ;without wrapping
    XOR DX,DX               ; We have to clear DX since when the divisor is 2bytes,
                            ;the dividend is DX:AX.
    DIV [SI].len            ;DX = TailIndex = (HeadIndex + CurrentElem) mod len    
                    
AddByteValue:
    MOV BX, DX             ;Move the tailIndex to BX for array accessing.
    POP AX                 ;retrieve the value to be enqueued
    MOV BYTE PTR [SI].Queue[BX], AL ;Add the argument byte to the tail of the
                                    ;queue.
    JMP IncCurrentElemNumByte       ;Now that we have added the value,
                                    ;go increment the current number of elments.
 
GetWORDTailIndex:
    XOR AX,AX                   ;clear the high bits of AX since we are going to 
                                ;store the HeadIndex in AL.
    MOV AL, [SI].HeadIndex      ;Get the head index of the queue to find the tail
                                ;index.
    SHL AX, MULT_BY_2           ;We have to double the headIndex
                                ;to translate it into the head index for word version
                                ;In word queue, head index should proceed by 
                                ;a step of WORD_SIZE(EQU 2)
    ADD AX, [SI].CurrentElemNum ;TailIndex = headIndexWordVer + CurrentElemNum
    XOR DX,DX                   ;We have to clear DX since when the divisor is 2bytes,
                                ;the dividend is DX:AX.
    DIV [SI].len                ;DX = TailIndex = (HeadIndex*2+CurrentElemNum) mod len    
    
    
AddWordValue:

    MOV BX, DX                             ;Move the tailIndex to BX for array accessing.
    POP AX                                 ;retrieve the value to be enqueued
    MOV WORD PTR [SI].Queue[BX], AX        ;Add the argument word to the tail of the queue.
                                          
    JMP IncCurrentElemNumWord              ;Now that we have added the value,
                                           ;go increment the current number of elments.


IncCurrentElemNumByte:
    ADD [SI].CurrentElemNum, BYTE_SIZE     ;since we added a byte, 
                                           ;current number of elements must be
                                           ;increased by BYTE_SIZE(EQU 1)
    JMP EndEnqueue
    
IncCurrentElemNumWord:        
    ADD    [SI].CurrentElemNum, WORD_SIZE  ;since we added a word,
                                           ;current number of elements must be
                                           ;increased by WORD_SIZE(EQU 2)
    
        
EndEnqueue:                            
    
    RET

Enqueue ENDP

CODE ENDS

END
