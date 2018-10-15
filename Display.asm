NAME Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Display                                  ;
;                                  Homework 4                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;    This file contains the functions necessary for displaying number or strings
;    on 7-Segment LED digits.
;
; Table of Contents:
;    InitDisplay         - Initializes the necessary buffers and constants for
;                          Display routine.
;    Display             - Converts the passed strings to appropriate pattern values
;                          and save them in a buffer.
;    DisplayNum          - Receive a decimal value, convert it to a string, and convert
;                          the string to its 7-Segment pattern and save it in a buffer.
;    DisplayHex          - Receive a hexadecimal value, convert it to a string, 
;                          and convert the string to its 7-Segment pattern and 
;                          save it in a buffer.
;    DisplayEventHandler - This eventhandler takes a pattern value from
;                          PatternBuffer and output it to the current LED digit.
;                          It is called by Timer2 at every 1ms.
;
;
; Revision History:
;    10/25/2016   Sunghoon Choi      Created
;    10/25/2016   Sunghoon Choi      Fixed PatternBuffer's index error
;    10/25/2016   Sunghoon Choi      Fixed the error caused by not pushing 
;                                    the SI value before calling Dec2String
;                                    and Hex2String.
;    10/28/2016   Sunghoon Choi      Updated functional specifications
;    10/29/2016   Sunghoon Choi      Revised comments.



$INCLUDE (Display.inc)    ;include the .inc file which contains constants for
                          ;Display.asm


EXTRN   Dec2String:NEAR         ;import Dec2String for string conversion
EXTRN   Hex2String:NEAR         ;import Hex2String for string conversion
EXTRN   ASCIISegTable:BYTE      ;reference to the ASCIISegTable
                                ;allows us to convert a character
                                ;to its proper 7-Segment LED pattern.


CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP
       
 

; InitDisplay
;
; Description:       
;       The function fills the StringBuffer with <NULL>s and fills the 
;       PatternBuffer with BLANK_PATTERNs. Also, it initializes the 
;       Current_Digit which is an index that indicates the next digit to be 
;       displayed to the first digit(which has the index of 0)
; Operation:  
;        Since lengths of StringBuffer and PatternBuffer are both NUM_DIGITS(EQU
;       8), it first sets the iteration counter of initialization loop to
;       NUM_DIGITS(EQU 8). Then, it starts inserting <NULL> to StringBuffer and 
;       BLANK_PATTERN to PatternBuffer with increasing the buffer index per
;       each loop. Finally, it initializes the CurrentDigit to the index of
;       first LED digit, which is zero.             
; Arguments:  
;       None 
; Return Value:   
;       None
; Local Variables:   
;       LoopIndex (CX)      -  A counter for initializing loop
;       BufferIndex (SI)    -  An index used for initializing StringBuffer
;                               and PatternBuffer's elements.  
; Shared Variables:  
;        StringBuffer   - [Write]  - A buffer that receives the strings converted
;                                   by Dec2String and Hex2String. The maximum
;                                   length of string it can contain at each time
;                                   is NUM_DIGITS(EQU 8)       
;       PatternBuffer  - [Write]  - A buffer that contains the pattern values
;                                   for the ASCII characters to be displayed on
;                                   LED.
;       CurrentDigit   - [Write]  - The index which indicates the next digit
;                                   to be displayed.
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
;       Buffers
; Registers Changed: 
;       CX, SI
; Limitations:
;       None     
; Known bugs:        
;       None 
; Special Notes: 
;       None
; Author:           
;       Sunghoon Choi
; Revision History:
;       10/25/2016  -   Sunghoon Choi   Started writing functional specification
;       10/25/2016  -   Sunghoon Choi   Initial Compilation
;       10/28/2016  -   Sunghoon Choi   Updated documentation
   
InitDisplay PROC    NEAR
            PUBLIC  InitDisplay  
   
    MOV CX, NUM_DIGITS          ;We have to initialize the buffers by
                                ;initializing each buffer element per loop.
                                ;So, set the loop counter to NUM_DIGITS(EQU 8)
                                ;since the lengths of the buffers are NUM_DIGITS
    XOR SI,SI                   ;Initialize the buffers from their first element
                                ;So, we have to set the buffer index to zero.
FillNullChar:
    MOV StringBuffer[SI], 0     ;Initialize the StringBuffer with NULLs.
FillBlankPattern:    
    MOV PatternBuffer[SI], BLANK_PATTERN 
                                ;Fill the PatternBuffer with BLANK_PATTERNs.
                                ;When BLANK_PATTERNs are output to LEDs,
                                ;corresponding LED will stay turned off.
    INC SI                      ;increment the buffer index to continue
                                ;initialize next element.
    LOOP FillNullChar           ;We continue initializing the buffers until
                                ;the buffers are completely filled.
SetToFirstDigit:                            
    MOV CurrentDigit, 0         ;We are going to turn on the LED digits from the 
                                ;leftmost one. Thus, we initialize the 
                                ;CurrentDigit to the index of first, leftmost
                                ;digit.
EndInitDisplay:
    RET                         ;End initialization process
InitDisplay ENDP







; Display
;
; Description:       
;       The function is passed a string of NUM_DIGITS(EQU 8)length including 
;       NULL. If the passed string is shorter than NUM_DIGITS, pad BLANK_PATTERN
;       for the remaining LEDs.
; Operation:
;       It takes a character from ES:[SI], the location of StringBuffer, to
;       check if the received character is NULL. If the character is NULL, fill
;       BLANK_PATTERNs to the remaining slots of PatternBuffer. If the character
;       is not NULL, it converts the character to the corresponding pattern
;       by using the ASCIISegTable. Note that ES is used for string transfer
;       to leave the string in code segment without changing DS.
; Arguments:  
;       str - ES:[SI] - The string to be converted to a pattern(and to be 
;                       displayed in the end)
; Return Value:   
;       PatternBuffer will get values returned from Display function.
; Local Variables:   
;      string          - ES:[SI] - The string to be converted to a pattern(and to be 
;                                  displayed in the end
;      PatternBufIndex - DI      - The index of PatternBuffer. 
;                                  Used for storing the converted patterns in
;                                  the Pattern Buffer.
;      StringBufIndex  - SI      - The index of StringBuffer.
;                                  Used for taking the strings from StringBuffer.    
; Shared Variables:  
;        StringBuffer  - [Read]  - A buffer that receives the strings converted
;                                  by Dec2String and Hex2String. The maximum
;                                  length of string it can contain at each time
;                                  is NUM_DIGITS(EQU 8)       
;       PatternBuffer  - [Write] - A buffer that contains the pattern values
;                                  for the ASCII characters to be displayed on
;                                  LED.                                                                                                    
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
;       Buffers
; Registers Changed: 
;       AX, BX, DI, SI
; Limitations:     
;       The function can fill the buffer just once per call.
;       Thus, it cannot convert and fill more than NUM_DIGITS(EQU 8) characters
;       per each call.
; Known bugs:        
;       None 
; Special Notes:    
;       None
; Author:            
;       Sunghoon Choi
; Revision History:
;       10/25/2016  -   Sunghoon Choi   Started writing functional specification
;       10/25/2016  -   Sunghoon Choi   Initial Compilation
;       10/25/2016  -   Sunghoon Choi   Fixed Index error of PatternBuffer
;       10/28/2016  -   Sunghoon Choi   Updated documentation  


Display PROC    NEAR
        PUBLIC  Display
        
    XOR DI,DI                           ;We are going to store the patterns in 
                                        ;PatternBuffer from the beginning(index 0).
                                        ;Thus, set the index to zero.            
CheckNull:
    XOR AX,AX                   
    MOV AL, BYTE PTR ES:[SI]            ;Bring a character from StringBuffer
    CMP AL, 0                           ;Is the character null?
    JE FillBlanks                       ;yes, fill the PatternBuffer with
                                        ;BLANK_PATTERNs.
    ;JNE ConvertAndStorePattern         ;No, convert and store patterns.
ConvertAndStorePattern:
    MOV BX, OFFSET(ASCIISegTable)       ;We have to refer to ASCIISegTable
                                        ;to obtain the pattern converted from
                                        ;the string.
                                        
    XLAT CS:ASCIISegTable               ;Converted pattern will be stored in
                                        ;AL. Since the elements of
                                        ;ASCIISegTable are in the order of
                                        ;ASCII strings, we can use the 
                                        ;string(from StringBuffer) as an index
                                        ;for looking up the table.     
                                        
    MOV PatternBuffer[DI], AL           ;Stores the converted pattern in
                                        ;PatternBuffer[PatternBufIndex].
                                        
    INC DI                              ;We have to store the next pattern 
                                        ;which will be used for the next digit.
    INC SI                              ;We should obtain the next string from 
                                        ;StringBuffer which will be displayed
                                        ;on the next LED digit.
                                    
    CMP DI, NUM_DIGITS                  ;Was that the pattern for 
                                        ;the last digit?
    JGE EndDisplay                      ;Yes, finish filling PatternBuffer.
    JL CheckNull                        ;No, go convert the next character.

FillBlanks:
    MOV PatternBuffer[DI], BLANK_PATTERN    ;Since the character was NULL,
                                            ;display BLANK_PATTERN on the digit.
                                            ;BLANK_PATTERN will display 
                                            ;nothing on the LED.          
                                            
    INC DI                              ;Go display BLANK_PATTERN on the next
                                        ;digit too.
    CMP DI, NUM_DIGITS                  ;Was that the last digit?
    JL  FillBlanks                      ;No, keep padding BLANK_PATTERN for
                                        ;the remaining LED digits.
    ;JGE EndDisplay                     ;Yes, the LED digits are completely
                                        ;padded with blank patterns.
                                        ;So, finish the process.
EndDisplay:
    RET                                 ;End of Display Process.
Display ENDP   


   
   
; DisplayNum
;
; Description:       
;       The function is passed a 16-bit signed value to output in decimal 
;       of 5 digits plus sign. If the number has less than 5 digits, 
;       digit 0 will be padded for the remaining spots.
; Operation:  
;        First, it makes DS and ES point to the same segment.
;       Then, it obtains the address of StringBuffer where the string converted
;       by Dec2String will be stored. Next, it calls Dec2String to store the 
;       converted string in StringBuffer and calls Display function to convert
;       the string into patterns.             
; Arguments:  
;       n   -   AX  -   The 16-bit signed value to be output in decimal string.
; Return Value:   
;       none
; Local Variables:   
;       StringBufAddr   -   SI  -   The address of StringBuffer where
;                                   the converted strings will be saved.
; Shared Variables:  
;        StringBuffer   - [Write] - A buffer that receives the strings converted
;                                   by Dec2String and Hex2String. The maximum
;                                   length of string it can contain at each time
;                                   is NUM_DIGITS(EQU 8)                                         
; Global Variables:  
;        None
; Input:
;        None
; Output:            
;        None
; Error Handling:   
;        None
; Algorithms:        
;        None
; Data Structures:   
;        Buffers
; Registers Changed: 
;        BX, ,DS, ES, SI
; Limitations:     
;        None
; Known bugs:
;        None 
; Special Notes:
;        None
; Author:            
;        Sunghoon Choi
; Revision History:
;       10/25/2016  -   Sunghoon Choi   Started writing functional specification
;       10/25/2016  -   Sunghoon Choi   Initial Compilation
;       10/25/2016  -   Sunghoon Choi   Fixed the SI related error by
;                                       pushing SI before calling Dec2String
;                                       and popping SI before calling Display.
;       10/28/2016  -   Sunghoon Choi   Updated documentation            
   
DisplayNum  PROC    NEAR
            PUBLIC  DisplayNum
            
AdjustSegmentNum:
    MOV BX, DS                      ;We should make DS and ES point to the
    MOV ES, BX                      ;same segment since Dec2String returns
                                    ;the converted string to DS:SI while
                                    ;Display gets the string from ES:SI.
                                    
    MOV SI, OFFSET(StringBuffer)    ;set the destination for converted strings
                                    ;The string converted by Dec2String will
                                    ;be stored in StringBuffer.

ConvertAndStoreNum:
    PUSH SI                         ;We need to save the address of StringBuffer
                                    ;since it will be used by Display in the
                                    ;future.
                                    ;Dec2String alters the value of SI.
    CALL Dec2String                 ;The converted decimal string will be stored
                                    ;in StringBuffer.
    POP SI                          ;Return the address of StringBuffer since
                                    ;Dec2String has changed the value of SI.
    CALL Display                    ;Now that we have string values in
                                    ;StringBuffer, go convert the string to
                                    ;proper patterns.
EndDisplayNum:    
    RET                             ;Finsih the process and return to where
                                    ;this process was called.
DisplayNum ENDP    
        

        
        
        
        
 
; DisplayHex
;
; Description:       
;       The function is passed a 16-bit unsigned value to output in hexadecimal 
;       of 4 digits. If the number has less than 4 digits, 
;       digit 0 will be padded for the remaining spots.            
; Operation:  
;       First, it makes DS and ES point to the same segment.
;       Then, it obtains the address of StringBuffer where the string converted
;       by Hex2String will be stored. Next, it calls Hex2String to store the 
;       converted string in StringBuffer and calls Display function to convert
;       the string into patterns.                     
; Arguments:  
;       n   -   AX  -   The 16-bit unsigned value to be output 
;                       in hexadecimal string.
; Return Value:   
;        none
; Local Variables:   
;         StringBufAddr -   SI     -   The address of StringBuffer where
;                                      the converted strings will be saved.
;
; Shared Variables:  
;        StringBuffer   - [Write]  -   A buffer that receives the strings converted
;                                      by Dec2String and Hex2String. The maximum
;                                      length of string it can contain at each time
;                                      is NUM_DIGITS(EQU 8)                                                                                           
; Global Variables:  
;        none
; Input:    
;        none
; Output:            
;        none
; Error Handling:   
;        none
; Algorithms:        
;        none
; Data Structures:   
;        Buffers
; Registers Changed: 
;        BX, DS, ES, SI
; Limitations:     
;        none
; Known bugs:
;        none 
; Special Notes:
;        none
; Author:           
;        Sunghoon Choi
; Revision History:
;       10/25/2016  -    Sunghoon Choi  Started writing functional specification
;       10/25/2016  -   Sunghoon Choi   Initial Compilation
;       10/25/2016  -   Sunghoon Choi   Fixed the SI related error by
;                                       pushing SI before calling Hex2String
;                                       and popping SI before calling Display.
;       10/28/2016  -   Sunghoon Choi   Updated documentation          
        
DisplayHex  PROC    NEAR
            PUBLIC  DisplayHex
            
AdjustSegmentHex:
    MOV BX, DS                      ;We should make DS and ES point to the
    MOV ES, BX                      ;same segment since Hex2String returns
                                    ;the converted string to DS:SI while
                                    ;Display gets the string from ES:SI.
                                    
    MOV SI, OFFSET(StringBuffer)    ;set the destination for converted strings
                                    ;The string converted by Hex2String will
                                    ;be stored in StringBuffer.  

ConvertAndStoreHex:
    PUSH SI                         ;We need to save the address of StringBuffer
                                    ;since it will be used by Display
                                    ;in the future.
                                    ;Hex2String alters the value of SI.
    CALL Hex2String                 ;The converted hexadecimal string will be 
                                    ;stored in StringBuffer.
    POP SI                          ;Return the address of StringBuffer since
                                    ;Hex2String has changed the value of SI.
    CALL Display                    ;Now that we have string values in
                                    ;StringBuffer, go convert the string to
                                    ;proper patterns.

EndDisplayHex:    
    RET                             ;Finsih the process and return to where
                                    ;this process was called.
DisplayHex ENDP    
                
        
        
 


; DisplayEventHandler
;
; Description:       
;        It is an EventHandler for displaying on LED digits.
;        It is called by Timer interrupts to take patterns from PatternBuffer and 
;        output them to the actual LED(hardware). 
;        It goes through each digit from the leftmost digit to rightmost digit
;        by being called once per 1ms so that human cannot notice that the LEDs
;        are actually blinking.        
; Operation:  
;        It obtains each patterns digit by digit from PatternBuffer and output
;        the patterns to LEDs. After outputting to LEDs, it increments the 
;        CurrentDigit with wrapping to display on the next LED digit.
; Arguments:  
;        none
; Return Value:   
;        none
; Local Variables:   
;        CurrentLEDAddr    -    DX    -    The address of the current LED digit to
;                                          be displayed.
;        PatternVal        -   AL     -    The pattern to be displayed on the current
;                                          LED digit.
; Shared Variables:  
;       PatternBuffer  - [Write]      -    A buffer that contains the pattern values
;                                          for the ASCII characters to be displayed on
;                                          LED.
;       CurrentDigit   - [Read]       -    The index which indicates the next digit
;                                          to be displayed.                                                                                                    
; Global Variables:  
;        none
; Input:    
;        none         
; Output:            
;        7-Segment LED digits.(00H~07H)
; Error Handling:   
;        none
; Algorithms:        
;        none
; Data Structures:   
;        Buffers
; Registers Changed: 
;        AX, BX, CX, DX
; Limitations:     
;        The eventhandler can display only one LED digit per call.
; Known bugs:        
;        none 
; Special Notes:   
;        none
; Author:            
;        Sunghoon Choi
; Revision History:
;       10/25/2016  -   Sunghoon Choi   Started writing functional specification
;       10/25/2016  -   Sunghoon Choi   Initial Compilation
;       10/25/2016  -   Sunghoon Choi   Fixed index calculation.
;       10/28/2016  -   Sunghoon Choi   Updated documentation         


DisplayEventHandler PROC    NEAR
                    PUBLIC  DisplayEventHandler
                    
    XOR AX, AX        
    XOR BX, BX          ;We need to clear the high bits of BX since
                        ;we are going to use BX as an index for PatternBuffer
                        ;while its maximum value is NUM_DIGITS(EQU 8), a byte
                        ;value obtained from CurrentDigit.
    XOR DX, DX          ;The LED digit's address starts at 00H.
    MOV DL, LED_ADDR    ;So clear DH and set the address of output port
                        ;to LED_ADDR(EQU 00H)
      

HandlerFillPattern:
    MOV BL, CurrentDigit         ;We are going to output to the LED digits
                                 ;one by one, by moving from left to right.
                                 ;Thus, load the current digit's index.
    ADD DL, BL                   ;Current digit's address equals
                                 ;First LED's address + digit count
    MOV AL, PatternBuffer[BX]    ;Bring the pattern for current digit.
    OUT DX, AL                   ;Output the pattern to the current LED digit.

HandlerUpdateDigit:
    MOV AL, CurrentDigit         ;Since we have outputted to the current 
                                 ;LED, we should proceed to output to the 
                                 ;next LED digit.
    INC AL                       ;Move to the next digit.
    MOV CL, NUM_DIGITS           ;We need to wrap when incrementing 
                                 ;the digit index.
    DIV CL                       ;CurrentDigit = (CurrentDigit + 1) mod NUM_DIGITS
    MOV CurrentDigit, AH         ;Update the CurrentDigit.
EndDisplayEventHandler:
    RET                          ;End of DisplayEventHandler.
        
DisplayEventHandler ENDP        


CODE ENDS                    
        
DATA    SEGMENT PUBLIC  'DATA'
      StringBuffer  DB  NUM_DIGITS  DUP (?) ;The buffer which contains
                                            ;string(a list of characters)
                                            ;to be converted to patterns.
      PatternBuffer DB  NUM_DIGITS  DUP (?) ;The buffer which contains
                                            ;patterns to be outputted to
                                            ;7-Segment LED digits.
      CurrentDigit  DB  ?                   ;The digit to be outputted next.
DATA    ENDS


END