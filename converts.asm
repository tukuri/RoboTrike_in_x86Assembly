NAME    CONVERTS    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                Converts.asm                                ;
;                                 Homework2                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      This program converts 16-bit signed or unsigned values to 
;                   convert them into decimal or hexadecimal and store them as
;                   strings.
; Table of Contents:
;                   Dec2String - converts a 16-bit signed value to convert to 
;                                decimal and store it as a string
;                   Hex2String - converts a 16-bit unsigned value to convert to 
;                                hexadecimal and store it as a string
;
; Input:            None.
; Output:           None.
;
; User Interface:   None.
; Error Handling:   None.
;
; Algorithms:       Keep dividing the value by 10 or 16 and add ASCII offset to 
;                   convert the value to digit string.
; Data Structures:  None.
;
; Revision History:
;        10/14/2016  17:00 Sunghoon Choi Started writing functional specification
;        10/14/2016  18:00 Sunghoon Choi initial compilation
;        10/14/2016  21:00 Sunghoon Choi Corrected stack pointer error
;        10/15/2016  04:00 Sunghoon Choi updated documentation



$INCLUDE(converts.INC)    ;include file for constants  


CGROUP  GROUP   CODE

CODE SEGMENT PUBLIC 'CODE'
    
    ASSUME CS:CGROUP
                 
        
; Dec2String
;
; Description:       This function is passed a 16-bit signed value to convert to 
;                    decimal and store as a string. The string is 5 digits plus 
;                    negative sign if the argument is negative. The string will 
;                    have leading zeros if the number of digits are less than 5. 
;                    The string contains the decimal representation of the argum
;                    ent in ASCII and terminates with <null>. The string is stor
;                    ed starting at the memory location indicated by the
;                    passed address. 
;
; Operation:         The function checks if the argument(AX) is negative. If it 
;                    is, store the minus sign '-' in DS:[SI] and negate the 
;                    argument so that we can get the absolute value of it. If 
;                    the argument is positive, skip the negating procedure and 
;                    start the loop for extracting digits. Inside the loop, the 
;                    function divides the argument by power-of-10, adds '0' to 
;                    the quotient for ASCII conversion, and store it in DS:[SI]. 
;                    The remainder will be used for updating the argument(AX) 
;                    later. Note that the power-of-10 is initialized 
;                    to 10000(for 5 digit number) before the beginning of the 
;                    loop. Now, after storing the converted digit string 
;                    at DS:[SI], the function increments SI for the next loop, 
;                    and updates the power-of-10 by dividing the power-of-10 
;                    by 10. Now, update the argument(AX) to the remainder of 
;                    'arg / pwr10' division, and go back to the beginning of the
;                    loop. The loop will end when the power-of-10 becomes zero. 
;                    When all the digits are converted and stored, the function 
;                    adds <NULL> to DS:[SI] and ends.
;                     
; Arguments:         AX - 16 bit signed value to convert to decimal string
;                    SI - The starting memory location where the converted 
;                         string will be stored.
; Return Value:      None
;
; Local Variables:   AX - 16 bit signed value to convert to decimal string
;                    SI - The starting memory location where the converted 
;                         string will be stored. 
; Shared Variables:  None
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    None
;
; Algorithms:        Repeat dividing the argument by power of 10 and use the 
;                    quotients to convert them to ASCII string and store them.
; Data Structures:   None.
;
; Registers Changed: AX, BX, CX, DX, SI, flags
;
; Limitations:       None
; Known bugs:        None
; Special Notes:     None


; Author:            Sunghoon Choi
; Last Modified:     10/15/2016
; Revision History:
;        10/14/2016  17:00 Sunghoon Choi Started writing functional specification
;        10/14/2016  18:00 Sunghoon Choi initial compilation
;        10/14/2016  21:00 Sunghoon Choi Corrected stack pointer error
;        10/15/2016  04:00 Sunghoon Choi updated documentation
                          
                
              
                
                
Dec2String PROC NEAR
           PUBLIC Dec2String

InitDec2String:
           
CheckSign:                  ;Dec2String gets passed a 16-bit signed value.
                            ;Thus, we need to check sign first
CMP AX, 0                   ;Is the argument positive or equal to 0?

JGE Prepare10pow            ;Yes. Skip the negating procedure and start 
                            ;extracting digits.
;JL HandlerNegArg           ;No. Store negative sign and get the absolute value.
           
HandleNegArg:
MOV BYTE PTR [SI], '-'      ;store negative sign first
INC SI                      ;increment the index to store other digits
NEG AX                      ;get the absolute value of the argument so that we 
                            ;can convert it.

                            
Prepare10pow:               ;Now we start extracting and converting digits.
MOV CX, INITIAL_PWR10       ;Since the result string is 5 digits plus negative 
                            ;sign(if applicable),
                            ;we initialize the dividing power-of-10 to 10000.
              
DecConvertStoreLoop:        ;start dividing the argument by power of 10 to 
                            ;extract digits.
MOV DX, 0                   ;clear DX for dividing procedure.
DIV CX                      ;divide the argument by the power of 10 to get a 
                            ;digit.
                            ;AX<-quotient(digit), 
                            ;DX<-remainder(will be used to update argument)

ADD AX, ASCII_CODE_ZERO     ;add the ASCII offset to the quotient to convert 
                            ;it into ASCII.
MOV BYTE PTR [SI], AL       ;store the converted string at DS:[SI]
                    
INC SI                      ;increment the index for next digit

                            
PUSH DX                     ;save the remainder of previous division so that we 
                            ;can use it for updating the argument(AX) later
MOV DX, 0                   ;clear DX for dividing procedure. We will update the 
                            ;power of 10 here.                         
MOV AX, CX                  ;move the power-of-10 to AX for division process.
MOV BX, 10                  ;We should divide the power of 10 by 10 to update it
DIV BX                      ;divide the power-of-10 by 10 to update it.

POP DX                      ;retrieve the remainder of previous division to 
                            ;update the argument(AX)  

CMP AX, 0                   ;Is the updated power-of-10 zero?
 
JE EndDec2String            ;if the updated power-of-10 is zero, it means we 
                            ;have completed converting the value.
                            ;if the updated power-of-10 is still not zero, 
                            ;we keep the digit-converting loop.
                    
MOV CX, AX                  ;move the updated power-of-10 to CX so that we can 
                            ;continue digit-converting process.

                
MOV AX, DX                  ;update the argument to the remainder of 
                            ;(argument/power-of-10) and 
                            ;repeat the digit converting process.
     

JMP DecConvertStoreLoop     ;continue repeating the digit-converting 
                            ;process.                                                           
                             

EndDec2String:              ;We finished converting and storing the strings.

MOV  BYTE PTR[SI], 0        ;Attach <NULL> to the end of the string.

RET                         ;return to where this function was called
Dec2String ENDP             
       
                         
                                               
                         
; Hex2String
;
; Description:       This function is passed a 16-bit unsigned value to convert 
;                    to hexadecimal and store as a string. The string is 
;                    4 digits(plus <NULL>) The string will have leading zeros if
;                    the number of digits are less than 4. The string contains 
;                    the hexadecimal representation of the input value in ASCII 
;                    and terminates with <null>.
;                    The string is stored starting at the memory location 
;                    indicated by the passed address. 
;
; Operation:         The function starts a loop of extracting and converting the 
;                    digits. Inside the loop, it divides the argument by 
;                    power-of-16. If the quotient is bigger than or equal to 10, 
;                    it adds '0' to the quotient for ASCII conversion. 
;                    Otherwise, it adds 'A'-10. 
;                    Then, it stores the quotient in DS:[SI]. The remainder will 
;                    be used for updating the argument(AX) later. Note that the 
;                    power-of-16 is initialized to 4096(for 4 digit number) 
;                    before the beginning of the loop. 
;                    Now, after storing the converted digit string at DS:[SI],
;                    the function increments SI for the next loop, and updates 
;                    the power-of-16 by dividing the power-of-16 by 16. Now, 
;                    update the argument(AX) to the
;                    remainder of 'arg / pwr16' division, and go back to the 
;                    beginning of the loop. The loop will end when the 
;                    power-of-16 becomes zero.
;                    When all the digits are converted and stored, the function 
;                    adds <NULL> to DS:[SI] and ends.
;                     
; Arguments:         AX - 16 bit unsigned value to convert to hexadecimal string
;                    SI - The starting memory location where the converted 
;                         string will be stored.
; Return Value:      None
;
; Local Variables:   AX - 16 bit unsigned value to convert to hexadecimal string
;                    SI - The starting memory location where the converted 
;                         string will be stored. 
; Shared Variables:  None
; Global Variables:  None
;
; Input:             None
; Output:            None
;
; Error Handling:    None
;
; Algorithms:        Repeat dividing the argument by power of 16 and use the 
;                    quotients to convert them to ASCII string and store them.
; Data Structures:   None.
;
; Registers Changed: AX, BX, CX, DX, SI, flags
;
; Limitations:       None
; Known bugs:        None
; Special Notes:     None


; Author:            Sunghoon Choi
; Last Modified:     10/15/2016
; Revision History:
;        10/14/2016  17:00 Sunghoon Choi Started writing functional specifications
;        10/14/2016  18:00 Sunghoon Choi initial compilation
;        10/14/2016  21:00 Sunghoon Choi Corrected stack pointer error
;        10/15/2016  04:00 Sunghoon Choi updated documentation                     

    
Hex2String PROC NEAR
           PUBLIC Hex2String        
                    

Prepare16pow:                
MOV CX, INITIAL_PWR16       ;initialize the power-of-16 to 4096(16^3) since 
                            ;the argument will be converted to 4 digit string.

HexConvertStoreLoop:        ;start dividing the argument by power of 16 to 
                            ;extract digits
                            
MOV DX, 0                   ;clear DX for dividing procedure.
  
DIV CX                      ;divide the argument by the power of 16 to get a 
                            ;digit.
                            ;AX<-quotient(digit), 
                            ;DX<-remainder(will be used to update argument)

CheckDigitOver10:           ;check if the digit is larger than or equal to 10
                            ;since 10 is 'A' and the required ASCII offset will 
                            ;become different.
                            
CMP AX, 10                  ;Is the digit larger than or equal to 10?
JB SaveLessThan10Ascii      ;No. add '0' to it and store it.
;JAE SaveBiggerThanOrEqualto10Ascii   ;Yes. add 'A'-10 to it and store it.


SaveBiggerThanOrEqualTo10Ascii:
ADD AX, ASCII_CODE_AMINUSTEN  ;add 'A'-10 to the digit to convert it into ASCII
MOV BYTE PTR[SI], AL          ;save the ascii string at DS:[SI]
INC SI                        ;increment the index for next digit
MOV AX, DX                    ;update the argument(AX) to the remainder of 
                              ;previous division. So, now the number of digits 
                              ;of the argument is decreased by 1

JMP UpdatePwr16               ;Now that we have converted a digit, 
                              ;we should update the power-of-16


SaveLessThan10Ascii:                   
ADD AX, ASCII_CODE_ZERO       ;add the ASCII offset to the quotient to convert 
                              ;it into ASCII.
MOV BYTE PTR[SI], AL          ;store the converted string at DS:[SI]
INC SI                        ;increment the index for next digit
MOV AX, DX                    ;update the argument(AX) to the remainder of 
                              ;previous division.
                              ;So, now the number of digits decreased by 1.

UpdatePwr16:  
         
PUSH DX                ;Save the remainder of previous division so that we can 
                       ;use it for updating the argument(AX) later

MOV DX, 0              ;Clear DX for dividing procedure. We will update the 
                       ;power of 16 here.                
MOV AX, CX             ;Move the power-of-16 to AX for division process.
MOV BX, 16             ;We should divide the power-of-16 by 16 to update it.
DIV BX                 ;Divide the power-of-16 by 16 to update it.   

POP DX                 ;Retrieve the remainder of previous division to update 
                       ;the argument(AX)   

CMP AX, 0              ;Is the updated power-of-16 zero?

JE EndHex2String       ;If the updated power-of is zero, it means we have 
                       ;completed converting the value.
                       ;If the updated power-of-16 is still not zero, 
                       ;we keep the digit-converting loop.

MOV CX, AX             ;Move the updated power-of-16 to CX so that we can 
                       ;continue digit-converting process.   
                       
MOV AX, DX             ;Update the argument to the remainder of 
                       ;(argument/power-of-16) and repeat the digit converting 
                       ;process
     
JMP HexConvertStoreLoop;Continue the digit-converting process.                                                        
                            
             
EndHex2String:         ;We finished converting and storing the strings.

MOV  BYTE PTR[SI], 0   ;Attach <NULL> to the end of the string.
           

RET                    ;return to where this function was called
Hex2String ENDP             
     
     
     
     
UnsignedDec2String PROC NEAR
                   PUBLIC UnsignedDec2String
                    
UnsignPrepare10pow:          ;Now we start extracting and converting digits.
MOV CX, INITIAL_PWR10        ;Since the result string is 5 digits plus negative 
                             ;sign(if applicable),
                             ;we initialize the dividing power-of-10 to 10000.
              
UnsignDecConvertStoreLoop:   ;start dividing the argument by power of 10 to 
                             ;extract digits.
MOV DX, 0                    ;clear DX for dividing procedure.
DIV CX                       ;divide the argument by the power of 10 to get a 
                             ;digit.
                             ;AX<-quotient(digit), 
                             ;DX<-remainder(will be used to update argument)

ADD AX, ASCII_CODE_ZERO      ;add the ASCII offset to the quotient to convert 
                             ;it into ASCII.
MOV BYTE PTR [SI], AL        ;store the converted string at DS:[SI]
                    
INC SI                       ;increment the index for next digit

                            
PUSH DX                      ;save the remainder of previous division so that we 
                             ;can use it for updating the argument(AX) later
MOV DX, 0                    ;clear DX for dividing procedure. We will update the 
                             ;power of 10 here.                         
MOV AX, CX                   ;move the power-of-10 to AX for division process.
MOV BX, 10                   ;We should divide the power of 10 by 10 to update it
DIV BX                       ;divide the power-of-10 by 10 to update it.

POP DX                       ;retrieve the remainder of previous division to 
                             ;update the argument(AX)  

CMP AX, 0                    ;Is the updated power-of-10 zero?
 
JE EndUnsignedDec2String     ;if the updated power-of-10 is zero, it means we 
                             ;have completed converting the value.
                             ;if the updated power-of-10 is still not zero, 
                             ;we keep the digit-converting loop.
                    
MOV CX, AX                   ;move the updated power-of-10 to CX so that we can 
                             ;continue digit-converting process.

                
MOV AX, DX                   ;update the argument to the remainder of 
                             ;(argument/power-of-10) and 
                             ;repeat the digit converting process.
     

JMP UnsignDecConvertStoreLoop ;continue repeating the digit-converting 
                              ;process.                                                           
                             

EndUnsignedDec2String:        ;We finished converting and storing the strings.

MOV  BYTE PTR[SI], 0          ;Attach <NULL> to the end of the string.

RET                           ;return to where this function was called
UnsignedDec2String ENDP            
                   
CODE ENDS


END