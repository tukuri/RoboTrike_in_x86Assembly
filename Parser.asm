NAME Parser

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Parser                                   ;
;                                  Homework 8                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:
;    This file contains functions necessary for handling characters of serial input.
;    It executes various commands through finite state machine. 
;
; Table of Contents:
;    ParseSerialChar        -    Get passed a character and process it as a command.
;    InitParser             -    Initializes shared variables related to parsing routine
;    ResetHandle            -    Reset the shared variables related to the arguments of parsing
;                                routine.
;    GetToken               -    Returns the token value and token type.
;    DoSetAbsMotorSpeed     -    Sets the absolute value of motor speeds.
;    DoSetRelMotorSpeed     -    Increases or decreases the motors speeds. 
;    DoSetMotorDirection    -    Sets the direction of RoboTrike.
;    DoSetLaserOn           -    Turns the laser on.
;    DoSetLaserOff          -    Turns the laser off.
;    DoSetAbsTurretAngle    -    Sets the absolute value of the turret angle.
;    DoSetRelTurretAngle    -    Increases or decreases the angle of the turret.
;    DoSetTurretElevation   -    Sets the elevation of the turret.
;    DoAddDigit             -    Adds a digit to the shared argument value(ArgNum)
;    DoSetError             -    Returns PARSER_ERROR.
;    DoNOP                  -    Does nothing. Created for State Machine.
;    DoSetNegSign           -    Set the negative sign of shared argument value (ArgSign)

; Revision History:
;    11/23/2016      Sunghoon Choi     Created    
;    11/25/2016      Sunghoon Choi     Initial Compilation
;    11/26/2016      Sunghoon Choi     Updated Documentation

$INCLUDE(general.inc)             ;Include the .inc file which contains general constants for 
                                  ;RoboTrike system.
$INCLUDE(Parser.inc)              ;Include the .inc file which contains constants for parsing
                                  ;routine.
$INCLUDE(Motors.inc)              ;Include the .inc file which contains constatns for motors
                                  ;and laser turrets.
EXTRN   SetMotorSpeed:NEAR        ;Import SetMotorSpeed for setting motor speeds.
EXTRN    GetMotorSpeed:NEAR       ;Import GetMotorSpeed for obtaining current motor speeds.
EXTRN    GetMotorDirection:NEAR   ;Import GetMotorDirection for obtaining current direction.
EXTRN    SetLaser:NEAR            ;Import SetLaser for turning laser on.
EXTRN    SetTurretAngle:NEAR      ;Import SetTurretAngle for setting the turret's angle.
EXTRN    SetRelTurretAngle:NEAR   ;Import SetRelTurretAngle for increasing or decreasing the
                                  ;turret's angle.
EXTRN    SetTurretElevation:NEAR  ;Import SetTurretElevation for setting the elevation of
                                  ;the turret.

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP

; ParseSerialChar
;
; Description:
;      The function is passed a character (c) which is presumed to be from the serial input. 
;      The character (c) is passed by value in AL. It executes proper functions of current 
;      transition and proceed from the current state to the next state. The function returns
;      the status of the parsing operation in AX. PARSER_SUCCESS is returned if there is no
;      parsing error and PARSER_ERROR is returned if there is any parsing error.
; Operation:
;      It first calls GetToken to obtain the token value and token type. Next, it calculates
;     the offset for current transition by using the StateTable and the token type. 
;      Then, it calls the action function of current transition with token value as an 
;      argument. When the action function is done, it proceeds ParseState to the next state.
;      Finally, it checks if there was an error with parsing and executing commands. If 
;      there was, it resets the ParseState to ST_INIT and resets all shared variables
;      related to parsing routine. 
; Arguments:  
;     character(AL) - character to be parsed
; Return Value:   
;     ParserErrorFlag(AX)    -    PARSER_SUCCESS  if there's no parsing error
;                            -    PARSER_ERROR    if there's a parsing error
; Local Variables:   
;     TokenType(DH)    -  The type of the token
;     TokenValue(CH)   -  The value of the token
;     TableOffset(BX)  -  The offset for state table  
; Shared Variables:  
;      ParserState     -   [R/W]  -  The current state of parsing state machine                      
; Global Variables:  
;     None
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     PARSER_ERROR will be returned in AX if there's a parsing error.  
; Algorithms:        
;     None
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     AX, BX, CX, DX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments
    
ParseSerialChar PROC    NEAR
                PUBLIC  ParseSerialChar
            
GetTheToken:
    CALL GetToken                ;Get the token value and token type                            
    MOV DH, AH                   ;DH = Token type
    MOV CH, AL                   ;CH = Token Value
GetNextTableIndex:
     MOV    AL, NUM_TOKEN_TYPES  ;find row in the table
    MUL    ParserState           ;AX is start of row for current state
    ADD    AL, DH                ;get the actual transition
    ADC    AH, 0                 ;propagate low byte carry into high byte

    IMUL    BX, AX, SIZE TRANSITION_ENTRY   ;BX = Table Offset

DoNextAction:   
    MOV AL, CH                        ;Retrieve the token value to use it as an argument 
                                      ;for functions to be called by the transition.
    CALL CS:StateTable[BX].ACTION     ;Do the proper action(function) for the transition.
                                      ;All action functions will return the parsing result
                                      ;in AX.
GoNextState:
    MOV BL, CS:StateTable[BX].NEXT_STATE ;Go to the next state of the state machine.
    MOV ParserState, BL                  ;Update ParserState with the new state.
CheckParserError:
    CMP AX, PARSER_ERROR                 ;Was there any error with parsing and executing
                                         ;functions?
    JNE EndParserSerialChar              ;There was no error. Finish ParseSerialChar procedure.
;JE ResetParserState                     ;There was an error. Reset the state to the initial
                                         ;state and reset ArgNum and ArgSign.
ResetParserState:
    MOV ParserState, ST_INITIAL          ;Reset the state back to the ST_INITIAL.
    CALL ResetHandle                     ;Reset ArgNum and ArgSign.
EndParserSerialChar:
    RET                                  ;End of ParserSerialChar
ParseSerialChar ENDP

    


; InitParser
;
; Description:
;      It initializes all shared variables related to parsing routine.
; Operation:
;      It first initializes ParserState to ST_INITIAL.
;      Then, it calls ResetHandle to initialize ArgNum and ArgSign.
; Arguments:  
;     None
; Return Value:   
;     None
; Local Variables:   
;     None 
; Shared Variables:  
;      ParserState     -    [Write]  -  The current state of parsing state machine                      
; Global Variables:  
;     None
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     None
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments
            
InitParser  PROC    NEAR
            PUBLIC  InitParser


InitParserState:            
    MOV ParserState, ST_INITIAL ;Initializes ParserState to the initial state of 
                                ;finite state machine.
ResetParserHandle:
    CALL ResetHandle            ;Calls ResetHandle to reset ArgNum and ArgSign.

EndInitParser:
    RET                        ;End of InitParser

InitParser  ENDP    



; ResetHandle
;
; Description:
;      It resets shared variables(ArgNum, ArgSign) of parsing routine.
; Operation:
;      It resets ArgSign to ARG_POSITIVE and ArgNum to ARG_INIT.
; Arguments:  
;     None
; Return Value:   
;     None
; Local Variables:   
;     None 
; Shared Variables:  
;      ArgSign    -    [Write]  -  The sign of the argument for action functions.
;      ArgNum     -   [Write]   -  The value of the argument for action functions.
; Global Variables:  
;     None
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     None
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments

ResetHandle PROC    NEAR
            PUBLIC  ResetHandle
            
InitArguments:
    MOV ArgSign, ARG_POSITIVE    ;Resets argument's sign to positive.
                                
    MOV ArgNum,  ARG_INIT        ;Resets the argument value to ARG_INIT.
EndResetHandle:
    RET                          ;End of ResetHandle
ResetHandle ENDP
    

; GetToken
;
; Description:
;     This procedure returns the token class and token value for the passed character.
;     The character is truncated to 7-bits.
; Operation:
;      First, it clears the unused high bit by using a mask.
;      Then, it obtains the token type and token value of the passed character by 
;      referring to TokenTypeTable and TokenValueTable.
; Arguments:  
;     character(AL)    - The character to look up.
; Return Value:   
;     TokenValue(AL) - token value of the character
;     TokenType(AH)  - token type of the character.
; Local Variables:   
;     None 
; Shared Variables:  
;      None
; Global Variables:  
;     None
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     AX, BX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments

GetToken    PROC    NEAR
            PUBLIC  GetToken

MaskTokenBits:
    AND        AL, TOKEN_MASK             ;strip unused high bits
    MOV        AH, AL                     ;and preserve value in AH

GetTokenType:
    MOV     BX, OFFSET(TokenTypeTable)    ;BX points at table
    XLAT    CS:TokenTypeTable             ;Store token type in AL
    
    XCHG    AH, AL                        ;token type in AH, character in AL

GetTokenValue:
    MOV     BX, OFFSET(TokenValueTable)   ;BX points at table
    XLAT    CS:TokenValueTable            ;Store token value in AL

EndGetToken:
    RET                                   ;End of GetToken
GetToken ENDP  


; DoSetAbsMotorSpeed
;
; Description:
;      It sets the absolute speed of the motors and returns PARSER_SUCCESS.
; Operation:
;      It sets AX to ArgNum and BX to IGNORE_ANGLE and calls SetMotorSpeed to set the 
;      absolute motor speed. After updaating speed is done, it sets ErrorFlag to
;      PARSER_SUCCESS to notify that this action function was executed successfully.
;      Finally, it calls ResetHandle to reset shared variables and exits.
;      Note that BX's value is pushed in the beginning and popped in the end to keep its
;      value unchnaged. This is because BX's value has to be used as an index for finding
;      the next state in ParseSerialChar.
; Arguments:  
;     None
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     NewSpeed(AX)    -    The speed of motors to be set.
;      Angle(BX)      -   The direction angle of motors to be set.
; Shared Variables:  
;      ArgNum     -   [Read]  -  The value of the argument for action functions.
; Global Variables:  
;     None
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments

DoSetAbsMotorSpeed  PROC    NEAR
                    PUBLIC  DoSetAbsMotorSpeed
                    
   PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                           ;finding the next state in ParseSerialChar.     
SetSpeedAngle:
   MOV AX, ArgNum          ;Speed(Argument 1 for SetMotorSpeed) = ArgNum
   MOV BX, IGNORE_ANGLE    ;Angle(Argument 2 for SetMotorSpeed) = IGNORE_ANGLE
                           ;Since we are setting the absolute speed, we should not change
                           ;the direction of motors.
   CALL SetMotorSpeed      ;With the two arguments, calls SetMotorSpeed to set the 
                           ;absolute speed of motors.
EndDoSetAbsMotorSpeed:
   MOV AX, PARSER_SUCCESS  ;Since this action function is over, return PARSER_SUCCESS.
   CALL ResetHandle        ;Since the action function is over, we have to reset ArgNum
                           ;and ArgSign to execute the functions of next transition.
   POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
   RET                     ;End of DoSetAbsMotorSpeed
DoSetAbsMotorSpeed ENDP


; DoSetRelMotorSpeed
;
; Description:
;      It increases or decreases the motor speed by the value of ArgNum.
;      If the updated speed exceeds the upper limit or lower limit, it saturates the
;      NewSpeed to MAX_SPEED and MIN_SPEED. It returns PARSER_SUCCESS when setting speed is
;      done.
; Operation:  
;      It first calls GetMotorSpeed to obtain the current motor speed. Then, it checks 
;      ArgSign to figure out whether ArgNum is positive or negative. If ArgNum is positive, 
;      it adds ArgNum to the current speed. Now, if a carry flag is set or the new speed 
;      exceeds the upper limit(MAX_SPEED), it saturates the new speed to MAX_SPEED. 
;      If ArgNum is negative, it subtracts ArgNum from the current speed.  If carry flag is
;      set or the new speed exceeds lower limit(MIN_SPEED), it saturates the new speed to 
;      MIN_SPEED. Finally, it sets the angle to IGNORE_ANGLE and calls SetMotorSpeed to 
;      change the motor speed. Before the procedure ends, it returns PARSER_SUCCESS in AX 
;      and calls ResetHandle to reset shared variables.
;      Note that BX's value is pushed in the beginning and popped in the end to keep its
;      value unchnaged. This is because BX's value has to be used as an index for finding
;      the next state in ParseSerialChar.
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     NewSpeed(AX)    -   The speed of motors to be set.
;      Angle(BX)      -   The direction angle of motors to be set. 
; Shared Variables:  
;      ArgSign     -  [Read]  -  The sign of the argument for action functions.
;      ArgNum      -  [Read]  -  The value of the argument for action functions.                            
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, Flags 
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments

DoSetRelMotorSpeed  PROC    NEAR
                    PUBLIC  DoSetRelMotorSpeed
                    
    PUSH BX                     ;Save BX since BX's value has to be used as an index for 
                                ;finding the next state in ParseSerialChar.           
ParserGetMotorSpeed:
    CALL   GetMotorSpeed        ;AX = CurrentSpeed [0,65534]
CheckSpeedSign:
    CMP ArgSign, ARG_POSITIVE   ;Is the argument positive?
    JNE SubtractSpeed           ;No, it's negative. So we have to decrease the speed.
    ;JE AddSpeed                ;Yes, it's positive. So we have to add the speed.
AddSpeed:
    ADD AX, ArgNum              ;AX = NewSpeed = CurrentSpeed + ArgNum
    JC SaturateMAXSpeed         ;If a carry was generated due to adding the speeds,
                                ;we have to saturate the speed to MAX_SPEED since it 
                                ;indicates that the calculation result has exceeded the
                                ;maximum value of WORD data.
    CMP AX, MAX_SPEED           ;Does NewSpeed exceed MAX_SPEED?
    JBE SetRelMotorSpeed        ;No. Thus, we don't need to saturate the speed.
                                ;Go update the speed.
    ;JG SaturateMAXSpeed        ;Yes. Thus, we have to saturate the speed to MAX_SPEED
                                ;before updating the speed.
SaturateMAXSpeed:
    MOV AX, MAX_SPEED           ;Saturate the NewSpeed to MAX_SPEED.
    JMP SetRelMotorSpeed        ;Now that we are done with calculating the NewSpeed,
                                ;let's go update the speed.
SubtractSpeed:
    NEG ArgNum                  ;Since we are going to subtract speed, we need to first
                                ;get the absolute value of negative ArgNum.
    SUB AX, ArgNum              ;New Speed = CurrentSpeed - Abs(ArgNum)
    JC SaturateMINSpeed         ;If a carry was borrowed for the subtraction, it means
                                ;that the calculated speed was below zero. Thus, we have 
                                ;to saturate the speed to MIN_SPEED.
    CMP AX, MIN_SPEED           ;Is NewSpeed below MIN_SPEED?
    JAE SetRelMotorSpeed        ;No. Thus, we don't need to saturate the NewSpeed to 
                                ;MIN_SPEED. Go update the speed.
    ;JB SetRelMotorSpeed        ;YES. Thus, we have to saturate the speed to MIN_SPEED
                                ;before updating the speed.
SaturateMINSpeed:
    MOV AX, MIN_SPEED           ;Saturate the NewSpeed to MIN_SPEED. 
    JMP SetRelMotorSpeed        ;We're done with calculating NewSpeed.
                                ;Thus, go update the speed.

SetRelMotorSpeed:
    MOV BX, IGNORE_ANGLE        ;Since we are updating only the speed, we should keep the 
                                ;direction unchanged.
                                ;AX = NewSpeed BX = IGNORE_ANGLE
    CALL SetMotorSpeed            ;Call SetMotorSpeed to update the speed.
                                
EndDoSetRelMotorSpeed:
    MOV AX, PARSER_SUCCESS      ;We're done with setting relative motor speed.
                                ;Thus, return PARSER_SUCCESS
    CALL ResetHandle            ;Since the action function is over, we have to reset ArgNum
                                ;and ArgSign to execute the functions of next transition.
    POP BX                      ;Retrieve BX to find the next state in ParseSerialChar.
    RET                         ;End of SetRelMotorSpeed
DoSetRelMotorSpeed  ENDP    


; DoSetMotorDirection
;
; Description:
;      It increases or decreases the current angle of RoboTrike's direction by the degree
;      of argument value. It returns PARSER_SUCCESS when setting angle is done.
; Operation:  
;      It first calls GetMotorDirection to obtain the current direction angle of RoboTrike.
;      Then, it starts normalizing ArgNum. If ArgNum is positive, it normalizes ArgNum by
;      "ArgNum = ArgNum mod FULL_ANGLE". If ArgNum is negative, it normalizes ArgNum by 
;      "ArgNum = FULL_ANGLE - (abs(ArgNum) mod FULL_ANGLE)". When the normalization is done,
;      it adds ArgNum to the current angle of RoboTrike. Since ArgNum is normalized to the 
;      range of [SRAIGHT_ANGLE, FULL_ANGLE-1], we don't need to subtract ArgNum from the 
;      current angle. Then, it sets AX(speed) to IGNORE_SPEED and calls SetMotorSpeed to 
;      update the direction of RoboTrike. Finally, it returns PARSER_SUCCESS in AX and calls 
;      ResetHandle to reset ArgNum and ArgSign.
;      Note that BX's value is pushed in the beginning and popped in the end to keep its
;      value unchnaged. This is because BX's value has to be used as an index for finding
;      the next state in ParseSerialChar.    
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX)      =   PARSER_SUCCESS
; Local Variables:   
;     Speed(AX)                -   The speed of motors to be set.
;      NormalizedArgNum(DX)    -   Normalized value of ArgNum
;      NewAngle(BX)            -   The direction angle of motors to be set. 
; Shared Variables:  
;      ArgSign     -  [Read]   -   The sign of the argument for action functions.
;      ArgNum      -  [R/W]    -   The value of the argument for action functions.                                 
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, CX, DX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments 

DoSetMotorDirection PROC    NEAR
                    PUBLIC  DoSetMotorDirection
                    
    PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                            ;finding the next state in ParseSerialChar.     
SetParserMotorDirection:
    CALL GetMotorDirection  ;Obtain the current direction of motors in AX.
    MOV CX, AX              ;Save the current direction in CX.
    
    MOV AX, ArgNum          ;Start normalizing ArgNum before setting the direction.
                            ;Save ArgNum in AX to start normalization.
ParserNormAngle:
    CMP AX, STRAIGHT_ANGLE  ;Is ArgNum negative?
    JL ParserNormNegAngle   ;Yes. Start normalizing negative angle
;   JGE NormPosAngle        ;No. Start normalizing positive angle
ParserNormPosAngle:
    MOV BX, FULL_ANGLE      ;Divisor(BX)  = FULL_ANGLE
                            ;Dividend(AX) = ArgNum
    XOR DX,DX               ;Clear DX for DIV instruction
    DIV BX                  ;DX = ArgNum mod FULL_ANGLE
    MOV ArgNum, DX          ;Update ArgNum with the normalized ArgNum
    JMP ParserUpdateDirection    ;Now that we're done with normalizing ArgNum, 
                                 ;go update the direction.
ParserNormNegAngle:
    NEG AX                  ;Get the absolute value of negative ArgNum.
    MOV BX, FULL_ANGLE      ;Divisor(BX)  = FULL_ANGLE
                            ;Dividend(AX) = ArgNum
    XOR DX,DX               ;Clear DX for DIV instruction.
    DIV BX                  ;DX = abs(ArgNum) mod FULL_ANGLE
    NEG DX                  ;DX = - (abs(ArgNum) mod FULL_ANGLE)
 
    ADD DX, FULL_ANGLE      ;DX = normalized angle = FULL_ANGLE-(abs(angle) mod FULL_ANGLE)
    MOV ArgNum, DX          ;update ArgNum with the noramlized angle.        
ParserUpdateDirection:
    MOV BX, CX              ;Retrive current direction.
    ADD BX, ArgNum          ;Whether ArgNum was negative or positive, it is now normalized to 
                            ;non-negative range, [STRAIGHT_ANGLE, FULL_ANGLE-1]. Thus, just 
                            ;adding normalized ArgNum to current direction will set the new 
                            ;direction appropriately.
                            ;NewAngle = Current Direction + Normalized ArgNum
ParserSetMotorDirection:    
    MOV AX, IGNORE_SPEED    ;Since this function is changing the direction, we should not
                            ;change the speed. Thus, set the speed argument to IGNORE_SPEED.
                            ;AX = IGNORE_SPEED BX = NewAngle
    CALL SetMotorSpeed      ;Call SetMotorSpeed to update the direction.
                        
EndDoSetMotorDirection:
    MOV AX, PARSER_SUCCESS  ;We're done with increasing or decreasing the motors angle.
                            ;Thus, return PARSER_SUCCESS
    CALL ResetHandle        ;Since the action function is over, we have to reset ArgNum
                            ;and ArgSign to execute the functions of next transition.
    POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
    RET                     ;End of DoSetMotorDirection
DoSetMotorDirection ENDP



; DoSetLaserOn
;
; Description:
;      Turns the laser on and returns PARSER_SUCCESS
; Operation:  
;      It sets AX to LASER_ON and calls SetLaser to turn the laser on.
;      Then, it returns PARSER_SUCCESS in AX and calls ResetHandle to reset ArgNum and
;      ArgSign. Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;      finding the next state in ParseSerialChar.
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     LaserArg(AX)        -    Indicates whether laser should be turned on or not.
;                              It is an argument for SetLaser.
; Shared Variables:  
;      None                             
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments 
DoSetLaserOn    PROC    NEAR
                PUBLIC  DoSetLaserOn
                
    PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                            ;finding the next state in ParseSerialChar.  
TurnLaserOn:
    MOV AX, LASER_ON        ;AX is an argument for SetLaser function.
                            ;Set it to LASER_ON.
    CALL SetLaser           ;Calls SetLaser to turn the laser on.
EndDoSetLaserOn:
    MOV AX, PARSER_SUCCESS  ;Since this action function was handled successfully, 
                            ;return PARSER_SUCCESS.
    CALL ResetHandle        ;Resets ArgNum and ArgSign for executing the functions of
                            ;next transition.
    
    POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
    RET                     ;End of DoSetLaserOn
DoSetLaserOn    ENDP


; DoSetLaserOff
;
; Description:
;      Turns the laser off and returns PARSER_SUCCESS
; Operation:  
;      It sets AX to LASER_OFF and calls SetLaser to turn the laser off.
;      Then, it returns PARSER_SUCCESS in AX and calls ResetHandle to reset ArgNum and
;      ArgSign. Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;     finding the next state in ParseSerialChar.
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX) =    PARSER_SUCCESS
; Local Variables:   
;     LaserArg(AX)       -    Indicates whether laser should be turned on or not.
;                              It is an argument for SetLaser.
; Shared Variables:  
;      None                             
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments     
DoSetLaserOff    PROC    NEAR
                 PUBLIC  DoSetLaserOff   
                 
    PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                            ;finding the next state in ParseSerialChar.
TurnLaserOff:
    MOV AX, LASER_OFF       ;AX is an argument for SetLaser function.
                            ;Set it to LASER_OFF to turn laser off.
    CALL SetLaser           ;Calls SetLaser to turn the laser off.
EndDoSetLaserOff:
    MOV AX, PARSER_SUCCESS  ;Since this action function was handled successfully, 
                            ;return PARSER_SUCCESS.
    CALL ResetHandle        ;Resets ArgNum and ArgSign for executing the functions of
                            ;next transition.
    POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
    RET                     ;End of DoSetLaserOff
DoSetLaserOff    ENDP


; DoSetAbsTurretAngle
;
; Description:
;      Sets the turret's absolute angle and return PARSER_SUCCESS.
; Operation:  
;      It sets AX to ArgNum, which is the absolute angle of the turret to be set.
;      Then, it calls SetTurretAngle to set the absolute angle of the turret.
;      Finally, it returns PARSER_SUCCESS in AX and calls ResetHandle to reset ArgNum and
;      ArgSign. Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;     finding the next state in ParseSerialChar.
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     TurretAngle(AX)   -   Angle of Turret to be set.
;                           Argument for SetTurretAngle
; Shared Variables:  
;      ArgNum      -  [Read]  -  The value of the argument for action functions.                                
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments   

DoSetAbsTurretAngle    PROC    NEAR
                       PUBLIC  DoSetAbsTurretAngle
    PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                            ;finding the next state in ParseSerialChar.
ParserSetTurretAngle:
    MOV AX, ArgNum          ;Set AX to ArgNum, which is the angle of turret to be set, as 
                            ;an argument for SetTurretAngle
    CALL SetTurretAngle     ;Call SetTurretAngle with the argument AX(=ArgNum) to set 
                            ;the turret's angle.
EndDoSetAbsTurretAngle:
    MOV AX, PARSER_SUCCESS  ;Since this action function was handled successfully, 
                            ;return PARSER_SUCCESS.
    CALL ResetHandle        ;Resets ArgNum and ArgSign for executing the functions of
                            ;next transition.
    POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
    RET                     ;End of DoSetAbsTurretAngle
DoSetAbsTurretAngle    ENDP

                       

; DoSetRelTurretAngle
;
; Description:
;      Sets the turret's relative angle and return PARSER_SUCCESS.
; Operation:  
;      It sets AX to ArgNum, which is the relative angle of the turret to be set.
;      Then, it calls SetRelTurretAngle to set the relative angle of the turret.
;      Finally, it returns PARSER_SUCCESS in AX and calls ResetHandle to reset ArgNum and
;      ArgSign. Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;     finding the next state in ParseSerialChar.
; Arguments:  
;       None  
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     TurretRelAngle(AX)     -    Relative angle of Turret to be set.
;                                 Argument for SetRelTurretAngle
; Shared Variables:  
;      ArgNum      -  [Read] -    The value of the argument for action functions.                                
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments   
DoSetRelTurretAngle    PROC    NEAR
                       PUBLIC  DoSetRelTurretAngle
    PUSH BX                 ;Save BX since BX's value has to be used as an index for 
                            ;finding the next state in ParseSerialChar.
ParserSetRelTurretAngle:
    MOV AX, ArgNum          ;Set AX to ArgNum, which is the relative angle of turret to 
                            ;be set as an argument for SetRelTurretAngle
    CALL SetRelTurretAngle  ;Call SetRelTurretAngle with the argument AX(=ArgNum) to set 
                            ;the turret's relative angle.
EndDoSetRelTurretAngle:
    MOV AX, PARSER_SUCCESS  ;Since this action function was handled successfully, 
                            ;return PARSER_SUCCESS.
    CALL ResetHandle        ;Resets ArgNum and ArgSign for executing the functions of
                            ;next transition.
    POP BX                  ;Retrieve BX to find the next state in ParseSerialChar.
    RET                     ;End of DoSetRelTurretAngle
DoSetRelTurretAngle    ENDP


; DoSetTurretElevation
;
; Description:
;      Sets the elevation angle of the turret. If the elevation angle is not in the range of
;      [NEG_TURRET_ELEV_BOUND, POS_TURRET_ELEV_BOUND], it returns PARSER_ERROR.
; Operation:  
;      First, it checks if ArgNum is in the range of [NEG_TURRET_ELEV_BOUND, POS_TURRET_ELEV
;      _BOUND]. If it is not, returns PARSER_ERROR in AX and exits the procdeure.
;      If it is in the range of [NEG_TURRET_ELEV_BOUND, POS_TURRET_ELEV_BOUND], it sets
;      AX to ArgNum and calls SetTurretElevation to set the turret elevation angle.
;      Finally, it returns PARSER_SUCCESS in AX and calls ResetHandle to reset ArgNum and
;      ArgSign. Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;     finding the next state in ParseSerialChar.
; Arguments:  
;     None
; Return Value:   
;     ParserErrorFlag(AX)    -    PARSER_SUCCESS  if there's no parsing error
;                            -    PARSER_ERROR    if there's a parsing error
; Local Variables:   
;     ElevationAngle(AX)     -    The angle of turret elevation
; Shared Variables:  
;      ArgNum      -  [Read] -    The value of the argument for action functions.                                
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     It returns PARSER_ERROR if the elevation angle is not in the range of 
;      [NEG_TURRET_ELEV_BOUND, POS_TURRET_ELEV_BOUND]. 
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine
; Registers Changed: 
;     AX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments  
 
DoSetTurretElevation   PROC   NEAR
                       PUBLIC  DoSetTurretElevation 

    PUSH BX            ;Save BX since BX's value has to be used as an index for 
                       ;finding the next state in ParseSerialChar.
CheckTurretPosBound:
    CMP ArgNum, POS_TURRET_ELEV_BOUND     ;Does ArgNum(Turret Elevation Angle) exceed
                                          ;POS_TURRET_ELEV_BOUND?
    JG TurretElevationError               ;Yes. The ArgNum is illegal. Thus, go return
                                          ;the PARSER_ERROR.
   ;JLE CheckTurretNegBound               ;No. Now check the lower boundary.
CheckTurretNegBound:
    CMP ArgNum, NEG_TURRET_ELEV_BOUND     ;Is ArgNum(Turret Elevation Angle) less than
                                          ;NEG_TURRET_ELEV_BOUND?
    JL  TurretElevationError              ;Yes. The ArgNum is illegal. Thus, go return
                                          ;the PARSER_ERROR.
   ;JGE ParserSetTurretElevation          ;No. The ArgNum is legal. Thus, go set 
                                          ;the turret elevation angle.
ParserSetTurretElevation:          
    MOV AX, ArgNum                        ;Set AX to ArgNum(Turret Elevation Angle) as 
                                          ;an argument for SetTurretElevation
    CALL SetTurretElevation               ;Call SetTurretElevation to set the elvation 
                                          ;angle of the turret.
TurretElevationSuccess:
    MOV AX, PARSER_SUCCESS                ;Since this action function was handled
                                          ;successfully, return PARSER_SUCCESS.
    JMP EndParserSetTurretElevation       ;Go reset ArgNum and ArgSign for next transitions
    
TurretElevationError:
    MOV AX, PARSER_ERROR                  ;Setting turret elevation was not processed 
                                          ;successfully. Thus, return PARSER_ERROR.
EndParserSetTurretElevation:    
    CALL ResetHandle                      ;Resets ArgNum and ArgSign for executing the
                                          ;functions of next transition.
    POP BX                                ;Retrieve BX to find the next state in 
                                          ;ParseSerialChar.
    RET                                   ;End of DoSetTurretElevation
DoSetTurretElevation    ENDP
                       
                     

; DoAddDigit
;
; Description:
;      It adds a digit to the shared variable ArgNum. It returns PARSER_ERROR if an overflow
;      or an underflow occurs while adding the digit. It returns PARSER_SUCCESS if there 
;      was no overflow or underflow.
; Operation:  
;      First, it multiplies current ArgNum by DECIMAL_BASE. Then, if ArgSign is ARG_POSITIVE,
;      it adds the digit to ArgNum. If it is ARG_NEGATIVE, it subtracts the digit from 
;      ArgNum. If an overflow or underflow occurs in the process of addition or subtraction,
;      it returns PARSER_ERROR. If there's no overflow or underfow, it returns PARSER_SUCCESS.
;      Note that BX's value is pushed in the beginning and popped in the end to 
;      keep its value unchnaged. This is because BX's value has to be used as an index for 
;     finding the next state in ParseSerialChar.
; Arguments:  
;     Digit(AL)         -    The digit to be added to ArgNum
; Return Value:   
;     ParserErrorFlag(AX)     -    PARSER_SUCCESS  if there's no parsing error
;                             -    PARSER_ERROR    if there's a parsing error
; Local Variables: 
;      DecMultipledArg(AX)    -    The value of ArgNum multiplied with DECIMAL_BASE  
;     Digit(CX)               -    The new digit to be added to ArgNum
; Shared Variables:  
;      ArgSign     -  [Read]  -    The sign of the argument for action functions.
;      ArgNum      -  [R/W]   -    The value of the argument for action functions.                                      
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     It returns PARSER_ERROR if there's an overflow or underflow in calculation.  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine
; Registers Changed: 
;     AX, CX, DX, Flags
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi   Created
;     11/25/2016    Sunghoon Choi   Initial Compilation
;     11/26/2016    Sunghoon Choi   Revised Comments 

DoAddDigit    PROC    NEAR
            PUBLIC    DoAddDigit
            
    PUSH BX                   ;Save BX since BX's value has to be used as an index for 
                              ;finding the next state in ParseSerialChar.
    MOV CX, AX                ;Save the input digit in CL.    
MultArgNumDec:
    MOV BX, DECIMAL_BASE      ;We are going to multiply ArgNum with DECIMAL_BASEE to add
                              ;a digit to it. Set BX to DECIMAL_BASE
    MOV AX, ArgNum            ;Set AX to ArgNum so that we can multiply ArgNum and
                              ;DECIMAL_BASE
    XOR DX,DX                 ;Clear DX for IMUL instruction.
    IMUL BX                   ;DX:AX = ArgNum * DECIMAL_BASE
    MOV ArgNum, AX            ;Update ArgNum. "ArgNum = ArgNum * DECIMAL_BASE"
    JO    AddDigitError       ;If there was an overflow or underflow by mutliplication,
                              ;return PARSER_ERROR.
;    JNO AddDigitCheckSign    ;If there was no overflow(or underflow), go check ArgSign.
AddDigitCheckSign:
    CMP ArgSign, ARG_POSITIVE   ;Is ArgSign ARG_POSITIVE? (Is ArgNum positive?)
    JNE    SubtractDigitArg     ;No. Subtract the digit from ArgNum.
    ;JE    AddDigitArg          ;Yes. Add the digit to ArgNum.
AddDigitArg:
    XOR BX, BX                  ;Clear BH since we will save the digit in BX.
    MOV BL, CL                  ;Retrieve the digit in BL
    ADD ArgNum, BX              ;"New ArgNum = Decimal Multiplied ArgNum + digit"
    JO    AddDigitError         ;Return PARSER_ERROR if there was an overflow or underflow.
  ;JNO  AddDigitSuccess         ;If there was no overflow or underflow, return the 
    JMP    AddDigitSuccess      ;PARSER_SUCCESS.
SubtractDigitArg:
    XOR BX, BX                  ;Clear BH since we will save the digit in BX.
    MOV BL, CL                  ;Retrieve the digit in BL
    SUB ArgNum, BX              ;"New ArgNum = Decimal Multiplied ArgNum - digit"
    JO    AddDigitError         ;Return PARSER_ERROR if there was an overflow or underflow.
  ;JNO  AddDigitSuccess         ;If there was no overflow or underflow, return the    
    JMP    AddDigitSuccess      ;PARSER_SUCCESS.
AddDigitSuccess:
    MOV AX, PARSER_SUCCESS      ;Return PARSER_SUCCESS in AX.
    JMP EndDoAddDigit           ;Exit the procedure.
AddDigitError:
    MOV AX, PARSER_ERROR        ;Return PARSER_ERROR in AX.
EndDoAddDigit:
    POP BX                      ;Retrieve BX to find the next state in ParseSerialChar.                        
    RET
DoAddDigit    ENDP

     

; DoSetError
;
; Description:
;      It returns PARSER_ERROR in AX.
; Operation:  
;      It sets AX to PARSER_ERROR.
; Arguments:  
;     None
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_ERROR
; Local Variables: 
;      None
; Shared Variables:  
;      None                                      
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine
; Registers Changed: 
;     AX
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments                   

DoSetError  PROC    NEAR
            PUBLIC  DoSetError

ParserSetError:
   MOV AX, PARSER_ERROR       ;Returns PARSER_ERROR in AX.
EndDoSetError:
   RET                        ;End of DoSetError.
DoSetError  ENDP


; DoNOP
;
; Description:
;      It does nothing but returns PARSER_SUCCESS
; Operation:  
;      It sets AX to PARSER_SUCCESS
; Arguments:  
;     None
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     None
; Shared Variables:  
;      None                            
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine  
; Registers Changed: 
;     AX
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;      11/24/2016    Sunghoon Choi    Created
;      11/25/2016    Sunghoon Choi    Initial Compilation
;      11/26/2016    Sunghoon Choi    Revised Comments 

DoNOP    PROC    NEAR
         PUBLIC  DoNOP

ParserDoNOP:
   MOV AX, PARSER_SUCCESS        ;Returns PARSER_SUCCESS in AX.
EndDoNOP:
    RET                          ;End of DoNOP
DoNOP    ENDP


; DoSetNegSign
;
; Description:
;      It sets the sign flag of argument, ArgSign, to ARG_NEGATIVE.
; Operation:  
;      It updates ArgSing with ARG_NEGATIVE. Then, it returns PARSER_SUCCESS in AX.
; Arguments:  
;     None
; Return Value:   
;     ParserErrorFlag(AX) = PARSER_SUCCESS
; Local Variables:   
;     None
; Shared Variables:  
;      ArgSign     -    [Write]  -  The sign of the argument for action functions.                                  
; Global Variables:  
;     None  
; Input:    
;     None           
; Output:            
;     None
; Error Handling:   
;     None  
; Algorithms:        
;     None  
; Data Structures:   
;     Finite State Machine 
; Registers Changed: 
;     None
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/24/2016    Sunghoon Choi    Created
;     11/25/2016    Sunghoon Choi    Initial Compilation
;     11/26/2016    Sunghoon Choi    Revised Comments 
DoSetNegSign    PROC    NEAR
             PUBLIC  DoSetNegSign

ParserSetNegSign:
   MOV ArgSign, ARG_NEGATIVE         ;Set ArgSign to ARG_NEGATIVE
EndDoSetNegSign:
   MOV AX, PARSER_SUCCESS            ;Return PARSER_SUCCESS in AX.
   RET
DoSetNegSign   ENDP   
   


; StateTable
;
; Description:      
;      This is the state transition table for the state machine.
;      Each entry consists of the next state and the action for that transition.
; Notes:             
;       This table is declared PRIVATE to prevent other codes accessing the table. 
;       Also, READ ONLY tables should always be in the code segment so that in a standalone 
;       system it will be located in the ROM with the code.
;
; Author:           
;       Sunghoon Choi
; Revision history:    
;      11/24/2016    Sunghoon Choi    Created
;      11/25/2016    Sunghoon Choi    Initial Compilation
;      11/26/2016    Sunghoon Choi    Revised Comments 

TRANSITION_ENTRY        STRUC           ;structure used to define table
    NEXT_STATE   DB      ?              ;the next state for the transition
    ACTION       DW      ?              ;action for the transition
TRANSITION_ENTRY      ENDS


;define a macro to make table a little more readable
;macro just does an offset of the action routine entries to build the STRUC
%*DEFINE(TRANSITION(nxtst, act))  (
    TRANSITION_ENTRY< %nxtst, OFFSET(%act) >
)


StateTable    LABEL    TRANSITION_ENTRY

    ;Current State = ST_INITIAL                        Input Token Type
    %TRANSITION(ST_SPD,     DoNOP)                      ;TOKEN_SPEED
    %TRANSITION(ST_REL_SPD, DoNOP)                     ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_DIR,     DoNOP)                       ;TOKEN_DIRECTION
    %TRANSITION(ST_LSR_ON,    DoNOP)                        ;TOKEN_LASER_ON
    %TRANSITION(ST_LSR_OFF,    DoNOP)                        ;TOKEN_LASER_OFF
    %TRANSITION(ST_TURRET_ANGLE, DoNop)                    ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_TURRET_ELEVATION, DoNop)                ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_DIGIT
    %TRANSITION(ST_INITIAL, DoNOP)                        ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoNOP)                        ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_OTHER
    
    ;Current State = ST_SPD                             Input Token Type
    %TRANSITION(ST_INITIAL,  DoSetError)                ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_SPD_SIGN, DoNOP)                       ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_SPD_DIGIT,DoAddDigit)               ;TOKEN_DIGIT
    %TRANSITION(ST_SPD,      DoNOP)                       ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL,  DoSetError)               ;TOKEN_OTHER
    

    ;Current State = ST_SPD_SIGN                        Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_SPD_DIGIT, DoAddDigit)               ;TOKEN_DIGIT
    %TRANSITION(ST_SPD_SIGN, DoNOP)                       ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_OTHER
    
    
    ;Current State = ST_SPD_DIGIT                        Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL,    DoSetError)                   ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_SPD_DIGIT, DoAddDigit)               ;TOKEN_DIGIT
    %TRANSITION(ST_SPD_DIGIT, DoNOP)                   ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetAbsMotorSpeed)          ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_OTHER
    
    
    ;Current State = ST_REL_SPD                       Input Token Type
    %TRANSITION(ST_INITIAL,  DoSetError)              ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_REL_SPD_SIGN, DoNOP)                  ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_REL_SPD_SIGN, DoSetNegSign)          ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_REL_SPD_DIGIT, DoAddDigit)          ;TOKEN_DIGIT
    %TRANSITION(ST_REL_SPD, DoNOP)                      ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_OTHER

    ;Current State = ST_REL_SPD_SIGN                   Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_REL_SPD_DIGIT, DoAddDigit)          ;TOKEN_DIGIT
    %TRANSITION(ST_REL_SPD_SIGN, DoNOP)                  ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_OTHER
    
    
    ;Current State = ST_REL_SPD_DIGIT                  Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_REL_SPD_DIGIT, DoAddDigit)          ;TOKEN_DIGIT
    %TRANSITION(ST_REL_SPD_DIGIT, DoNOP)              ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetRelMotorSpeed)          ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_OTHER



    ;Current State = ST_DIR                              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_DIR_SIGN, DoNOP)                     ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_DIR_SIGN, DoSetNegSign)             ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_DIR_DIGIT, DoAddDigit)             ;TOKEN_DIGIT
    %TRANSITION(ST_DIR, DoNOP)                         ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER

    
    ;Current State = ST_DIR_SIGN                      Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_DIR_DIGIT, DoAddDigit)             ;TOKEN_DIGIT
    %TRANSITION(ST_DIR_SIGN, DoNOP)                     ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                   ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER


    
    ;Current State = ST_DIR_DIGIT                      Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_DIR_DIGIT, DoAddDigit)             ;TOKEN_DIGIT
    %TRANSITION(ST_DIR_DIGIT, DoNOP)                 ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetMotorDirection)     ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER

    ;Current State = ST_LSR_ON                          Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIGIT
    %TRANSITION(ST_LSR_ON,  DoNOP)                     ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetLaserOn)            ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER

    ;Current State = ST_LSR_OFF                      Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIGIT
    %TRANSITION(ST_LSR_OFF,  DoNOP)                     ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetLaserOff)            ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER

    ;Current State = ST_TURRET_ANGLE                  Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_TURRET_ANGLE_SIGN, DoNOP)         ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_TURRET_ANGLE_SIGN, DoSetNegSign)     ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_ABS_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ANGLE,  DoNOP)             ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER

    
    ;Current State = ST_TURRET_ABS_DIGIT              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                    ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                  ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_ABS_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ABS_DIGIT,  DoNOP)         ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetAbsTurretAngle)     ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER    
    
    
    ;Current State = ST_TURRET_ANGLE_SIGN              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_REL_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ANGLE_SIGN, DoNOP)         ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER
    
    
    ;Current State = ST_TURRET_REL_DIGIT              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_REL_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_REL_DIGIT, DoNOP)             ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetRelTurretAngle)     ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER    
    
    
    
    ;Current State = ST_TURRET_ELEVATION              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_TURRET_ELEV_SIGN, DoNOP)             ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_TURRET_ELEV_SIGN, DoSetNegSign)     ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_ELEV_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ELEVATION, DoNOP)              ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                  ;TOKEN_OTHER    
    
    
    ;Current State = ST_TURRET_ELEV_SIGN              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_ELEV_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ELEV_SIGN, DoNOP)             ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetError)                ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER    
    
    ;Current State = ST_TURRET_ELEV_DIGIT              Input Token Type
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SPEED
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_RELATIVE_SPEED
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_DIRECTION
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_ON
    %TRANSITION(ST_INITIAL,    DoSetError)                 ;TOKEN_LASER_OFF
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ANGLE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_TURRET_ELEVATION
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_POSITIVE
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_SIGN_NEGATIVE
    %TRANSITION(ST_TURRET_ELEV_DIGIT, DoAddDigit)     ;TOKEN_DIGIT
    %TRANSITION(ST_TURRET_ELEV_DIGIT, DoNOP)         ;TOKEN_IGNORE
    %TRANSITION(ST_INITIAL, DoSetTurretElevation)     ;TOKEN_RETURN
    %TRANSITION(ST_INITIAL, DoSetError)                 ;TOKEN_OTHER        

    
; Token Tables
;
; Description:      
;       This creates the tables of token types and token values.
;      Each entry corresponds to the token type and the token value for a character.  
;       Macros are used to actually build two separate tables - TokenTypeTable for token 
;       types and TokenValueTable for token values.
; Notes:            
;       This table is declared PRIVATE to prevent other codes accessing the table. 
;       Also, READ ONLY tables should always be in the code segment so that in a standalone 
;       system it will be located in the ROM with the code.
; Author:          
;       Sunghoon Choi
; Revision history:    
;      11/24/2016   Sunghoon Choi   Created
;       11/25/2016    Sunghoon Choi    Initial Compilation
;       11/26/2016    Sunghoon Choi    Revised Comments 

%*DEFINE(TABLE)  (
        %TABENT(TOKEN_OTHER, 0)                ;<null> 
        %TABENT(TOKEN_OTHER, 1)                ;SOH
        %TABENT(TOKEN_OTHER, 2)                ;STX
        %TABENT(TOKEN_OTHER, 3)                ;ETX
        %TABENT(TOKEN_OTHER, 4)                ;EOT
        %TABENT(TOKEN_OTHER, 5)                ;ENQ
        %TABENT(TOKEN_OTHER, 6)                ;ACK
        %TABENT(TOKEN_OTHER, 7)                ;BEL
        %TABENT(TOKEN_OTHER, 8)                ;backspace
        %TABENT(TOKEN_IGNORE, 9)            ;TAB (TOKEN_IGNORE)
        %TABENT(TOKEN_OTHER, 10)            ;new line
        %TABENT(TOKEN_OTHER, 11)            ;vertical tab
        %TABENT(TOKEN_OTHER, 12)            ;form feed
        %TABENT(TOKEN_RETURN, 13)            ;carriage return
        %TABENT(TOKEN_OTHER, 14)            ;SO
        %TABENT(TOKEN_OTHER, 15)            ;SI
        %TABENT(TOKEN_OTHER, 16)            ;DLE
        %TABENT(TOKEN_OTHER, 17)            ;DC1
        %TABENT(TOKEN_OTHER, 18)            ;DC2
        %TABENT(TOKEN_OTHER, 19)            ;DC3
        %TABENT(TOKEN_OTHER, 20)            ;DC4
        %TABENT(TOKEN_OTHER, 21)            ;NAK
        %TABENT(TOKEN_OTHER, 22)            ;SYN
        %TABENT(TOKEN_OTHER, 23)            ;ETB
        %TABENT(TOKEN_OTHER, 24)            ;CAN
        %TABENT(TOKEN_OTHER, 25)            ;EM
        %TABENT(TOKEN_OTHER, 26)            ;SUB
        %TABENT(TOKEN_OTHER, 27)            ;escape
        %TABENT(TOKEN_OTHER, 28)            ;FS
        %TABENT(TOKEN_OTHER, 29)            ;GS
        %TABENT(TOKEN_OTHER, 30)            ;AS
        %TABENT(TOKEN_OTHER, 31)            ;US
        %TABENT(TOKEN_IGNORE, ' ')            ;space
        %TABENT(TOKEN_OTHER, '!')            ;!
        %TABENT(TOKEN_OTHER, '"')            ;"
        %TABENT(TOKEN_OTHER, '#')            ;#
        %TABENT(TOKEN_OTHER, '$')            ;$
        %TABENT(TOKEN_OTHER, 37)            ;percent
        %TABENT(TOKEN_OTHER, '&')            ;&
        %TABENT(TOKEN_OTHER, 39)            ;'
        %TABENT(TOKEN_OTHER, 40)            ;open paren
        %TABENT(TOKEN_OTHER, 41)            ;close paren
        %TABENT(TOKEN_OTHER, '*')            ;*
        %TABENT(TOKEN_SIGN_POSITIVE, +1)       ;+ (positive sign)
        %TABENT(TOKEN_OTHER, 44)            ;,
        %TABENT(TOKEN_SIGN_NEGATIVE, -1)       ;- (negative sign)
        %TABENT(TOKEN_OTHER, '.')               ;. (decimal point)
        %TABENT(TOKEN_OTHER, '/')               ;/
        %TABENT(TOKEN_DIGIT, 0)                   ;0 (digit)
        %TABENT(TOKEN_DIGIT, 1)                   ;1 (digit)
        %TABENT(TOKEN_DIGIT, 2)                   ;2 (digit)
        %TABENT(TOKEN_DIGIT, 3)                   ;3 (digit)
        %TABENT(TOKEN_DIGIT, 4)                   ;4 (digit)
        %TABENT(TOKEN_DIGIT, 5)                   ;5 (digit)
        %TABENT(TOKEN_DIGIT, 6)                   ;6 (digit)
        %TABENT(TOKEN_DIGIT, 7)                   ;7 (digit)
        %TABENT(TOKEN_DIGIT, 8)                   ;8 (digit)
        %TABENT(TOKEN_DIGIT, 9)                   ;9 (digit)
        %TABENT(TOKEN_OTHER, ':')               ;:
        %TABENT(TOKEN_OTHER, ';')               ;;
        %TABENT(TOKEN_OTHER, '<')               ;<
        %TABENT(TOKEN_OTHER, '=')               ;=
        %TABENT(TOKEN_OTHER, '>')               ;>
        %TABENT(TOKEN_OTHER, '?')               ;?
        %TABENT(TOKEN_OTHER, '@')               ;@
        %TABENT(TOKEN_OTHER, 'A')               ;A
        %TABENT(TOKEN_OTHER, 'B')               ;B
        %TABENT(TOKEN_OTHER, 'C')              ;C
        %TABENT(TOKEN_DIRECTION, 'D')           ;D (Set Direction)
        %TABENT(TOKEN_TURRET_ELEVATION, 0)  ;E (Set Turret Elevation)
        %TABENT(TOKEN_LASER_ON, 'F')        ;F (Turn Laser On)
        %TABENT(TOKEN_OTHER, 'G')            ;G
        %TABENT(TOKEN_OTHER, 'H')            ;H
        %TABENT(TOKEN_OTHER, 'I')            ;I
        %TABENT(TOKEN_OTHER, 'J')            ;J
        %TABENT(TOKEN_OTHER, 'K')            ;K
        %TABENT(TOKEN_OTHER, 'L')            ;L
        %TABENT(TOKEN_OTHER, 'M')            ;M
        %TABENT(TOKEN_OTHER, 'N')            ;N
        %TABENT(TOKEN_LASER_OFF, 'O')        ;O (Turn Laser Off)
        %TABENT(TOKEN_OTHER, 'P')            ;P
        %TABENT(TOKEN_OTHER, 'Q')            ;Q
        %TABENT(TOKEN_OTHER, 'R')            ;R
        %TABENT(TOKEN_SPEED, 'S')            ;S (Set Absolute Speed)
        %TABENT(TOKEN_TURRET_ANGLE, 'T')    ;T (Set Turret Angle)
        %TABENT(TOKEN_OTHER, 'U')            ;U
        %TABENT(TOKEN_RELATIVE_SPEED, 'V')    ;V (Set Relative Speed)
        %TABENT(TOKEN_OTHER, 'W')            ;W
        %TABENT(TOKEN_OTHER, 'X')            ;X    
        %TABENT(TOKEN_OTHER, 'Y')            ;Y
        %TABENT(TOKEN_OTHER, 'Z')            ;Z
        %TABENT(TOKEN_OTHER, '[')            ;[
        %TABENT(TOKEN_OTHER, '\')            ;\
        %TABENT(TOKEN_OTHER, ']')            ;]
        %TABENT(TOKEN_OTHER, '^')            ;^
        %TABENT(TOKEN_OTHER, '_')            ;_ 
        %TABENT(TOKEN_OTHER, '`')            ;`
        %TABENT(TOKEN_OTHER, 'a')            ;a
        %TABENT(TOKEN_OTHER, 'b')            ;b
        %TABENT(TOKEN_OTHER, 'c')            ;c
        %TABENT(TOKEN_DIRECTION, 'd')        ;d (Set Direction)
        %TABENT(TOKEN_TURRET_ELEVATION, 'e');e (Set Turret Elevation)
        %TABENT(TOKEN_LASER_ON, 'f')        ;f (Turn Laser On)
        %TABENT(TOKEN_OTHER, 'g')            ;g
        %TABENT(TOKEN_OTHER, 'h')            ;h
        %TABENT(TOKEN_OTHER, 'i')            ;i
        %TABENT(TOKEN_OTHER, 'j')            ;j
        %TABENT(TOKEN_OTHER, 'k')            ;k
        %TABENT(TOKEN_OTHER, 'l')            ;l
        %TABENT(TOKEN_OTHER, 'm')            ;m
        %TABENT(TOKEN_OTHER, 'n')            ;n
        %TABENT(TOKEN_LASER_OFF, 'o')        ;o (Turn Laser Off)
        %TABENT(TOKEN_OTHER, 'p')            ;p
        %TABENT(TOKEN_OTHER, 'q')            ;q
        %TABENT(TOKEN_OTHER, 'r')            ;r
        %TABENT(TOKEN_SPEED, 's')            ;s (Set Absolute Speed)
        %TABENT(TOKEN_TURRET_ANGLE, 't')    ;t (Set Turret Angle)
        %TABENT(TOKEN_OTHER, 'u')            ;u
        %TABENT(TOKEN_RELATIVE_SPEED, 'v')    ;v (Set Relative Speed)
        %TABENT(TOKEN_OTHER, 'w')            ;w
        %TABENT(TOKEN_OTHER, 'x')            ;x
        %TABENT(TOKEN_OTHER, 'y')            ;y
        %TABENT(TOKEN_OTHER, 'z')            ;z
        %TABENT(TOKEN_OTHER, '{')            ;{
        %TABENT(TOKEN_OTHER, '|')            ;|
        %TABENT(TOKEN_OTHER, '}')            ;}
        %TABENT(TOKEN_OTHER, '~')            ;~
        %TABENT(TOKEN_OTHER, 127)            ;rubout
)

; token type table - uses first byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokentype
)

TokenTypeTable    LABEL   BYTE
        %TABLE


; token value table - uses second byte of macro table entry
%*DEFINE(TABENT(tokentype, tokenvalue))  (
        DB      %tokenvalue
)

TokenValueTable    LABEL       BYTE
        %TABLE   
   
CODE ENDS
   
DATA SEGMENT PUBLIC 'DATA'
    ArgNum        DW      ?    ;The argument value for the action functions
    ArgSign        DB        ? ;Sign of ArgNum
    ParserState DB        ?    ;Current state of the parsing state machine.                    
DATA ENDS
                
END