NAME Motors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   Motors                                   ;
;                                  Homework 6                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Description:
;     This file contains the functions necessary for handling DC motors and laser.
;
; Table of Contents:
;    InitMotorLaser    -    Initializes all variables and arrays related to Motors routine.
;    SetMotorSpeed     -    Sets the speed and angle of motors of RoboTrike.
;    GetMotorSpeed     -    Gets the current speed setting of RoboTrike.
;    GetMotorDirection -    Gets the current direction of movement setting for RoboTrike.
;    SetLaser          -    Sets the laser setting to turn it on or turn it off.
;    GetLaser          -    Gets the current laser status of RoboTrike.
;   MotorLaserEventHandler   - Output values to parallel port B to control motors and laser.
;                            - It is called by Timer1 of 4KHz frequency.
;
;
; Revision History:
;     11/6/2016    Sunghoon Choi      Created
;     11/7/2016    Sunghoon Choi      Corrected syntax error for Force tables.
;     11/8/2016    Sunghoon Choi      Initial Compilation
;     11/11/2016   Sunghoon Choi      Updated documentation


    
$INCLUDE(Motors.inc)        ;Include the.inc file which contains constatns for Motors.asm

EXTRN   Sin_Table:NEAR      ;import Sin_Table to calculate the dot product of force and
                            ;velocity.
EXTRN   Cos_Table:NEAR      ;import Cos_Table to calculate the dot product of force and
                            ;velocity.

CGROUP GROUP CODE
DGROUP GROUP DATA

CODE SEGMENT PUBLIC 'CODE'

        ASSUME  CS:CGROUP, DS:DGROUP
        
; XForces
;
; Description:      
;        This is the table for X component of force vectors of each motors.
;        The values are in Q0.15 format.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:      Sunghoon Choi
; Revision history:    11/6/2016        Sunghoon Choi    Created
;                      11/7/2016        Sunghoon Choi    Corrected syntax error
;                      11/11/2016       Sunghoon Choi    Updated documentation        
                
XForces    LABEL   WORD                
          
    DW    07FFFH    ;XForce_Motor1
    DW    0C000H    ;XForce_Motor2
    DW    0C000H    ;XForce_Motor3
    

; YForces
;
; Description:      
;        This is the table for Y component of force vectors of each motors.
;        The values are in Q0.15 format.
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:       Sunghoon Choi
; Revision history:    11/6/2016        Sunghoon Choi    Created
;                      11/7/2016        Sunghoon Choi    Corrected syntax error
;                      11/11/2016       Sunghoon Choi    Updated documentation            
    
YForces     LABEL  WORD
           
    DW    00000H    ;YForceMotor1
    DW    09127H    ;YForceMotor2
    DW    06ED9H    ;YForceMotor3
    
    
; BackDirMask
;
; Description:      
;        This is a table of the masks to be used to reverse the directions of motors.
;
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:       Sunghoon Choi
; Revision history:    11/6/2016        Sunghoon Choi    Created
;                      11/7/2016        Sunghoon Choi    Corrected syntax error
;                      11/11/2016       Sunghoon Choi    Updated documentation         
    
BackDirMask     LABEL  BYTE         
                
    DB 00000001B       ;ReverseMaskMotor1
    DB 00000100B       ;ReverseMaskMotor2
    DB 00010000B       ;ReverseMaskMotor3

; TurnOnMask
;
; Description:      
;        This is a table of the masks to be used to turn on each motors.
;
; Notes:            
;        This table is declared PRIVATE to prevent other codes accessing the table. 
;        Also, READ ONLY tables should always be in the code segment so that in a standalone 
;        system it will be located in the ROM with the code.
;
; Author:       Sunghoon Choi
; Revision history:    11/6/2016        Sunghoon Choi    Created
;                      11/7/2016        Sunghoon Choi    Corrected syntax error
;                      11/11/2016       Sunghoon Choi    Updated documentation
         
TurnOnMask      LABEL  BYTE         
                
    DB 00000010B       ;TurnOnMaskMotor1
    DB 00001000B       ;TurnOnMaskMotor2
    DB 00100000B       ;TurnOnMaskMotor3

    
; InitMotorLaser
;
; Description:
;      Initializes all variables and arrays related to Motor routine.       
; Operation:  
;      Inserts INIT_PULSE_WIDTH to all elements of pulseWidths by incrementing the
;      array index every loop. It repeats the loop until the index reaches NUM_MOTORS.
;      Inserts INIT_PULSE_COUNTER to pulseWidthCounter.
;      Inserts INIT_SPEED to driveSpeed.
;      Inserts INIT_ANGLE to driveAngle.
;      Inserts INIT_LASER_STAT to laserStatus.
; Arguments:  
;     None
; Return Value:   
;     None
; Local Variables:   
;     MotorIndex(DI)      -    The index of current motor
; Shared Variables:  
;      driveSpeed(DS)     - [Write] - The speed at which the RoboTrike is to move
;      driveAngle(DS)     - [Write] - The angle at which the RoboTrike is to move in degrees
;      pulseWidths(DS)    - [Write] - An array which contains the pulse widths for 
;                                     NUM_MOTORS motors.
;      pulseWidthCounter(DS)  - [Write] - A counter used for Pulse Width Modulation control
;      laserStatus(DS)        - [Write] - Indicates whether the laser is on or off.
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
;     None  
; Registers Changed: 
;     DI, Flags  
; Limitations:
;     None       
; Known bugs:        
;     None   
; Special Notes: 
;     None  
; Author:           
;     Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi      Created
;     11/8/2016    Sunghoon Choi      Initial Compilation
;     11/11/2016   Sunghoon Choi      Updated documentation
        

InitMotorLaser  PROC    NEAR
                PUBLIC  InitMotorLaser

    XOR DI, DI                  ;We begin initializing pulseWidths array with its
                                ;first element.
InitPulseWidths:                
    MOV pulseWidths[DI], INIT_PULSE_WIDTH     ;pulse widths for each motors are
                                              ;initialized to INIT_PULSE_WIDTH.
    INC DI                                    ;Proceed to initialize the pulse width of next motor.
    CMP DI, NUM_MOTORS                        ;Is this the end of pulseWidths array?
    JB  InitPulseWidths                       ;No, go back to the beginning of the loop and 
                                              ;initialize next pulse width.
;   JGE InitPulseWidthCnt                     ;Yes, we are done with initializing pulse widths.
                                              ;Go initialize the pulse width counter.

InitPulseWidthCnt:              
    MOV pulseWidthCounter, INIT_PULSE_COUNTER ;Initialize pulseWidthCounter to
                                              ;INIT_PULSE_COUNTER

InitSpeednAngle:
    MOV driveSpeed, INIT_SPEED         ;Initialize driveSpeed to INIT_SPEED
    MOV driveAngle, INIT_ANGLE         ;Initialize driveAngle to INIT_ANGLE
    
InitLaserStatus:    
    MOV laserStatus, INIT_LASER_STAT   ;Initialize laserStatus to INIT_LASER_STAT
EndInitMotorLaser:
    RET                                ;End of InitMotorLaser procedure.
    
InitMotorLaser   ENDP    
    
    
        
        
        
; SetMotorSpeed
;
; Description:
;    The function is called with two arguments. First argument, speed(unsigned word), 
;    is passed in AX. Speed of IGNORE_SPEED indicates the current speed should not be changed
;    If the speed is not IGNORE_SPEED, driveSpeed will be updated.
;    RoboTrike is at its full speed when the speed is equal to MAX_SPEED.
;    The second argument, angle, is a signed value and is passed in BX.
;    STRAIGHT_ANGLE degree indicates that the Robotrike should move straight ahead relative to
;    RobotTrike orientation while an angle of IGNORE_ANGLE indicates the current direction of
;    travel should not be changed. If the angle is not IGNORE_ANGLE, driveAngle will be updated.
;    The angle will be normalized to the range of [STRAIGHT_ANGLE,FULL_ANGLE-1] and speed will be 
;    normalized to range of [MIN_SPEED,NORM_MAX_SPEED] for dot product calculation in the procedure.           
; Operation:  
;    It first checks if the speed is IGNORE_SPEED. If it is, skip updating driveSpeed.
;    If the speed is not IGNORE_SPEED, it updates driveSpeed with the argument-speed.
;    Then, it normalizes the speed by shifting speed value to right by one bit to make the 
;    highest bit zero. This normalization is needed since the function is going to use IMUL 
;    instruction to multiply the speed with force vectors and cosine or sine 
;    values. Shifting the speed to right by 1 prevents speed being treated as
;    negative value by IMUL while degrading the precision. The range of speed gets halved.
;    It saves the normalized speed in driveSpeed.
;    Now it checks if the angle is IGNORE_ANGLE. If so, skip updating angle.
;    If the angle is not IGNORE_ANGLE, it normalizes the angle to the range of 
;    [STRAIGHT_ANGLE,FULL_ANLGE-1]. If the angle is positive, it executes 
;    "Normalized Angle = angle mod FULL_ANGLE". 
;    If the angle is negative, it executes 
;    "Normalized Angle = FULL_ANGLE - {abs(angle) mod FULL_ANGLE}". 
;    It saves the normalized angle in dirveAngle.
;    Now that we have normalized both speed and angle, the function performs 
;    the dot product of motors' forces and the velocity vectors using the Sin_Table and 
;    Cos_Table as: "DotProductResult = XForces[MotorIndex] * NormSpeed * Cos_Table[TrigIndex]
;   + YForces[MotorIndex] * NormSpeed * Sin_Table[TrigIndex]".
;    Q0.15 fixed point value operations are used for dot product calculation. 
;    Thus, duplicated sign bits are removed at the end of calculation.
;    The calculated pulseWidth is stored in the pulseWidths array. When calculating and 
;    storing the pulseWidth for current motor is done, increment the motor index and repeat 
;    the same process for remaining motors. When it has gone through all NUM_MOTORS motors, 
;    the procedure ends.        
; Arguments:  
;     speed - AX - The absolute(unsigned) speed at which the RoboTrike is to move
;    angle - BX - The signed angle at which the RoboTrike is to move in degrees      
; Return Value:   
;   None
; Local Variables:   
;    speed         - AX - The absolute(unsigned) speed at which the RoboTrike is to move
;    angle         - BX - The signed angle at which the RoboTrike is to move in degrees 
;    TrigIndex     - SI - The index used to obtain cos or sin value for each angle from 
;                         Cos_Tables and Sin_Tables
;    MotorIndex    - DI - The index of current motor    
;    NormSpeed     - AX - The normalized speed.     
; Shared Variables:  
;    pulseWidths(DS) - [Write] - An array which contains the pulse widths for 
;                                NUM_MOTORS motors.
;    driveSpeed(DS)  - [Read/Write] - The speed at which the RoboTrike is to move
;    driveAngle(DS)  - [Read/Write] - The angle at which the RoboTrike is to move in 
;                                     degrees      
; Global Variables:  
;    None    
; Input:    
;    None            
; Output:            
;    None   
; Error Handling:   
;    None  
; Algorithms:        
;    None   
; Data Structures:   
;    None    
; Registers Changed: 
;    AX, BX, CX, DX, DI, SI, Flags   
; Limitations:
;    The precision of speed goes down due to shifting the speed to right by one bit for IMUL
;    instruction. Also, there is a precision loss in dot product due to truncation.        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi      Created
;     11/8/2016    Sunghoon Choi      Initial Compilation
;     11/11/2016   Sunghoon Choi      Updated documentation        
      
SetMotorSpeed   PROC    NEAR
                PUBLIC SetMotorSpeed

    XOR DI, DI           ;MotorIndex is initialized to F_INDEX_MOTOR1 to calculate the 
                         ;dot product of vectors for first motor.
                         ;(For an array, index 0 is the first element)
    XOR SI, SI           ;TrigIndex is initialized to STRAIGHT_ANGLE.
                         ;TrigIndex will be updated to the current angle setting.
CheckIgnoreSpeed:
    CMP AX, IGNORE_SPEED ;Is the speed equal to IGNORE_SPEED?
    JE CheckIgnoreAngle  ;Yes, skip updating the shared variable driveSpeed
                         ;and go check the angle.
;   JNE UpdateDriveSpeed ;No. Update driveSpeed with the argument value.
           
UpdateDriveSpeed:
    MOV driveSpeed, AX   ;Update driveSpeed with the argument-speed's value.

CheckIgnoreAngle:
    CMP BX, IGNORE_ANGLE ;Is the angle IGNORE_ANGLE?
    JE  CalcDotProduct   ;Yes, skip updating driveAngle and start calculating pulse widths.
;   JNE NormAngle        ;No. Start normalizing the angle.
NormAngle:
    CMP BX, STRAIGHT_ANGLE    ;Is the angle negative?
    JL NormNegAngle           ;Normalize negative angle
;   JGE NormPosAngle          ;Normalize positive angle
NormPosAngle:
    MOV AX, BX                ;Set the dividend to angle.
    MOV BX, FULL_ANGLE        ;Set the divisor to FULL_ANGLE.
    XOR DX,DX                 ;Clear DX for DIV instruction.
    DIV BX                    ;DX = normalized angle = angle mod FULL_ANGLE
                              ;Now the normalized angle is in the range of [0,FULL_ANGLE-1]
    MOV driveAngle, DX        ;Update driveAngle with the normalized angle
    JMP CalcDotProduct        ;Now that we're done with normalizing speed and angle, 
                              ;go calculate the pulse widths.
    
NormNegAngle:
    NEG BX                    ;Get the absolute value of the negative angle.
    MOV AX, BX                ;Set the dividend to the absolute value of the angle.
    MOV BX, FULL_ANGLE        ;Set the divisor to FULL_ANGLE
    XOR DX,DX                 ;Clear DX for DIV instruction.
    DIV BX                    ;DX = abs(angle) mod FULL_ANGLE
    NEG DX                    ;DX = - (abs(angle) mod FULL_ANGLE)
 
    ADD DX, FULL_ANGLE        ;DX = normalized angle = FULL_ANGLE-(abs(angle) mod FULL_ANGLE)
    MOV driveAngle, DX        ;update driveAngle with the noramlized angle.
  
    
CalcDotProduct:               ;In this loop, it calculates the pulse widths for each motor
                              ;by calculating the dot product of force and velocity.
                         
    SHL DI,NUM_SHIFT_DOUBLE   ;Double MotorIndex since XForces and YForces tables 
                              ;are WORD tables.
    MOV  AX, driveSpeed       ;Retrieve the updated driveSpeed to AX.
                         
    SHR AX, NUM_SHIFT_ERASE_SIGN            
                                ;Get rid of the sign bit of AX to obtain the absolute value 
                                ;of driveSpeed for normalization.
                                ;Now, the normalized speed, normSpeed, is in the range of
                                ;[STRAIGHT_ANGLE, FULL_ANGLE]
                
    PUSH AX                     ;Save NormSpeed since AX will be changed.
    
    MOV  BX, CS:XForces[DI]     ;Obtain the X component of force vector of current motor
    IMUL BX                     ;DX:AX = Fx * v while
                                ;        Fx = X component of force vector of current motor
                                ;        v = NormSpeed
                                 
    MOV  AX, DX                 ;Truncate to DX for normalization.
                                ;AX = Fx * v
    MOV SI, driveAngle          ;Obtain the current angle setting to do the trigonometric
                                ;calculation. Since driveAngle is in the range of
                                ;[STRAIGHT_ANGLE,FULL_ANGLE-1], we don't need further 
                                ;normalization. Thus, driveAngle = NormAngle
    SHL SI, NUM_SHIFT_DOUBLE    ;Double the angle since Cos table and Sin table are WORD
                                ;tables.
    MOV  BX, CS:Cos_Table[SI]   ;BX = Cos(NormAngle)
    IMUL BX                     ;DX:AX = Fx*v*Cos(NormAngle)        
    MOV CX, DX                  ;Truncate to DX for normalization.
                                ;CX = Fx * v * Cos(NormAngle) = X factor of pulse width
                                
    POP AX                      ;Retrieve NormSpeed
                                                             
    MOV BX, CS:YForces[DI]      ;Obtain the Y component of force vector of current motor
    IMUL BX                     ;DX:AX = Fy * v while
                                ;        Fy = Y component of force vector of current motor
                                ;        v = NormSpeed
    MOV AX, DX                  ;Truncate to DX for normalization.
                                ;AX = Fy*v
    
    MOV BX, CS:Sin_Table[SI]    ;BX = Sin(NormAngle)
    IMUL BX                     ;DX:AX = Fy*v*Sin(NormAngle)
    MOV BX, DX                  ;Truncate to DX for normalization.
                                ;BX = Fy * v * Sin(NormAngle) = Y factor of pulse width

    
    ADD CX, BX                  ;pulseWidth = Fx*v*Cos(NormAngle) + Fy*v*Sin(NormAngle)
    
    SAL CX, DUPSIGN_0DOT15_MUL2 ;Remove duplicated extra sign bits caused by 
                                ;Q0.15 multiplications                                
                                ;Thus, remove the extra sign bits by shifting the result to 
                                ;left by DUPSIGN_0DOT15_MUL2.
    
    SHR DI, NUM_SHIFT_HALF      ;Halve MotorIndex back since pulseWidhts is a BYTE array.
    MOV pulseWidths[DI], CH     ;Truncate the pulseWidth of current motor to CH and 
                                ;save it in the pulseWidths array.
                                
    INC DI                      ;Proceed to handle next motor                 
    
    CMP DI, NUM_MOTORS          ;Is this the last motor?
    JL CalcDotProduct           ;No, handle the next motor.
;   JGE EndSetMotorSpeed        ;Yes, finish SetMotorSpeed

EndSetMotorSpeed:
    Ret                         ;End of SetMotorSpeed procedure.
SetMotorSpeed ENDP    
    

    
; GetMotorSpeed
;
; Description:
;    The function is called with no arguments and returns the current speed setting of 
;    RoboTrike in AX. A speed of MAX_SPEED indicates the maximum speed and a value of 
;    MIN_SPEED indicates that the RoboTrike is stopped.            
; Operation:  
;    Return driveSpeed in AX.    
; Arguments:  
;   None   
; Return Value:   
;   AX(driveSpeed)   -    The speed at which the RoboTrike is to move
; Local Variables:   
;   None 
; Shared Variables:  
;    driveSpeed(DS)  - [Read] -    The speed at which the RoboTrike is to move 
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
;      11/6/2016    Sunghoon Choi      Created
;      11/8/2016    Sunghoon Choi      Initial Compilation
;      11/11/2016   Sunghoon Choi      Updated documentation        


GetMotorSpeed   PROC    NEAR
                PUBLIC  GetMotorSpeed
                
MOV AX, driveSpeed    ;Return driveSpeed to AX
RET                   ;End of GetMotorSpeed procedure
GetMotorSpeed ENDP                
   

   
; GetMotorDirection
;    
; Description:
;    The function is called with no arguments and returns the current direction of movement 
;    setting for the RoboTrike as an angle in degrees in AX. An angle of STRAIGHT_ANGLE indicates 
;    straight ahead relative to the RoboTrike orientation and angles are measured clockwise. 
;    The value returned will always be between STRAIGHT_ANGLE and FULL_ANGLE-1 inclusively.           
; Operation:  
;    Returns driveAngle in AX.    
; Arguments:  
;   None   
; Return Value:   
;   AX(driveAngle)  - The angle at which the RoboTrike is to move in degrees   
; Local Variables:   
;   None 
; Shared Variables:  
;    driveAngle(DS)  - [Read] - The angle at which the RoboTrike is to move in degrees 
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
;    AX   
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi   Created
;     11/8/2016    Sunghoon Choi   Initial Compilation
;     11/11/2016   Sunghoon Choi   Updated documentation                           
   
   
GetMotorDirection   PROC    NEAR
                    PUBLIC  GetMotorDirection
                    
MOV AX, driveAngle         ;Return driveAngle to AX.
RET                        ;End of GetMotorDirection procedure.
GetMotorDirection ENDP                    



; SetLaser
;
; Description:
;    The function is passed a single argument (onoff) in AX that indicates whether to turn 
;    the RoboTrike laser on or off. Value of LASER_OFF turns the laser off and a value
;    other than LASER_OFF turns it on.           
; Operation:  
;    Inserts the value of argument AX into laserStatus variable.    
; Arguments:  
;   laserPowerArg(AX) - The configuration value to turn the laser on or off.  
; Return Value:   
;   None   
; Local Variables:   
;   None  
; Shared Variables:  
;    laserStatus(DS)  - [Write] - Indicates whether the laser is on or off.   
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
;   None    
; Limitations:
;   None         
; Known bugs:        
;   None    
; Special Notes: 
;   None    
; Author:           
;   Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi   Created
;     11/8/2016    Sunghoon Choi   Initial Compilation
;     11/11/2016   Sunghoon Choi   Updated documentation                

SetLaser        PROC    NEAR
                PUBLIC  SetLaser                    
MOV laserStatus, AX     ;Sets laserStatus with the argument laserPowerArg.
RET                     ;End of SetLaser procedure.
SetLaser ENDP



; GetLaser
;
; Description:
;    The function is called with no arguments and returns the status of the RoboTrike laser 
;    in AX. A value of LASER_OFF indicates the laser is off and a value other than LASER_OFF
;    indicates the laser is on.           
; Operation:  
;    Returns laserStatus in AX.        
; Arguments:  
;    None  
; Return Value:   
;    laserStatus(AX)  -   Indicates whether the laser is on or off.
; Local Variables:   
;    None
; Shared Variables:  
;     laserStatus(AX) - [Read] -  Indicates whether the laser is on or off.
; Global Variables:  
;    None  
; Input:    
;    None           
; Output:            
;    None   
; Error Handling:   
;    None   
; Algorithms:        
;    None   
; Data Structures:   
;    None   
; Registers Changed: 
;    AX   
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi   Created
;     11/8/2016    Sunghoon Choi   Initial Compilation
;     11/11/2016   Sunghoon Choi   Updated documentation        

GetLaser        PROC    NEAR
                PUBLIC GetLaser
                
MOV AX, laserStatus            ;Return laserStatus to AX.
RET                            ;End of GetLaser procedure.
GetLaser ENDP        

; SetTurretAngle
;
; Description:
;    
; Operation:  
;    Returns laserStatus in AX.        
; Arguments:  
;    None  
; Return Value:   
;
; Local Variables:   
;    None
; Shared Variables:  
;     
; Global Variables:  
;    None  
; Input:    
;    None           
; Output:            
;    None   
; Error Handling:   
;    None   
; Algorithms:        
;    None   
; Data Structures:   
;    None   
; Registers Changed: 
;    AX   
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:


SetTurretAngle        PROC    NEAR
                      PUBLIC  SetTurretAngle
                
    RET                        

SetTurretAngle ENDP     

; SetRelTurretAngle
;
; Description:
;    
; Operation:  
;    Returns laserStatus in AX.        
; Arguments:  
;    None  
; Return Value:   
;
; Local Variables:   
;    None
; Shared Variables:  
;     
; Global Variables:  
;    None  
; Input:    
;    None           
; Output:            
;    None   
; Error Handling:   
;    None   
; Algorithms:        
;    None   
; Data Structures:   
;    None   
; Registers Changed: 
;    AX   
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:


SetRelTurretAngle        PROC    NEAR
                      PUBLIC  SetRelTurretAngle
                
    RET                        

SetRelTurretAngle ENDP     

; SetTurretElevation
;
; Description:
;    
; Operation:  
;    Returns laserStatus in AX.        
; Arguments:  
;    None  
; Return Value:   
;
; Local Variables:   
;    None
; Shared Variables:  
;     
; Global Variables:  
;    None  
; Input:    
;    None           
; Output:            
;    None   
; Error Handling:   
;    None   
; Algorithms:        
;    None   
; Data Structures:   
;    None   
; Registers Changed: 
;    AX   
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:


SetTurretElevation        PROC    NEAR
                          PUBLIC  SetTurretElevation
                
    RET                        

SetTurretElevation ENDP            

        
; MotorLaserEventHandler
;
; Description:
;    This is an eventhandler which activates motors and laser on parallel port B.
;    It checks if the pulseWidthCounter has reached the pulseWidth value of each motor
;    and turns the motors on or off accordingly. Once done with all motors, it checks the
;    laserStatus and turns the laser on or off accordingly. This function will be called 
;    by Timer1 at every PORTB_TIMER_MS miliseconds to enable PWM control.
;    The period of pulses is PERIOD_PWM_MOTORS ms and it has PRECISION_RATE_PWM bits of
;    precision.
; Operation:  
;    First it clears ParallelBOutput(The value to be output to parallel port B) to
;    DEFAULT_PORTB_OUTPUT
;   DEFAULT_PORTB_OUTPUT bit map:
;    -------0: Motor1 Direction Forward
;    ------0-: Motor1 Turned Off
;    -----0--: Motor2 Direction Forward
;    ----0---: Motor2 Turned Off
;    ---0----: Motor3 Direction Forward
;    --0-----; Motor3 Turned Off
;    0-------: Laser Turned Off
;    Next, it checks if the pulseWidth of current motor is negative. If it is negative,
;    save the absolute value of the negative pulseWidth in a register(AL) and set the 
;    direction bit of current motor to 1 to activate backward direction. If it is positive,
;    we don't need to change the direction bit since the default direction bit of
;    ParallelBOutput was MOTOR_DIRECTION_FORWARD.
;    Next, check if the pulseWidthCounter has reached the pulseWidth of current motor. 
;    If it has not reached the pulseWidth yet, set the power bit of current motor.
;    If it has already reached the pulseWidth of current motor, we don't need to change the
;    power bit since the default value of power bit of ParallelBOutput is MOTOR_TURN_OFF. 
;    Repeat this procedure for all motors.
;    Once the procedure has gone through all motors, the function updates pulseWidthCounter.
;    We use the equation "New pulseWidthCounter = (pulseWidthCounter+1) mod COUNTER_MAX" for
;    wrapping.
;    Finally, it sets the laser bit if laserStatus is not LASER_OFF. It resets the laser bit
;    otherwise.    
; Arguments:  
;   None  
; Return Value:   
;   None    
; Local Variables:   
;   ParallelBOutput - BL - The value to be output to parallel port B.
;    MotorIndex     - DI - The index of current motor       
; Shared Variables:  
;      driveSpeed(DS)    - [Read]  - The speed at which the RoboTrike is to move
;      driveAngle(DS)    - [Read]  - The angle at which the RoboTrike is to move in degrees
;      pulseWidths(DS)   - [Read]  - An array which contains the pulse widths for 
;                                    NUM_MOTORS motors.
;      pulseWidthCounter(DS)  - [Read/Write] - A counter used for PWM control.
;      laserStatus(DS)        - [Read] - Indicates whether the laser is on or off. 
; Global Variables:  
;    None   
; Input:    
;    None            
; Output:            
;    Parallel portB: Motors and laser.   
; Error Handling:   
;    None   
; Algorithms:        
;    None    
; Data Structures:   
;    None   
; Registers Changed: 
;    AX, BX, DI, Flags    
; Limitations:
;    None        
; Known bugs:        
;    None    
; Special Notes: 
;    None   
; Author:           
;    Sunghoon Choi
; Revision History:
;     11/6/2016    Sunghoon Choi   Created
;     11/8/2016    Sunghoon Choi   Initial Compilation
;     11/11/2016   Sunghoon Choi   Updated documentation            
MotorLaserEventHandler  PROC    NEAR
                        PUBLIC  MotorLaserEventHandler

InitMotorIndex:
    XOR DI, DI          ;Initializing the motor index.
                        ;The eventhandler handles motor1 first.

InitParallelValues:
    XOR BX, BX          ;Initialize ParallelBOutput(BL) to DEFAULT_PORTB_OUTPUT    
                        ;DEFAULT_PORTB_OUTPUT bit map
                        ;-------0: Motor1 Direction Forward
                        ;------0-: Motor1 Turned Off
                        ;-----0--: Motor2 Direction Forward
                        ;----0---: Motor2 Turned Off
                        ;---0----: Motor3 Direction Forward
                        ;--0-----; Motor3 Turned Off
                        ;0-------: Laser Turned Off
                        
CheckNegPulse:
    MOV AL, pulseWidths[DI]     ;First, we need to check if the pulseWidth is negative
                                ;to determine the direction.
    CMP AL, 0                   ;Is the pulse negative?
    JGE  CheckPCounter          ;No, it's positive.
                                ;Skip reversing the direction and go check if the counter
                                ;has reached pulseWidth.
    ;JGE SetBackDir             ;Yes, it's negative. Change the direction to backward.
SetBackDir:    
    NEG AL                      ;Get the absolute value of the negative pulseWidth
    OR  BL, CS:BackDirMask[DI]  ;Reverse the direction of current motor.
    JMP CheckPCounter           ;Now that we are done with handling direction,
                                ;proceed to check the counter.
CheckPCounter:
    CMP pulseWidthCounter, AL   ;Has the counter reached the pulse width of current motor?
    JGE HandleNextMotor         ;Yes. Halt the current motor and check the next motor.
;   JL  TurnCurrMotorOn         ;No. Continue activating the current motor.

TurnCurrMotorOn:
    OR BL, CS:TurnOnMask[DI]    ;Turn the current motor on.
HandleNextMotor:
    INC DI                      ;Increment index to handle the next motor
    CMP DI, NUM_MOTORS          ;Was that the last motor?
    JL CheckNegPulse            ;No. Go back to the beinning of loop and 
                                ;check the pulseWidth of next motor.
                               
;   JGE UpdatePCounter          ;Yes. We went through all NUM_MOTORS motors.
                                ;Now update the pulse width counter.
UpdatePCounter:
    INC pulseWidthCounter       ;Increment the pulseWidthCounter since we handled all 
                                ;motors once.
    XOR AX, AX                  ;Clear AH for DIV instruction.
    MOV AL, pulseWidthCounter   ;Dividend = pulseWidthCounter+1
    XOR DX, DX                  ;Clear DX for DIV instruction.
    MOV CX, COUNTER_MAX         ;Divisor = COUNTER_MAX
    DIV CX                      ;New pulseWidthCounter(DL) 
                                ; = (pulseWidthCounter + 1) mod COUNTER_MAX.
                                ; Modulus of COUNTER_MAX is done for wrapping.
    MOV pulseWidthCounter, DL   ;Update pulse counter with its new value.
    
CheckLaser:
    CMP laserStatus, LASER_OFF  ;Is the laser setting LASER_OFF??         
    JE  OutputParallel          ;Yes, turn laser off
;   JNE TurnLaserOn             ;No, turn laser on

TurnLaserOn:
    OR BL, LASER_POWER_MASK     ;Set the laser bit of ParallelBOutput.

OutputParallel:
    MOV DX, PARALLEL_B_ADDR    ;Get the address of parallel port B
    MOV AL, BL                 ;Get the final value of ParallelBOutput.
    OUT DX, AL                 ;Output the ParallelBOutput to parallel port B.
    
    
EndMotorLaserEventHandler:
    RET                        ;End of MotorLaserEventHandler  
MotorLaserEventHandler ENDP    
    

CODE ENDS                            
   
    
    


DATA SEGMENT PUBLIC 'DATA'
    pulseWidthCounter       DB  ?  ;A counter used for Pulse Width Modulation control
    pulseWidths             DB  NUM_MOTORS DUP (?) ;An array which contains the 
                                                   ;pulse widths for NUM_MOTORS motors.
    driveSpeed              DW  ?  ;The speed at which the RoboTrike is to move
    driveAngle              DW  ?  ;The angle at which the RoboTrike is to move in degrees
    laserStatus             DW  ?  ;Indicates whether the laser is on or off.
DATA ENDS
    

       
END
