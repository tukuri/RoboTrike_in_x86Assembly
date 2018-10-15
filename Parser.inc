;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  Parser.inc                                ;
;                                  Homework 8                                ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: 
;  This file contains the definitions for the Parser.asm
;
; Revision History:
;    11/23/2016   Sunghoon Choi     Created        
;    11/26/2016   Sunghoon Choi     Updated Documentation


;Argument constants
ARG_INIT        EQU 0    ;Initial value of ArgNum
ARG_POSITIVE    EQU 0    ;Indicates that ArgNum is positive.
ARG_NEGATIVE    EQU 1    ;Indicates that ArgNum is negative.


;Parsing error flag constants
PARSER_SUCCESS    EQU    0    ;Indicates that the action function was processed successfully.
PARSER_ERROR      EQU    1    ;Indicates that there was an error processing the action function.


;States
ST_INITIAL                 EQU        0     ;Initial State
ST_SPD                     EQU        1     ;State for setitng speed
ST_SPD_SIGN                EQU        2     ;State for setting speed's sign
ST_SPD_DIGIT               EQU        3     ;State for setting speed's value
ST_REL_SPD                 EQU        4     ;State for setting relative speed
ST_REL_SPD_SIGN            EQU        5     ;State for setting relative speed's sign
ST_REL_SPD_DIGIT           EQU        6     ;State for setting relative speed's value
ST_DIR                     EQU        7     ;State for setting direction
ST_DIR_SIGN                EQU        8     ;State for setting direction's sign
ST_DIR_DIGIT               EQU        9     ;State for setting direction's value
ST_LSR_ON                  EQU        10    ;State for turning laser on
ST_LSR_OFF                 EQU        11    ;State for turning laser off
ST_TURRET_ANGLE            EQU        12    ;State for setting turret angle
ST_TURRET_ABS_DIGIT        EQU        13    ;State for setting turret angle's absolute value
ST_TURRET_ANGLE_SIGN       EQU        14    ;State for setting turret angle's sign
ST_TURRET_REL_DIGIT        EQU        15    ;State for setting turret angle's relative value
ST_TURRET_ELEVATION        EQU        16    ;State for setting turret's elevation
ST_TURRET_ELEV_SIGN        EQU        17    ;State for setting turret's elevation's sign
ST_TURRET_ELEV_DIGIT       EQU        18    ;State for setting turret's elevation's value


;Token types
TOKEN_SPEED                EQU        0     ;Command S
TOKEN_RELATIVE_SPEED       EQU        1     ;Command V
TOKEN_DIRECTION            EQU        2     ;Command D
TOKEN_LASER_ON             EQU        3     ;Command F
TOKEN_LASER_OFF            EQU        4     ;Command O
TOKEN_TURRET_ANGLE         EQU        5     ;Command T
TOKEN_TURRET_ELEVATION     EQU        6     ;Command E
TOKEN_SIGN_POSITIVE        EQU        7     ;Sign +
TOKEN_SIGN_NEGATIVE        EQU        8     ;Sign -
TOKEN_DIGIT                EQU        9     ;Digits 0~9
TOKEN_IGNORE               EQU        10    ;space bar
TOKEN_RETURN               EQU        11    ;Carriage Return
TOKEN_OTHER                EQU        12    ;Other charactrs

NUM_TOKEN_TYPES            EQU        13    ;Total number of token types

;Token mask
TOKEN_MASK                 EQU        01111111B ;Mask off the highest bit of token
                                    
