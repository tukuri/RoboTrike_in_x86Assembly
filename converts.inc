;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                Converts.inc                                ;
;                                 Homework2                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Constants                 
                
INITIAL_PWR10 EQU 10000         ;initial value for the dividing factor of
                                ;dec2string
ASCII_CODE_ZERO EQU '0'         ;ASCII offset required to convert 0 to '0'
INITIAL_PWR16 EQU 4096          ;initial value for the dividing factor of
                                ;hex2string
ASCII_CODE_AMINUSTEN EQU 55     ;ASCII offset required to convert 10 to 'A'.
                                ;This is equal to 'A'-10