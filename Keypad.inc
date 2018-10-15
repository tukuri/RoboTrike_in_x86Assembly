;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Keypad.inc                                 ;
;                                 Homework5                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: 
;  This file contains the definitions for the Keypad functions for RoboTrike.
;
; Revision History:
;    10/30/2016   Sunghoon Choi    Created
;    11/3/2016    Sunghoon Choi    Updated documentation(comments) for constants.

KEY_COUNTER_MAX     EQU     50        ;Initial value of the counter which is used for
                                      ;debouncing each key.

FIRST_KEY           EQU     0         ;The index of the first key on keypad                        
COL_INTERVAL        EQU     1         ;Number of bit shift needed to move to next column key
                                    
REPEAT_COUNTER      EQU     250       ;Initial value of the counter which is used for
                                      ;handling the consistently pressed key.
NUM_KEYS            EQU     16        ;Number of keys on the board.

KEY_PRESSED         EQU     0         ;the value read from a pressed key
COL_OVER_LIMIT      EQU     00000000B ;indicates that the column index has exceeded the
                                      ;number of keys in a row

KEY_FIRST_ROW_ADDR  EQU     80H       ;The address of the first key row
KEY_LAST_ROW_ADDR   EQU     83H       ;The address of the last key row
KEYPAD_ROW_MASK     EQU     00001111B ;The mask used to extract only the key values
                                      ;and remove all other bits
KEYPAD_COLUMN_MASK  EQU     00000001B ;The mask used to indicate a specific key
                                      ;inside a row.