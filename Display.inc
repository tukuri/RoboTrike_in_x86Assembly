;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                               Display.inc                                  ;
;                                Homework 4                                  ;
;                               Sunghoon Choi                                ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description: 
;  This file contains the definitions for the Display functions for RoboTrike.
;
; Revision History:
;    10/25/2016    Sunghoon Choi    Created
;     10/28/2016   Sunghoon Choi    Updated documentation(comments) for constants.

NUM_DIGITS      EQU     8         ;The number of LED digits on the board.
BLANK_PATTERN   EQU     00000000B ;The pattern for empty digit.
LED_ADDR        EQU     00H       ;The starting address of the 7-Segment LEDs.
                                  ;(The leftmost LED digit)
DISPLAY_START_INDEX EQU 0         ;The starting index of display.