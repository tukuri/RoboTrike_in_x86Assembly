;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Events.inc                                 ;
;                                                                            ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Description: 
;  This file contains the definitions for the all events of Robotrike.
;
; Revision History:
;    10/30/2016  Sunghoon Choi    Created
;    11/3/2016   Sunghoon Choi    Updated documentation(comments) for constants.
;    11/19/2016  Sunghoon Choi    Events for Serial have been updated.
;    12/02/2016  Sunghoon Choi    Added BLANK_EVENT

BLANK_EVENT           EQU 00H     ;No event. Exists as a blank slot
                                  ;for the jump table in Remote.asm
KEY_EVENT             EQU 01H     ;Keypad pressed event
SERIAL_RECEIVED_EVENT EQU 02H     ;Serial Data Received Event
SERIAL_ERROR_EVENT    EQU 03H     ;Serial Error Event