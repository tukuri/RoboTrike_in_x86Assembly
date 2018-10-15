;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                 Queue.inc                                  ;
;                                 Homework3                                  ;
;                                Sunghoon Choi                               ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Description:      
;    This file contains definitions of constants and structures for (queue.asm)
;
;
; Revision History:    
;   2016/10/21    Sunghoon Choi    Initial Revision
;   2016/10/22    Sunghoon Choi    Changed MAX_QUEUE_SIZE value to 256 from 1024


BYTE_SIZE         EQU    1            ;The size of a byte in bytes.
WORD_SIZE         EQU    2            ;The size of a word in bytes.
MAX_QUEUE_SIZE    EQU    256          ;The maximum size, or length for the queue.
QUEUE_INIT_CON    EQU 0               ;Initial condition for queue elements

QueueModule STRUC
    Queue        DB    MAX_QUEUE_SIZE DUP (?)  ;Even number slots required since words can be inserted.
    HeadIndex    DB  ?
    len          DW    ?                       ;Final Index is 255. The Index starts from zero
    ElemSize     DB  ?
    CurrentElemNum DW ?
QueueModule ENDS