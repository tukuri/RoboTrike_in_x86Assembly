NAME  SEGTABLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                   SEGTABLE                                 ;
;                           Tables of 7-Segment Codes                        ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This file contains tables of 7-segment codes.  The segment ordering is a to
; g followed by the decimal point with segment a in the low bit and the
; decimal point in the high bit.  The tables included are:
;    ASCIISegTable - table of codes for 7-bit ASCII characters
;    DigitSegTable - table of codes for hexadecimal digits
;
; Revision History:
;    11/12/93  Glen George              initial revision
;    11/7/94   Glen George              added Revision History section
;    11/7/95   Glen George              added DigitSegTable
;    10/26/98  Glen George              updated comments
;    12/26/99  Glen George              changed segment name from PROGRAM to
;                                          CODE
;                                       added CGROUP group declaration
;                                       updated comments
;    12/25/00  Glen George              updated comments
;     2/06/07  Glen George              added pattern for lowercase 'g'



; local include files
;    none




;setup code group and start the code segment
CGROUP  GROUP   CODE

CODE    SEGMENT PUBLIC 'CODE'




; ASCIISegTable
;
; Description:      This is the segment pattern table for ASCII characters.
;                   It contains the active-high segment patterns for all
;                   possible 7-bit ASCII codes.  Codes which do not have a
;                   "reasonable" way of being displayed on a 7-segment display
;                   are left blank.  None of the codes set the decimal point.
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Glen George
; Last Modified:    Nov. 11, 1993

ASCIISegTable   LABEL   BYTE
                PUBLIC  ASCIISegTable


;       DB      .gfedcba                ;ASCII character

        DB      00000000B               ;NUL
        DB      00000000B               ;SOH
        DB      00000000B               ;STX
        DB      00000000B               ;ETX
        DB      00000000B               ;EOT
        DB      00000000B               ;ENQ
        DB      00000000B               ;ACK
        DB      00000000B               ;BEL
        DB      00000000B               ;backspace
        DB      00000000B               ;TAB
        DB      00000000B               ;new line
        DB      00000000B               ;vertical tab
        DB      00000000B               ;form feed
        DB      00000000B               ;carriage return
        DB      00000000B               ;SO
        DB      00000000B               ;SI
        DB      00000000B               ;DLE
        DB      00000000B               ;DC1
        DB      00000000B               ;DC2
        DB      00000000B               ;DC3
        DB      00000000B               ;DC4
        DB      00000000B               ;NAK
        DB      00000000B               ;SYN
        DB      00000000B               ;ETB
        DB      00000000B               ;CAN
        DB      00000000B               ;EM
        DB      00000000B               ;SUB
        DB      00000000B               ;escape
        DB      00000000B               ;FS
        DB      00000000B               ;GS
        DB      00000000B               ;AS
        DB      00000000B               ;US

;       DB      .gfedcba                ;ASCII character

        DB      00000000B               ;space
        DB      00000000B               ;!
        DB      00100010B               ;"
        DB      00000000B               ;#
        DB      00000000B               ;$
        DB      00000000B               ;percent symbol
        DB      00000000B               ;&
        DB      00000010B               ;'
        DB      00111001B               ;(
        DB      00001111B               ;)
        DB      00000000B               ;*
        DB      00000000B               ;+
        DB      00000000B               ;,
        DB      01000000B               ;-
        DB      00000000B               ;.
        DB      00000000B               ;/
        DB      00111111B               ;0
        DB      00000110B               ;1
        DB      01011011B               ;2
        DB      01001111B               ;3
        DB      01100110B               ;4
        DB      01101101B               ;5
        DB      01111101B               ;6
        DB      00000111B               ;7
        DB      01111111B               ;8
        DB      01100111B               ;9
        DB      00000000B               ;:
        DB      00000000B               ;;
        DB      00000000B               ;<
        DB      01001000B               ;=
        DB      00000000B               ;>
        DB      00000000B               ;?

;       DB      .gfedcba                ;ASCII character

        DB      01011111B               ;@
        DB      01110111B               ;A
        DB      01111111B               ;B
        DB      00111001B               ;C
        DB      00111111B               ;D
        DB      01111001B               ;E
        DB      01110001B               ;F
        DB      01111101B               ;G
        DB      01110110B               ;H
        DB      00000110B               ;I
        DB      00011110B               ;J
        DB      00000000B               ;K
        DB      00111000B               ;L
        DB      00000000B               ;M
        DB      00000000B               ;N
        DB      00111111B               ;O
        DB      01110011B               ;P
        DB      00000000B               ;Q
        DB      00000000B               ;R
        DB      01101101B               ;S
        DB      00000000B               ;T
        DB      00111110B               ;U
        DB      00000000B               ;V
        DB      00000000B               ;W
        DB      00000000B               ;X
        DB      01100110B               ;Y
        DB      00000000B               ;Z
        DB      00111001B               ;[
        DB      00000000B               ;\
        DB      00001111B               ;]
        DB      00000000B               ;^
        DB      00001000B               ;_

;       DB      .gfedcba                ;ASCII character

        DB      00100000B               ;`
        DB      00000000B               ;a
        DB      01111100B               ;b
        DB      01011000B               ;c
        DB      01011110B               ;d
        DB      00000000B               ;e
        DB      00000000B               ;f
        DB      01101111B               ;g
        DB      01110100B               ;h
        DB      00000100B               ;i
        DB      00000000B               ;j
        DB      00000000B               ;k
        DB      00110000B               ;l
        DB      00000000B               ;m
        DB      01010100B               ;n
        DB      01011100B               ;o
        DB      00000000B               ;p
        DB      00000000B               ;q
        DB      01010000B               ;r
        DB      00000000B               ;s
        DB      01111000B               ;t
        DB      00011100B               ;u
        DB      00000000B               ;v
        DB      00000000B               ;w
        DB      00000000B               ;x
        DB      01101110B               ;y
        DB      00000000B               ;z
        DB      00000000B               ;{
        DB      00000110B               ;|
        DB      00000000B               ;}
        DB      00000001B               ;~
        DB      00000000B               ;rubout




; DigitSegTable
;
; Description:      This is the segment pattern table for hexadecimal digits.
;                   It contains the active-high segment patterns for all hex
;                   digits (0123456789AbCdEF).  None of the codes set the
;                   decimal point.  
;
; Notes:            READ ONLY tables should always be in the code segment so
;                   that in a standalone system it will be located in the
;                   ROM with the code.
;
; Author:           Glen George
; Last Modified:    Nov. 7, 1995

DigitSegTable   LABEL   BYTE
                PUBLIC  DigitSegTable


;       DB      .gfedcba                ;Hex Digit

        DB      00111111B               ;0
        DB      00000110B               ;1
        DB      01011011B               ;2
        DB      01001111B               ;3
        DB      01100110B               ;4
        DB      01101101B               ;5
        DB      01111101B               ;6
        DB      00000111B               ;7
        DB      01111111B               ;8
        DB      01100111B               ;9
        DB      01110111B               ;A
        DB      01111100B               ;b
        DB      00111001B               ;C
        DB      01011110B               ;d
        DB      01111001B               ;E
        DB      01110001B               ;F




CODE    ENDS



        END