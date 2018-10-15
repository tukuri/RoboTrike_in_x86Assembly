;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                                  ChipSel.inc                               ;
;                                 Sunghoon Choi                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Description: 
;  This file contains the definitions for the ChipSel.asm
;
; Revision History:
;    10/25/2016    Sunghoon Choi    Created
;    10/28/2016    Sunghoon Choi    Updated documentation(comments) for constants.

PCSctrl EQU 0FFA4H      ;The address of PCS control register
MPCSctrl EQU 0FFA8H     ;The address of MPCS register

PACSval EQU 00003H      ;PCS base at 0,    3 wait states
                        ;0000000000------  starts at address 0
                        ;----------000---  reserved                
                        ;-------------0--  wait for RDY inputs                
                        ;--------------11  3 wait states                
                        ;PCS in I/O space, use PCS5/6, 3 wait states
MPCSval EQU 00183H      ;0---------000---  reserved
                        ;-0000001--------  MCS is 8KB
                        ;--------1-------  output PCS5/PCS6
                        ;---------0------  PCS in I/O space
                        ;-------------0--  wait for RDY inputs
                        ;--------------11  3 wait states                
