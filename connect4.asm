device ZXSPECTRUMNEXT

INCLUDE "connect4.inc"
INCLUDE "globals.inc"
INCLUDE "utilities.inc"
INCLUDE "levelSetup.inc"
INCLUDE "spriteRoutines.inc"

; ----------------------------
; Game Initialization
; ----------------------------
MAINPROG
; this stuff is only called ONCE
        NEXTREG $7, 0                       ; set speed to 28mhz
        CALL spriteSetup                    ; initialise graphics
        CALL newGame
        CALL displayCounter

; ----------------------------
; main game start
; ----------------------------
MainLoop:
; ----------------------------
; Exit Handling (X key)
; ----------------------------
CheckExit:
        LD BC, $fefe                ; key group: shift, Z, X, C, V
        IN A, (C)
        AND %00000100               ; bit 2 = X
        JR NZ, MainLoop             ; X not pressed → continue game
        JR EndProgram               ; X pressed → end program

; ----------------------------
; Program End
; ----------------------------
EndProgram:
        CALL endProg
        NEXTREG $7, 0                   ; set speed to 3.5mhz
        RET


length EQU $ - $6000
SAVEBIN "connect4.bin", $6000, length

SAVENEX OPEN "connect4.nex", MAINPROG, $FF40
;SAVENEX CORE 2, 0, 0
;SAVENEX CFG 7, 0, 0, 0
SAVENEX AUTO
SAVENEX CLOSE

