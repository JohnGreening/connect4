device ZXSPECTRUMNEXT

INCLUDE "connect4.inc"
INCLUDE "globals.inc"
INCLUDE "utilities.inc"
INCLUDE "levelSetup.inc"
INCLUDE "spriteRoutines.inc"
INCLUDE "soundEffects.inc"
include "text.inc"

; ----------------------------
; Game Initialization
; ----------------------------
MAINPROG
; this stuff is only called ONCE
        NEXTREG $7, 0                       ; set speed to 28mhz
        CALL setupIM2
        CALL spriteSetup                    ; initialise graphics
newGame:
        call killSprites
        call tileMapOnTop
        CALL displayBoard
        call initialiseBoard
        call initWinPairLookup
        ;call ULAon
        ;call test1
        call newGo
;        call displaychip
; ----------------------------
; main game start
; ----------------------------
MainLoop:
        LD BC, $dffe
        IN A, (C)
        AND %00000001
        JR Z, keyRight
        IN A, (C)
        AND %00000010
        JR Z, keyLeft

        LD BC, $7ffe
        IN A, (C)
        AND %00000001
        JR Z, keyEnter

; ----------------------------
; Exit Handling (X key)
; ----------------------------
CheckExit:
        LD BC, $fefe                ; key group: shift, Z, X, C, V
        IN A, (C)
        AND %00000100               ; bit 2 = X
        JR NZ, MainLoop             ; X not pressed → continue game
        jp EndProgram               ; X pressed → end program

keyRight:
        call keyPause
        LD A, soundKey
        CALL playsound
        LD A, (columnSelected)
        INC A
        CP maxColumn + 1
        JR NZ, scc1
        LD A, minColumn
scc1:
        LD (columnSelected), A
        call movechipLR
        jp MainLoop

keyLeft:
        call keyPause
        LD A, soundKey
        call playsound
        LD A, (columnSelected)
        DEC A
        CP minColumn - 1
        JR NZ, scc2
        LD A, maxColumn
scc2:
        LD (columnSelected), A
        call movechipLR
        jp MainLoop

keyEnter:
        call keyPause
        ld a, (columnSelected)                  ; get column selected
        dec a                                   ; make 0 based index
        ld hl, columns                          ; point HL to columns data
        add hl, a                               ; point to actual column selected
        ld a, (hl)                              ; get how many chips in this column
        cp 6                                    ; are we full?
        jr z, MainLoop                          ; branch back if so
        inc a                                   ; add the new chip
        ld (hl), a                              ; and store in columns

        ld b, a                                 ; temp store of A
        ld a, 7                                 ; the row is 7 - chips in column                             
        sub b                                   ; get the row
        ld (rowSelected), a                     ; store the row selected
        call getYPixel
        ld d, 0
        ld e, a

        ld a, soundDrop
        call playsound
        call movechipDown

        call setSlotValue
        jp c, winDetected
        call newGo
        jp MainLoop

winDetected:
        call winnerDisplay
        ld a, soundTada1
        call playsound
        halt 
        halt 
        halt 
        halt

        ld a, soundTada2
        call playsound
        halt 
        halt 
        halt 
        halt
        ld a, soundTada3
        call playsound

wd1:
        ld bc, $7ffe
        in a, (c)
        and %00001000
        jr nz, wd1
        jp newGame

; ----------------------------
; Program End
; ----------------------------
EndProgram:
        CALL endProg
        IM 1
        NEXTREG $7, 0                   ; set speed to 3.5mhz
        RET

setupIM2:
; sub routine to set-up IM2 to point to BB
        DI 
        LD HL, IM2Tab
        LD DE, IM2Tab +1
        LD A, H
        LD I , A
        LD A, $f0
        LD (HL), A
        LD BC, 256
        LDIR 

        IM 2
        EI
        RET

; Align 256
DEFS -$&$FF
IM2Tab:
        DEFS 257, 0

ORG $f0f0
im2Routine
        PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL

INCLUDE "im2Routine.inc"

        POP HL
        POP DE
        POP BC
        POP AF

        EI
        RETI 


length EQU $ - $6000
SAVEBIN "connect4.bin", $6000, length

SAVENEX OPEN "connect4.nex", MAINPROG, $FF40
;SAVENEX CORE 2, 0, 0
;SAVENEX CFG 7, 0, 0, 0
SAVENEX AUTO
SAVENEX CLOSE

