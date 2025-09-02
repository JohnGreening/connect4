device ZXSPECTRUMNEXT

INCLUDE "connect4.inc"
INCLUDE "globals.inc"
INCLUDE "utilities.inc"
INCLUDE "levelSetup.inc"
INCLUDE "spriteRoutines.inc"
INCLUDE "soundEffects.inc"
include "textDisplay.inc"
include "macros.inc"
include "aiProcessing.inc"

; ----------------------------
; Game Initialization
; ----------------------------
MAINPROG
; this stuff is only called ONCE
        NEXTREG $7, 0                           ; set processor speed
        CALL setupIM2                           ; set-up sound effects routine
        CALL spriteSetup                        ; initialise graphics
        call convertChars                       ; convert some tiles to chars
        call initCellWinLines                   ; create cellWinLines table

newGame:
        call killSprites                        ; remove any sprites that may be around
        call tileMapOnTop                       ; ensure tileMap priority in display
        CALL displayBoard                       ; show the Connect 4 board
        call initialiseBoard                    ; initialise internal values
        displayText txtTitle
        displayText txtInstruction1
        call newGo

; ----------------------------
; main game start
; ----------------------------
MainLoop:
        ld a, (chipPattern)
        cp redChip
        jp z, AIMove

        LD BC, $dffe                            ; keyboard port for Y, U, I, O, P
        IN A, (C)                               ; read port
        AND %00000001                           ; bit 0 is "P"
        JR Z, keyRight                          ; branch if pressed
        IN A, (C)                               ; read port again
        AND %00000010                           ; bit 1 is "O"
        JR Z, keyLeft                           ; branch if pressed

        LD BC, $7ffe                            ; keyboard port for B, N, M, sym, space
        IN A, (C)                               ; read port
        AND %00000001                           ; bit 0 is "spacebar"
        JR Z, keyEnter                          ; branch if pressed

; ----------------------------
; Exit Handling (X key)
; ----------------------------
CheckExit:
        LD BC, $fefe                            ; keyboard port for V, C, X, Z, shift
        IN A, (C)                               ; read port
        AND %00000100                           ; bit 2 is "X"
        JR NZ, MainLoop                         ; X not pressed → continue game
        jp EndProgram                           ; X pressed → end program

keyRight:
        call keyPause                           ; delay processing
        playSoundEffect soundKey

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
        playSoundEffect soundKey

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

        playSoundEffect soundDrop
        call movechipDown

; determine board aray element 0..41
        ld a, (rowSelected)                     ; get row
        dec a                                   ; make 0 based
        ld d, a                                 ; put in D ready for multiplication
        ld e, 7                                 ; multiply by 7
        mul d, e                                ; do it
        ld a, (columnSelected)                  ; get the column 
        dec a                                   ; make 0 based
        add a, e                                ; a is now  0-41 !

        call setSlotValue
        ; expected return is A= winLine for a win, otherwise 255
        ;                    C= line (0..68) where win detected
        cp 255
        jr nz, winDetected
        call newGo
        jp MainLoop

winDetected:
        call winnerDisplay
        playSoundEffect soundTada1
        halt 
        halt 
        halt 
        halt

        playSoundEffect soundTada2
        halt 
        halt 
        halt 
        halt
        playSoundEffect soundTada3
        displayText txtWinner1
        displayText txtWinner2
wd1:
        ld bc, $7ffe
        in a, (c)
        and %00001000
        jr nz, wd1
        jp newGame

AIMove:
        call copyOriginalBoardState
        call aiMove
        ;call debugs
        ld iy, colScore
        call pickBestAIMove
        inc a
        ld (columnSelected), a
        call restoreOriginalBoardState
        jp keyEnter

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
        PUSH IX
        PUSH IY

INCLUDE "im2Routine.inc"

        POP IY
        POP IX
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

