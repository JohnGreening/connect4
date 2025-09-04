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
        call initialiseBoard                    ; set columns(), lineScore(), lineCount() to 0
        displayText txtTitle
        displayText txtInstruction1
        call newGo

; ----------------------------
; main game start
; ----------------------------
MainLoop:
        ld a, (whoseGo)
        cp redGo
        jp z, AIMove
        jr playerMove

playerMove:
        readKey $dffe, %00000001                ; check if "P" is pressed
        call z, keyRight                        ; move chip if allowed

        readKey $dffe, %00000010                ; check if "O" is pressed
        call z, keyLeft                         ; move chip if allowed

        readKey $7ffe, %00000001                ; check if [space] pressed
        jp z, keyEnter                          ; branch if pressed

        readKey $fefe, %00000100                ; check if "X" pressed
        JR NZ, playerMove                       ; branch if not

        CALL endProg                            ; tidy up sprites and turn off sound
        IM 1                                    ; restore im 1
        NEXTREG $7, 0                           ; set speed to 3.5mhz
        RET


keyRight:
;---------------------------------------------------------------------------------------------------------------
; Purpose   :   move the chip one column to the right, if allowed
;---------------------------------------------------------------------------------------------------------------
        call keyPause                           ; delay processing

        LD A, (columnSelected)                  ; get current column
        INC A                                   ; +1
        CP maxColumn + 1                        ; is it too many?
        ret z                                   ; return if so, no action

        LD (columnSelected), A                  ; update the column store
        call movechipLR                         ; move the chip
        playSoundEffect soundKey                ; make a beep

        ret

keyLeft:
        call keyPause                           ; delay processing

        LD A, (columnSelected)                  ; get current column
        DEC A                                   ; -1
        CP minColumn - 1                        ; is it too few
        ret z                                   ; return is so, no action

        LD (columnSelected), A                  ; update the column store
        call movechipLR                         ; move the chip
        playSoundEffect soundKey                ; make a beep

        ret

keyEnter:
        call keyPause
        ld a, (columnSelected)                  ; get column selected
        dec a                                   ; make 0 based index
        ld hl, columns                          ; point HL to columns data
        add hl, a                               ; point to actual column selected
        ld a, (hl)                              ; get how many chips in this column
        cp 6                                    ; are we full?
        jp z, playerMove                        ; return if move invalid

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
.loop:
        readKey $7ffe, %00001000                ; see if "N" is pressed
        jr nz, .loop                            ; loop if not
        jp newGame                              ; jump if pressed

AIMove:
        call copyOriginalBoardState             ; back-up columns, lineScore, lineCount
        call DoAIMove                             ; call the AI move routine
        call restoreOriginalBoardState          ; restore original columns, lineScore, lineCount

        jp keyEnter                             ; make the move


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

SAVENEX OPEN "connect4.nex", MAINPROG, $FE00
;SAVENEX CORE 2, 0, 0
;SAVENEX CFG 7, 0, 0, 0
SAVENEX AUTO
SAVENEX CLOSE

