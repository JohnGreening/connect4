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

MAINPROG
; this stuff is only called ONCE
        NEXTREG $7, 0                           ; set processor speed, 3.5mhz
        CALL setupIM2                           ; set-up sound effects routine
        CALL spriteSetup                        ; initialise graphics
        call convertChars                       ; convert some tiles to chars
        call initCellWinLines                   ; create cellWinLines table

        ld a, redChip                           ; yellowChip = 0, redChip = 4
        ld (startGo), a                         ; default red to go first, but actually we reverse this below

; initialisation for each game
newGame:
        call tileMapOnTop                       ; ensure tileMap priority in display
        CALL displayBoard                       ; show the Connect 4 board
        call initialiseBoard                    ; set columns(), lineScore(), lineCount() to 0
        displayText txtTitle                    ; dispplay the "Connect 4" title
        displayText txtInstruction1             ; display the navigation keys to use

        ld a, (startGo)                         ; get chip used to start last time (or initialisation)
        xor 4                                   ; swap yellow/red around
        ld (startGo), a                         ; save it back
        ld (whoseGo), a                         ; save next player move

; main game loop
MainLoop:
        ld ix, columns                          ; point IX to columns data
        ld hl, 0                                ; set counter count to 0
        ld b, 7                                 ; loop for 7 columns
.drawLoop
        ld a, (ix +columnCnt)                   ; get the counter count for this column
        add hl, a                               ; add to the running total
        inc ix                                  ; point to next column
        djnz .drawLoop                          ; loop if there are more columns
        ld a, l                                 ; get the counter running total
        cp 42                                   ; is the board full
        jp z, gameDraw                          ; jump if so, no move is possible

        ld ix, columns                          ; point IX to columns data
        ld a, 0                                 ; reset the sprite index
        ld (spriteIndex), a
        
;       wait until space bar is NOT being pressed
;       otherwise might take as a go when user not ready
        call chkSpaceNotPressed

        ld a, (whoseGo)                         ; get whose go it is
        cp yellowChip                           ; is it yellow - player
        jp z, playerYellow                      ; branch if so
        jp playerRed                            ; otherwise branch to AI

playerRed:
        call DoAIMove                           ; red is the AI player, do that processing
        jp dropChip                             ; drop chip selected by AI

playerYellow:
        call positionChip                       ; position chip initially

checkKeys:
        readKey $dffe, %00000001                ; check if "P" is pressed
        call z, keyRight                        ; move chip if allowed

        readKey $dffe, %00000010                ; check if "O" is pressed
        call z, keyLeft                         ; move chip if allowed

        readKey $7ffe, %00000001                ; check if [space] pressed
        jp z, keySpace                          ; branch if pressed

        jr checkKeys

keyRight:
        call keyPause                           ; delay processing

        LD A, (ix +columnNo)                    ; get current column
        CP 6                                    ; are we at far RHS column already?
        ret z                                   ; return if so, no action

        inc ix                                  ; advance to next column, over the column number
        inc ix                                  ; and over the chip count for that column
        call positionChip                       ; move the chip
        playSoundEffect soundKey                ; make a beep

        ret

keyLeft:
        call keyPause                           ; delay processing

        LD A, (ix +columnNo)                    ; get current column
        cp 0                                    ; are we at far LHS column already?
        ret z                                   ; return is so, no action

        dec ix                                  ; advance to prev column, over the column number
        dec ix                                  ; and over the chip count for that column
        call positionChip                       ; move the chip
        playSoundEffect soundKey                ; make a beep

        ret

keySpace:
        call keyPause
        ld a, (ix +columnCnt)                   ; get chip count in column selected
        cp 6                                    ; are we full?
        jp z, checkKeys                         ; return if move invalid

dropChip:
        ld a, (ix +columnCnt)                   ; get chip count in column selected
        inc a                                   ; add the new chip
        ld (ix +columnCnt), a                   ; and store in columns

        ld b, a                                 ; get ready to calculate the row selected
        ld a, 7                                 ; the row is 7 - chips in column                             
        sub b                                   ; get the row
        ld (rowSelected), a                     ; store the row impacted (1-6, 0 is when choosing column)
        ld a, (ix +columnNo)                    ; get the column selected
        ld (columnSelected), a                  ; and store that

        playSoundEffect soundDrop               ; play a drop sound
        call movechipDown                       ; move sprite down until it hits bottom of column

        call calculateBoardIndex                ; return BI (boardindex 0-41) for "selected" row/column
        call setSlotValue                       ; calculate the "board value" and hence did we win !
        cp 255                                  ; 255 means no winner
        jr nz, gameWin                          ; branch if not 255, winner detected !
        jp nextGo                               ; otherwise jump for next go

gameDraw
        displayText txtDraw1                    ; display that it's a draw
        displayText txtWinner2                  ; display "press N for a new game"
        jr readyNextGame                        ; play end game sound and wait for N key

gameWin
        call winnerDisplay                      ; display the winning row (one of them anyway)
        displayText txtWinner1                  ; display "winner found"
        displayText txtWinner2                  ; display "press N for a new game"

readyNextGame
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
.loop:
        readKey $7ffe, %00001000                ; see if "N" is pressed
        jr nz, .loop                            ; loop if not
        jp newGame                              ; jump if pressed

nextGo
        ld a, (whoseGo)                         ; whose go was it 0=yellow, 4=red
        xor 4                                   ; swap to other player
        ld (whoseGo), a                         ; set it
        jp MainLoop                             ; branch for next go

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

SAVENEX OPEN "connect4.nex", MAINPROG, $FE00
;SAVENEX CORE 2, 0, 0
;SAVENEX CFG 7, 0, 0, 0
SAVENEX AUTO
SAVENEX CLOSE

