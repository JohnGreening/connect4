device ZXSPECTRUMNEXT


;---------------------------------------------------------------------------------------------------------------
; Purpose :   backup board state before AI move, i.e. columns, board, lineScore, lineCount
; Input   :   none
; Output  :   none
; Updates :   copycolumns, copyboard, copylineScore, copylineCount 
;---------------------------------------------------------------------------------------------------------------
copyOriginalBoardState:
        ld hl, columns
        ld de, originalSnapshot
        ld bc, boardStateLen
        ldir

        ld a, (whoseGo)
        ld (copyWhoseGo), a
        
        ; initialise all column scores 0..6 to initialScore
        ld iy, colScore
        ld b, 7
        ld de, initialScore
.loop   ld (iy +0), e
        ld (iy +1), d
        inc iy
        inc iy
        djnz .loop    
        ret

restoreOriginalBoardState:
        ld hl, originalSnapshot
        ld de, columns
        ld bc, boardStateLen
        ldir

        ld a, (copyWhoseGo)
        ld (whoseGo), a
        ret

createSnapA:
        push hl
        push de
        push bc
        ld hl, columns
        ld de, snapA
        ld bc, boardStateLen
        ldir
        pop bc
        pop de
        pop hl
        ret

createSnapB:
        push hl
        push de
        push bc
        ld hl, columns
        ld de, snapB
        ld bc, boardStateLen
        ldir
        pop bc
        pop de
        pop hl
        ret

restoreFromSnapA:
        push hl
        push de
        push bc
        ld hl, snapA
        ld de, columns
        ld bc, boardStateLen
        ldir
        pop bc
        pop de
        pop hl
        ret

restoreFromSnapB:
        push hl
        push de
        push bc
        ld hl, snapB
        ld de, columns
        ld bc, boardStateLen
        ldir
        pop bc
        pop de
        pop hl
        ret


updateColScore:
;---------------------------------------------------------------------------------------------------------------
; Purpose   :   colScore(0-6) arrays hold the working score values to evaluate best AI move
;               IX +0 holds the current column being evaluated
;               we add DE to the colScore(current column)                
;---------------------------------------------------------------------------------------------------------------
        push bc
        push hl
        push de

        ld a, (ix +columnNo)                    ; current column 0..6
        add a, a                                ; x 2 since scores are 2 bytes
        ld b, 0                                 ; default b
        ld c, a                                 ; offset in c
        ld iy, colScore                         ; set base of column score array
        add iy, bc                              ; add to base colScore/yColScore


        ld   l, (iy +0)                         ; store back low
        ld   h, (iy +1)                         ; store back high

        ld de, (playerScore)                    ; get the value to update
        add hl, de
        ld   (iy +0), l                         ; store back low
        ld   (iy +1), h                         ; store back high

        pop de
        pop hl
        pop bc

.end
        ret


pickBestAIMove:
;--------------------------------------------------------------------------------------------------
; Input : IY -> array of 7 signed 16-bit words (little-endian)
;         NB IY is passed so that we can pass in an alternative colScore for yellow min/max scores
; Output: A  = index (0..6) of the maximum (signed) value
; Clobbers: A, B, C, D, E, H, L
; Tie-break: earlier index (lowest) wins
;--------------------------------------------------------------------------------------------------

        ld iy, colScore
        ld b, 0
        ld c, 0

        ld l, (iy+0)
        ld h, (iy+1)

        inc iy
        inc iy

.findLoop
        inc b
        ld a, b
        cp 7
        jr z, .done

        ld e, (iy+0)
        ld d, (iy+1)

        push hl
        or a
        sbc hl, de
        pop hl

        jr z, .rnd
        jp pe, .overflow
        jp p, .next
        jr .newMax

.overflow
        jp m, .next
        jr .newMax

.rnd
        ld a, r
        bit 0, a
        jr z, .next

.newMax
        ld l, e
        ld h, d
        ld c, b

.next
        inc iy
        inc iy
        jr .findLoop

.done
        ld a, c
        ret


DoAIMove:
;----------------------------------------------------------------------------------------------------------
; purpose
;  - loop through each of the columns 0..6
;  - evaluate board position
;  - pick best column
;
;  on return from here IX must point to column selected
;----------------------------------------------------------------------------------------------------------
        call copyOriginalBoardState             ; back-up columns, lineScore, lineCount
 
        ld ix, columns                          ; point to columns data, i.e. how many chips in each column
.loop:
        ld hl, 0                                ; default this column score to 0
        ld (playerScore), HL                    ; set it

        ld a, (ix +columnCnt)                   ; get chip count in column
        cp 255                                  ; check end of columns marker
        jr z, .end                              ; branch if finished
        cp 6                                    ; is it full
        jr z, .colFull                          ; branch if so

        ; just evaluate for centre preferred
        call evaluateColumnWeights

        ; check for an immediate yellow win
        call evaluateImmediateThreat

        ; place red and all possible yellow replies
        call evaluatePlayerOpponent1Ply

.nextCol:
        ld hl, 0
        ld (playerScore), hl
        
        ld a, (goes)
        cp 4
        jr nc, .skip

        ld de, (columnWeight)
        add hl, de

.skip
        ld de, (immediateThreat)
        add hl, de
        
        ld de, (placePlayer)
        add hl, de
        
        ld de, (bestPlaceOpponent)
        sbc hl, de

        ld (playerScore), hl

.updateScore
        call updateColScore
        inc ix                                  ; advance over board number
        inc ix                                  ; advance over chip count
        jr .loop                                ; loop for next column

.colFull
        ld hl, fullCol
        ld (playerScore), hl
        jr .updateScore

; all 7 columns have been evaluated
; now pick the best one and return
.end   
        call pickBestAIMove                     ; return A as the best index from the scores

; set ix to point to the column selected
        add a, a                                ; columns is 2 bytes per column, so double A
        ld d, 0                                 ; default high byte
        ld e, a                                 ; e = 2 * A
        ld ix, columns                          ; set base
        add ix, de                              ; add offset
        call restoreOriginalBoardState        
        ret


evaluateImmediateThreat:
; Purpose :   Initially place an opponents chip and evaluate - we want to block immediate opponents threat
        call createSnapA                        ; save current columns(), lineScore(), lineCount() into undoColumns ...
 
        inc (ix +1)                             ; increase chip count for the column
        ld a, (copyWhoseGo)                     ; get whose go it is currently
        xor 4                                   ; we want to simulate an opposite chip
        ld (whoseGo), a                         ; save it for setSlotValue routine
        call calculateBoardIndex                ; calculate A=BI from IX +0, IX+= +1
        call setSlotValue                       ; place on board and evaluate

        ld de, (boardScore)
        ld (immediateThreat), de

        call restoreFromSnapA                   ; restore board to before this yellow test
        ret


evaluatePlayerOpponent1Ply:
; Purpose :   place a Red chip and then all possible Yellow replies and evaluate

        call createSnapA

        call simulatePlayer
        call subsequentOpponent

        call restoreFromSnapA

        ret

evaluateColumnWeights:
; Purpose :   If nothing else to go on, favour the centre columns
        ld hl, colWeights                       ; point to weights array
        ld a, (ix +0)                           ; column to be evaluated

        add hl, a                               ; add in offset

        ld d, 0
        ld e, (hl)                              ; get weight in DE

        ld (columnWeight), de
        ret

subsequentOpponent:
; here we are simulating yellow in each of the columns 0..6 AFTER placing red chip
        push ix                                 ; save current column pointer

        ld ix, columns                          ; we want to simulate yellow response in each of 0..6 on top of red go
        ld hl, 0                                ; initialise score
        ld (opponentScore), hl                  ; set it
.loop:
        ld a, (ix +columnCnt)                   ; get column count 
        cp 255                                  ; is it the end marker
        jr z, .end                              ; branch if it is

        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        call simulateOpponent                   ; drop a yellow chip and evaluate
.nextCol:
        inc ix                                  ; advance over row number
        inc ix                                  ; advance over chip count
        jr .loop                                ; branch for next column
.end
        ld de, (opponentScore)                  ; get the best running yellow score
        ld (bestPlaceOpponent), de              ; save it

        pop ix                                  ; restore columns pointer
        ret


simulateOpponent:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a yellow chip and evaluate
;---------------------------------------------------------------------------------------------------------------
        call createSnapB                        ; save current columns(), lineScore(), lineCount() into undoColumns ...

        inc (ix +columnCnt)                     ; increase chip count for the column
        ld a, (copyWhoseGo)                     ; get whose go it is currently
        xor 4                                   ; we want to simulate an opposite chip
        ld (whoseGo), a                         ; save it for setSlotValue routine
        call calculateBoardIndex                ; calculate A=BI from IX +0, IX+= +1
        call setSlotValue                       ; place on board and evaluate

        ; pick the best running opponentScore
        or a
        ld hl, (opponentScore)
        ld de, (boardScore)
        sbc hl, de
        jr nc, .end
        ld (opponentScore), de
.end
        call restoreFromSnapB
        ret

simulatePlayer:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a red chip and evaluate
;---------------------------------------------------------------------------------------------------------------
        inc (ix +1)                             ; increase chip count for the column
        ld a, (copyWhoseGo)                     ; get whose go it is currently
        ld (whoseGo), a                         ; save it for setSlotValue routine
        call calculateBoardIndex                ; calculate A=BI from IX +0, IX+= +1
        call setSlotValue                       ; place on board and evaluate

        ld de, (boardScore)
        ld (placePlayer), de
        ret
