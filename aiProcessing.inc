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
        
        ; initialise all column scores 0..6 to -32767
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
        ld a, (ix +columnCnt)
        cp 6
        jr z, .end

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

        ld de, (redScore)                       ; get the value to update
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

        ; Seed "best" with entry 0 at IY
        ld   c, (iy +0)                         ; best.low
        ld   a, (iy +1)                         ; best.high
        xor  080h                               ; signed -> unsigned adjust
        ld   b, a                               ; bestAdj.high
        ld   e, 0                               ; bestIdx = 0
        ld   d, 1                               ; curIdx  = 1

        ; HL = IY + 2  (points to entry1.low)
        push iy
        pop  hl
        inc  hl
        inc  hl

.loop:
        ld   a, d
        cp   7
        jr   z, .done                           ; processed all 7 entries

        ld   a, (hl)                            ; cand.low
        inc  hl                                 ; -> cand.high
        ld   a, (hl)                            ; cand.high
        xor  080h
        cp   b
        jr   c, .skip                           ; cand.high < bestAdj.high
        jr   nz, .take                          ; cand.high > bestAdj.high

        ; equal highs -> compare lows
        dec  hl                                 ; -> cand.low
        ld   a, (hl)
        cp   c
        inc  hl                                 ; restore -> cand.high
        jr   c, .skip

.take:  ; update best = candidate
        ld   a, (hl)                            ; cand.high
        xor  080h
        ld   b, a                               ; bestAdj.high
        dec  hl                                 ; -> cand.low
        ld   a, (hl)
        ld   c, a                               ; best.low
        inc  hl                                 ; restore -> cand.high
        ld   e, d                               ; bestIdx = curIdx

.skip:
        inc  d                                  ; next index
        inc  hl                                 ; advance HL to next.low
        jr   .loop

.done:
        ld   a, e                               ; return index

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
        ld (redScore), HL                       ; set it

        ld a, (ix +columnCnt)                   ; get chip count in column
        cp 255                                  ; check end of columns marker
        jr z, .end                              ; branch if finished
        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        ; just evaluate for centre preferred
        call evaluateColumnWeights
        ld hl, (redScore)
        add hl, de
        ld (redScore), hl

        ; check for an immediate yellow win
        call evaluateImmediateThreat
        ld hl, (redScore)                       ; get current score value
        ld de, (boardScore)                     ; get the board score
        add hl, de                              ; add it in
        ld (redScore), hl                       ; save back

        ; place red and all possible yellow replies
        call evaluateRedYellow1Ply

.nextCol:
        call debug1
        call updateColScore
        inc ix                                  ; advance over board number
        inc ix                                  ; advance over chip count
        jr .loop                                ; loop for next column

; all 7 columns have been evaluated
; now pick the best one and return
.end
        call debugs
        ld iy, colScore                         ; set to pick from best Red move
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
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Initially place a yellow chip and evaluate - we want to block immediate player (yellow) threat
;             return in DE the column score
;---------------------------------------------------------------------------------------------------------------
        call createSnapA                        ; save current columns(), lineScore(), lineCount() into undoColumns ...
 
        inc (ix +1)                             ; increase chip count for the column
        ld a, yellowChip                        ; simulate a player (yellow) chip
        ld (whoseGo), a                         ; save it for setSlotValue routine
        call calculateBoardIndex                ; calculate A=BI from IX +0, IX+= +1
        call setSlotValue                       ; place on board and evaluate

        call restoreFromSnapA                   ; restore board to before this yellow test
        ret


evaluateRedYellow1Ply:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a Red chip and then all possible Yellow replies and evaluate
;---------------------------------------------------------------------------------------------------------------

        call createSnapA

        call simulateRed
        ld hl, (redScore)                       ; get current score value
        ld de, (boardScore)                     ; get the board score
        add hl, de                              ; add it in
        ld (redScore), hl                       ; save back

        ld hl, 0
        ld (yellowScore), hl
        call subsequentYellow
        or a
        ld hl, (redScore)
        ld de, (yellowScore)
        sbc hl, de
        ld (redScore), hl

        call restoreFromSnapA

        ret

evaluateColumnWeights:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   If nothing else to go on, favour the centre columns
;             return in DE the column weight
;---------------------------------------------------------------------------------------------------------------
        ld hl, colWeights                       ; point to weights array
        ld a, (ix +0)                           ; column to be evaluated

        add hl, a                               ; add in offset

        ld d, 0
        ld e, (hl)                              ; get weight in DE

        ret

subsequentYellow:
; here we are simulating yellow in each of the columns 0..6 AFTER placing red chip
; we score each board position into yellowColsScore
; then we pick the biggest value (it'll still be negative) but we assume yellow will pick the best for it, not us
; this is then used as a delta for colScore()
        push ix                                 ; save current column pointer

        ld ix, columns                          ; we want to simulate yellow response in each of 0..6 on top of red go
        ld hl, 0                                ; initialise score
        ld (yellowScore), hl                    ; set it
.loop:
        ld a, (ix +columnCnt)                   ; get column count 
        cp 255                                  ; is it the end marker
        jr z, .end                              ; branch if it is

        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        call simulateYellow                     ; drop a yellow chip and evaluate
.nextCol:
        inc ix                                  ; advance over row number
        inc ix                                  ; advance over chip count
        jr .loop                                ; branch for next column
.end
        pop ix
        ret


simulateYellow:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a yellow chip and evaluate
;---------------------------------------------------------------------------------------------------------------
        call createSnapB                        ; save current columns(), lineScore(), lineCount() into undoColumns ...

        inc (ix +1)                             ; increase chip count for the column
        ld a, yellowChip                        ; simulate a player (yellow) chip
        ld (whoseGo), a                         ; save it for setSlotValue routine
        call calculateBoardIndex                ; calculate A=BI from IX +0, IX+= +1
        call setSlotValue                       ; place on board and evaluate

        or a
        ld hl, (yellowScore)
        ld de, (boardScore)
        sbc hl, de
        jr nc, .end
        ld (yellowScore), de
.end
        call restoreFromSnapB

        ret

simulateRed:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a red chip and evaluate
;---------------------------------------------------------------------------------------------------------------
        inc (ix +1)                             ; increase chip count for the column
        ld a, redChip                           ; simulate a AI (red) chip
        ld (whoseGo), a                         ; save it for setSlotValue routine

        ; calculate row/column and hence BI (0..41)
        call calculateBoardIndex                ; calculate BI (needs B=column chip count, C=column)
        call setSlotValue                       ; place on board and evaluate

        ret
