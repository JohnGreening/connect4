

colWeights  defb 3, 4, 5, 7, 5, 4, 3            ; center preferred column weights
colScore    defw $0000, $0000, $0000, $0000, $0000, $0000, $0000            ; each column has a 2 byte score
storeBI     db 0                                ; last BI cell, used for undo processing

copycolumns     defs 7, 0                       ; copy of the standard 4 for use in AI move
copyboard       defs 42, 0
copylineScore   defs 69, 0                      
copylineCount   defs 69, 0
copyWhoseGo     db 0
moveNowInd      db 0

initialScore  equ -32767

;---------------------------------------------------------------------------------------------------------------
; Purpose :   backup board state before AI move, i.e. columns, board, lineScore, lineCount
; Input   :   none
; Output  :   none
; Updates :   copycolumns, copyboard, copylineScore, copylineCount 
;---------------------------------------------------------------------------------------------------------------
copyBoardState:
        ld hl, columns
        ld de, copycolumns
        ld bc, 7 +42 +69 +69
        ldir

        ld a, (whoseGo)
        ld (copyWhoseGo), a

        ld a, 0
        ld (moveNowInd), a
        
        ; initialise all column scores 0..6
        ld iy, colScore
        ld b, 7
        ld de, initialScore
.loop   ld (iy +0), e
        ld (iy +1), d
        inc iy
        inc iy
        djnz .loop    
        ret

rollbackBoardState:
        ld hl, copycolumns
        ld de, columns
        ld bc, 7 +42 +69 +69
        ldir

        ld a, (copyWhoseGo)
        ld (whoseGo), a
        ret

rollbackCountsOnly:
        ld hl, copyboard
        ld de, board
        ld bc, 42 +69 +69
        ldir
        ret

updateColScore:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   helper function to do (IY) = (IY) + DE
; Input   :   IY points to colScore for column being evaluated
;             DE score to add/subtract
; Output  :   none
; Updates :   colScore
;---------------------------------------------------------------------------------------------------------------
; IY -> word (low at (IY), high at (IY+1))
; DE = signed 16-bit delta
; Preserves: BC, DE
; Clobbers:  AF, HL moves to (HL+1) at exit
        push iy
        pop hl
        ld   a,(hl)        ; low
        add  a,e           ; low += E
        ld   (hl),a
        inc  hl
        ld   a,(hl)        ; high
        adc  a,d           ; high += D + carry
        ld   (hl),a
        ret


pickBestAIMove:
; Input:  colScore = 7 signed 16-bit words (little-endian)
; Output: A = index (0..6) of the maximum signed value in colScore
        ld   hl, colScore

        ; seed best = entry0 (signed -> adjusted: high^$80)
        ld   c,(hl)               ; best.low
        inc  hl
        ld   a,(hl)               ; best.high
        xor  080h
        ld   b,a                  ; bestAdj.high
        ld   e,0                  ; bestIdx = 0
        ld   d,1                  ; curIdx  = 1
        inc  hl                   ; HL -> entry1.low

.loop:
        ; HL -> candidate.low
        ; load candidate.high (adjusted) into A and compare to bestAdj.high (B)
        ld   a,(hl)               ; (throwaway read of low for timing locality)
        inc  hl
        ld   a,(hl)               ; cand.high
        xor  080h                 ; adjust sign bit
        cp   b
        jr   c, .skip             ; cand < best -> skip
        jr   z, .tie

        ; cand.high > best.high  -> update best to candidate
.update:
        ld   b,a                  ; new bestAdj.high
        dec  hl
        ld   a,(hl)               ; cand.low
        inc  hl
        ld   c,a                  ; new best.low
        ld   a,d
        ld   e,a                  ; bestIdx = curIdx
        jr   .next

.tie:
        ; highs equal -> compare lows: cand.low vs best.low
        dec  hl
        ld   a,(hl)               ; cand.low
        cp   c
        inc  hl
        jr   c, .skip             ; cand.low < best.low -> keep
        jr   z, .skip             ; equal -> keep leftmost
        ; cand.low > best.low -> update
        ld   c,a                  ; new best.low
        ld   a,(hl)               ; re-read cand.high
        xor  080h
        ld   b,a                  ; new bestAdj.high
        ld   a,d
        ld   e,a                  ; bestIdx = curIdx

.next:
        inc  hl                   ; advance to next entry.low
        inc  d                    ; curIdx++
        ld   a,d
        cp   7
        jr   nz, .loop

        ld   a,e                  ; return best index 0..6
        ret

.skip:
        inc  hl                   ; advance to next entry.low
        inc  d
        ld   a,d
        cp   7
        jr   nz, .loop
        ld   a,e
        ret


aiBasicMove:
        ld c, 0                                 ; column being tested
        ld b, 7                                 ; loop for 7 columns
        ld iy, colScore                         ; point to column score table
.loop:
        ld a, c                                 ; get column
        ld hl, columns                          ; point to columns array
        add hl, a                               ; add in offset
        ld a, (hl)                              ; get count of chips in this column

        cp 6                                    ; is it full
        jr z, .noMove                           ; branch if so

        call evaluateWinBlock
        call evaluateColumnWeights
        inc c                                   ; advance to next column
        inc iy                                  ; advance score pointer
        inc iy                                  ; .. its 2 bytes
        djnz .loop                              ; loop for all 7 columns
        ret
.noMove:
        inc c                                   ; advance to next column
        inc iy                                  ; advance score pointer
        inc iy                                  ; .. its 2 bytes
        djnz .loop                              ; loop for all 7 columns
        ret


evaluateColumnWeights:
        ld hl, colWeights                       ; point to weights array
        ld a, c                                 ; restore column
        add hl, a                               ; add in offset

        ld d, 0
        ld e, (hl)                              ; get weight
        call updateColScore                     ; (IY) = (IY) +/- DE
        ret

evaluateWinBlock:
        push bc                                 ; save column being evalyated and loop count

        ; routine is called with HL pointing to columns(c) and a with count of chips in that column
        inc a                                   ; add a chip
        ld (hl), a                              ; place in column
        ld b, a                                 ; column chip count
        ld a, yellowGo                          ; simulate a yellow chip, i.e. block win check
        ld (whoseGo), a

        ; calculate row/column and hence BI (0..41)
        ld a, 6
        sub b
        ld d, a
        ld e, 7
        mul d, e
        ld a, e
        add c                                   ; a = bi (0..41)
        ld (storeBI), a                         ; store it because we will simulate a red chip i.e. win check next

        call setSlotValue
        cp 4
        jr nz, .checkRedWin

.blockImmediateWin
        ld de, 10000
        call updateColScore
        ld a, 1
        ld (moveNowInd), a

.checkRedWin
        call rollbackCountsOnly
        ld a, redGo                             ; simulate a red chip, i.e. red win check
        ld (whoseGo), a

        ld a, (storeBI)
        call setSlotValue
        cp 4
        jr nz, .end

        ld de, 30000
        call updateColScore
        ld a, 1
        ld (moveNowInd), a
.end
        ; if moveNowInd <> 0 then 
        ;     check each column 0-6 again to see if this move gifts a win to yellow
        ;     i.e. - simulate yellow chip in columns 0..6
        ;          - does yellow win
        ;           - if so, add -20000 to score for THIS column not the one simulated
        ;           - will need a specific undo to -1 the columnScore and columnCount
        ; quite involved !
        
        call rollbackBoardState
        pop bc
        ret
