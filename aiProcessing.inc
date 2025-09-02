

redLUT      defw 0, 0,  0,    0,     0
            defw 0, 6,  0,    0,     0
            defw 0, 0, 70,    0,     0
            defw 0, 0,  0, 1400,     0
            defw 0, 0,  0,    0, 30000

colWeights  defb 3, 4, 5, 7, 5, 4, 3            ; center preferred column weights
colScore    defw $0000, $0000, $0000, $0000, $0000, $0000, $0000            ; each column has a 2 byte score
ycolScore   defw $0000, $0000, $0000, $0000, $0000, $0000, $0000            ; each column has a 2 byte score
storeBI     db 0                                ; last BI cell, used for undo processing

; originl board state and working snapshots for AI processing
; copies of columns(), lineScore(), lineCount()
originalSnapshot defs boardStateLen, 0
snapA            defs boardStateLen, 0
snapB            defs boardStateLen, 0


copyWhoseGo     db 0
moveNowInd      db 0

initialScore  equ -32767

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
; Purpose   :   helper function to do (IY) = (IY) + DE
; Input     :   IY - points to colScore for column being evaluated
;               DE - score to add/subtract
; Output    :   none
; Updates   :   colScore()
; Preserves :   BC, DE, HL, IX, IY
;---------------------------------------------------------------------------------------------------------------
        push hl

        push iy
        pop hl
        ld   a,(hl)        ; low
        add  a,e           ; low += E
        ld   (hl),a
        inc  hl
        ld   a,(hl)        ; high
        adc  a,d           ; high += D + carry
        ld   (hl),a
        
        pop hl
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


aiMove:
        ld c, 0                                 ; column being tested
        ld b, 7                                 ; loop for 7 columns
        ld iy, colScore                         ; point to column score table
.loop:
        ld a, c                                 ; get column
        ld hl, columns                          ; point to columns array
        add hl, a                               ; add in offset
        ld a, (hl)                              ; get count of chips in this column

        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        call evaluateWinBlock
        call evaluateColumnWeights
.nextCol:
        inc c                                   ; advance to next column
        inc iy                                  ; advance score pointer
        inc iy                                  ; .. its 2 bytes
        djnz .loop                              ; loop for all 7 columns
        ret



evaluateWinBlock:
; this routine will be called for each valid column
; we :
;      simulate yellow in the column
;          - save current board state
;          - evaluate for immediate yellow win  +10000, i.e. block
;          - update score
;          - reverse yellow move
;      simulate a red in the column
;          - save current board state
;          - evaluate for immediate red win +30000, i.e. win !
;          - update score
;          - reverse red move

        call simulateYellow
        call simulateRed
        ret

evaluateColumnWeights:
        push bc
        ld hl, colWeights                       ; point to weights array
        ld a, c                                 ; restore column
        add hl, a                               ; add in offset

        ld d, 0
        ld e, (hl)                              ; get weight
        call updateColScore                     ; (IY) = (IY) +/- DE
        pop bc
        ret

subsequentYellow:
; here we are simulating yellow in each of the columns 0..6 AFTER placing red chip
; we score each board position into yellowColsScore
; then we pick the biggest value (it'll still be negative) but we assume yellow will pick the best for it, not us
; this is then used as a delta for colScore()
        ld c, 0                                 ; column being tested
        ld b, 7                                 ; loop for 7 columns
.loop:
        ld a, c                                 ; get column
        ld hl, columns                          ; point to columns array
        add hl, a                               ; add in offset
        ld a, (hl)                              ; get count of chips in this column

        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        call simulateYellow
.nextCol:
        inc c                                   ; advance to next column
        djnz .loop                              ; loop for all 7 columns
        ret

simulateYellow:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a yellow chip and evaluate
; Input   :   C - column being evaluated
;             HL - columns(c)
; Output  :   A  - =4 if win, otherwise 255
;             DE - score adjustment to apply
; Updates :   
;---------------------------------------------------------------------------------------------------------------
        push bc
        push hl
        call createSnapA                        ; save current columns(), lineScore(), lineCount() into undoColumns ...

        ld a, (hl)                              ; get chips in column(c)
        inc a                                   ; increase chip count for the column
        ld (hl), a                              ; place in column
        ld b, a                                 ; save column chip count
        ld a, yellowGo                          ; simulate a yellow chip
        ld (whoseGo), a                         ; save it for setSlotValue routine

        calculateBoardIndex                     ; calculate BI (needs B=column chip count, C=column)
        call setSlotValue                       ; place on board and evaluate
        cp 4                                    ; win detected?
        jr z, .Win                              ; branch if so

.noWin
        ; code here will evaluate yellows move
        ; but we won't actuall use it on the first "block immediate yellow win" call
        pop hl
        pop bc
        call restoreFromSnapA
        ret

.Win:
        LD DE, 10000
        call updateColScore
        ;call debugs
        call restoreFromSnapA
        pop hl
        pop bc
        ret

simulateRed:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a red chip and evaluate
; Input   :   C - column being evaluated
;             HL - columns(c)
; Output  :   A  - =4 if win, otherwise 255
;             DE - score adjustment to apply
; Updates :   
;---------------------------------------------------------------------------------------------------------------
        push hl
        push bc
        call createSnapA                        ; save current columns(), lineScore(), lineCount() into undoColumns ...

        ld a, (hl)                              ; get chips in column(c)
        inc a                                   ; increase chip count for the column
        ld (hl), a                              ; place in column
        ld b, a                                 ; save column chip count
        ld a, redGo                             ; simulate a yellow chip
        ld (whoseGo), a                         ; save it for setSlotValue routine

        ; calculate row/column and hence BI (0..41)
        calculateBoardIndex                     ; calculate BI (needs B=column chip count, C=column)
        call setSlotValue                       ; place on board and evaluate
        call restoreFromSnapA
        pop bc
        pop hl
        ret
