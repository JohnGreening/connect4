

redLUT      defw 0, 0,  0,    0,     0
            defw 0, 6,  0,    0,     0
            defw 0, 0, 70,    0,     0
            defw 0, 0,  0, 1400,     0
            defw 0, 0,  0,    0, 30000

yellowLUT   defw 0, 0,  0,    0,     0
            defw 0, 5,  0,    0,     0
            defw 0, 0, 50,    0,     0
            defw 0, 0,  0, 2000,     0
            defw 0, 0,  0,    0, 10000

colWeights  defb 3, 4, 5, 7, 5, 4, 3            ; center preferred column weights
colScore    defw $0000, $0000, $0000, $0000, $0000, $0000, $0000            ; each column has a 2 byte score
ycolScore   defw $0000, $0000, $0000, $0000, $0000, $0000, $0000            ; each column has a 2 byte score

; originl board state and working snapshots for AI processing
; copies of columns(), lineScore(), lineCount()
originalSnapshot defs boardStateLen, 0
snapA            defs boardStateLen, 0
snapB            defs boardStateLen, 0


copyWhoseGo     db 0

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
        push af

        ld   l, (iy +0)                         ; get current low byte
        ld   h, (iy +1)                         ; get current high byte        
        add  hl, de                             
        ld   (iy +0), l                         ; store back low
        ld   (iy +1), h                         ; store back high

        pop af
        pop hl
        ret


pickBestAIMove:
        push bc
        push de
        push hl

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
        ; return highest value found
;        ld   e, c
;        ld   a, b
;        xor  080h
;        ld   d, a              ; DE = max (signed)

        ld   a, e                               ; return index

        pop bc
        pop de
        pop hl
        ret



DoAIMove:
        ld c, 0                                 ; column being tested
        ld b, 7                                 ; loop for 7 columns
.loop:
        ld a, c                                 ; get column
        ld hl, columns                          ; point to columns array
        add hl, a                               ; add in offset
        ld a, (hl)                              ; get count of chips in this column

        cp 6                                    ; is it full
        jr z, .nextCol                          ; branch if so

        ; for each of these, we need to keep hold of BC, HL
        call_preserve_HL_BC evaluateInitialYellow
        call_preserve_HL_BC evaluateRedYellow1Ply
        call_preserve_HL_BC evaluateColumnWeights
.nextCol:
        inc c                                   ; advance to next column
        djnz .loop                              ; loop for all 7 columns

        ld iy, colScore                         ; set to pick from best Red move
        call pickBestAIMove                     ; pick the best index from the scores
        inc a                                   ; A=0..6, so make 1..7
        ld (columnSelected), a                  ; store it

        ret


evaluateInitialYellow:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Initially place a yellow chip and evaluate - we want to block immediate yellow threat
; Input   :   C  - column being evaluated
;             HL - columns(c)
; Output  :   A  - winLine if win, otherwise 255
; Updates :   
;---------------------------------------------------------------------------------------------------------------
        useRedColScore        
        call createSnapA                        ; save current columns(), lineScore(), lineCount() into undoColumns ...

        inc (hl)                                ; increase chip count for the column
        ld a, (hl)                              ; read back the cip count now
        ld b, a                                 ; save column chip count
        ld a, yellowGo                          ; simulate a yellow chip
        ld (whoseGo), a                         ; save it for setSlotValue routine

        calculateBoardIndex                     ; calculate A=BI (needs B=column chip count, C=column)
        call setSlotValue                       ; place on board and evaluate
        cp 255                                  ; win detected?
        jr nz, .Win                             ; branch if so
        jr .end                                 ; skip score update
.Win:
        LD DE, 10000                            ; yellow immediate threat, so give high score
        call updateColScore                     ; update score. ** Needs to be colScore(c)
.end
        call restoreFromSnapA
        ret


evaluateRedYellow1Ply:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a Red chip and evaluate
; Input   :   C  - column being evaluated
;             A  - chip count currently in that column
;             HL - columns(c)
; Output  :   A  - winLine if win, otherwise 255
; Updates :   
;---------------------------------------------------------------------------------------------------------------

; this routine will be called for each valid column
        useRedColScore
        call simulateRed

        ret

evaluateColumnWeights:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   If nothing else to go on, favour the centre columns
; Input   :   C  - column being evaluated
;             HL - columns(c)
; Output  :   none
; Updates :   colScore(IY)
;---------------------------------------------------------------------------------------------------------------
        useRedColScore

        ld hl, colWeights                       ; point to weights array
        ld a, c                                 ; restore column
        add hl, a                               ; add in offset

        ld d, 0
        ld e, (hl)                              ; get weight
        call updateColScore                     ; (IY) = (IY) +/- DE

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
; Output  :   A  - = winLine if win, otherwise 255
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
;.noWin
        ; code here will evaluate yellows move
        ; but we won't actuall use it on the first "block immediate yellow win" call
        pop hl
        pop bc
        call restoreFromSnapA
        ret

;.Win:
;        LD DE, 10000
;        call updateColScore
;        ;call debugs
;        call restoreFromSnapA
;        pop hl
;        pop bc
;        ret

simulateRed:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   place a red chip and evaluate
; Input   :   C - column being evaluated
;             HL - columns(c)
; Updates :   
;---------------------------------------------------------------------------------------------------------------
        push bc
        push hl

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

        pop hl
        pop bc
        ret
