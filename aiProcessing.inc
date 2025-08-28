

colWeights  defb 3, 4, 5, 7, 5, 4, 3            ; center preferred column weights
colScore    defw 0, 0, 0, 0, 0, 0, 0            ; each column has a 2 byte score
lastAICell  db 0                                ; last BI cell, used for undo processing

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

        ld hl, colScore
        ld b, 7
        ld de, -32768
.loop   ld (hl), e
        inc hl
        ld (hl), d
        inc hl
        djnz .loop
        ret


updateColScore:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   helper function to do (HL) = (HL) + DE
; Input   :   HL points to colScore for column being evaluated
;             DE score to add/subtract
; Output  :   none
; Updates :   colScore
;---------------------------------------------------------------------------------------------------------------
; HL -> word (low at (HL), high at (HL+1))
; DE = signed 16-bit delta
; Preserves: BC, DE
; Clobbers:  AF, HL moves to (HL+1) at exit
        ld   a,(hl)        ; low
        add  a,e           ; low += E
        ld   (hl),a
        inc  hl
        ld   a,(hl)        ; high
        adc  a,d           ; high += D + carry
        ld   (hl),a
        ret

;        push bc
;        ; Fetch existing value into BC
;        ld      c,(hl)         ; C = low byte
;        inc     hl
;        ld      b,(hl)         ; B = high byte
;
;        ; Add DE into BC (16-bit signed add works same as unsigned)
;        ex      de,hl          ; swap: HL = DE, DE = (colScore)
;        add     hl,bc          ; HL = old + adjustment
;        ex      de,hl          ; DE = result, HL = addr+1
;
;        ; Write result back
;        ld      (hl),d         ; high
;        dec     hl
;        ld      (hl),e         ; low
;
;        pop bc
;        ret

pickBestAIMove:
; Input:  colScore = 7 signed 16-bit words (little-endian)
; Output: A = index (0..6) of the maximum signed value in colScore
; Clobbers: AF, BC, DE, HL
; Works on Z80 and Z80N

bestColFromScores:
; Find max signed 16-bit in colScore[7]
; Return: A = index 0..6 of max
; Clobbers: AF, BC, DE, HL
; Z80/Z80N-safe

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
        xor a                                   ; initialise a=0 (we need to be more efficient in the AI processing!)
        ld b, 7                                 ; loop for 7 columns
.loop:
        ld c, a                                 ; save column

        ld hl, columns                          ; point to columns array
        add hl, a                               ; add in offset
        ld a, (hl)                              ; get count of chips in this column

        ld de, 0                                ; initialise score to 0
        cp 6                                    ; is it full
        jr z, .noMove                           ; branch if so

        ld hl, colWeights                       ; point to weights array

        ld a, c                                 ; restore column
        add hl, a                               ; add in offset
        ld e, (hl)                              ; get weight
.setScore:
        ld a, c                                 ; restore column
        add a, a                                ; col score is 2 bytes so double a
        ld hl, colScore                         ; point to base
        add hl, a                               ; add offset
        call updateColScore                     ; (HL) = (HL) +/- DE
        ld a, c
        inc a
        djnz .loop
        ret
.noMove:
        ld a, c
        inc a
        djnz .loop
        ret


