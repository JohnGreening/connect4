

getYPixel:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   return sprite Y pixel position for a given row value (1-6, 0 when choosing column)
; Input   :   A = row value
; Output  :   A = Y pixel value
;---------------------------------------------------------------------------------------------------------------
        push de
        ld de, YPixelLookup
        add de, a
        ld a, (de)
        pop de
        ret

getXPixel:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   return sprite X pixel position for a given col value (0-6)
; Input   :   A = col value
; Output  :   A = X pixel value
;---------------------------------------------------------------------------------------------------------------
        push de
        ld de, XPixelLookup
        add de, a
        ld a, (de)
        pop de
        ret


setSlotValue:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Determine board state (after columns() has been updated)
;             - return if a win for either player has been detected
;             - value the board iro that player
; Input   :   A = board index BI (0..41)
; Output  :   A = win line if win detected, otherwise 255
;----------------------------------------------------------------------------------------------------------------
        ; return the list of win lines that contain this chip (passed as BI 0..41)      
        ld hl, 0
        ld (boardScore), hl
        
        ld iy, cellWinLines                     ; point to base of cellWinLines
        ld d, a                                 ; 
        ld e, 14                                ; each line is 14 bytes for each BI
        mul d, e                                ; do the multiplication
        add iy, de                              ; and hence point IY to the winLines for this BI

        ld a, 255                               ; default to no win detected
        ld (winLine), a                         ; set it

.loop:
        ld a, (iy)                              ; get one of the win lines this chip is in
        cp 255                                  ; are we at the end of the list
        jr z, .end                              ; branch if so, end of processing for this BI
        ld hl, lineScore                        ; point to base of lineScore
        add hl, a                               ; point to lineScore for this winline


        ld a, (whoseGo)                         ; who moved last
        cp yellowGo                             ; was it yellow?
        jr z, .yellowGo                         ; branch for yellow if so
        jr    .redGo                            ; branch for red

.redGo:
        call scoreRedPosition
        jr .winCheck                            ; and check for a win

.yellowGo:
        call scoreYellowPosition

.winCheck:        
        ld a, b                                 ; get the saved lineScore
        cp 4                                    ; has anyone won?
        jr nz, .nextWinLine                     ; branch if not
        ld a, (iy)                              ; otherwise get the winline
        ld (winLine), a                         ; and store it

.nextWinLine:
        inc iy                                  ; point to next winline        
        jr .loop                                ; otherwise check next winline
.end:        
        ld a, (winLine)                         ; return with any winline
        ret

scoreYellowPosition:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Update lineScore() & lineCount() for this winline
;             value the line according to yellow's LUT
; Input   :   HL = lineScore(winline)
; Output  :   A = updated lineScore(winline)
;----------------------------------------------------------------------------------------------------------------       
        ; update lineScore(?) and lineCount(?)
        inc (hl)                                ; update lineScore(). +1 for yellow
        ld a, (hl)                              ; get the new value
        ld b, a                                 ; save lineScore() in B for LUT
        add hl, 69                              ; point to lineCount()
        inc (hl)                                ; add 1 to lineCount()
        ld a, (hl)                              ; get the new value
        ld c, a                                 ; save lineCount() in C for LUT

        ; we only score for lineScores(?) of 0, 1, 2, 3 & 4 (red moves can make it negative)
        ld a, b
        bit 7, a
        jr nz, .end

        ; calculate offset to lookup table
        ; this is 5 rows of 5 values, row is lineCount(?), column is lineScore(?)
        ld hl, yellowLUT                        ; point to base of table
        call readLUT                            ; determine LUT score to use in DE

        ld hl, (boardScore)                     ; get current score value
        add hl, de                              ; add in this winlines score
        ld (boardScore), hl                     ; otherwise store higher value
        
.end:
        ld a, b                                 ; return with lineScore (+4 indicates a win)
        ret

scoreRedPosition:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Update lineScore() & lineCount() for this winline
;             value the line according to Red's LUT
; Input   :   HL = lineScore(winline)
; Output  :   A = updated lineScore(winline)
;----------------------------------------------------------------------------------------------------------------
        ; update lineScore(?) and lineCount(?)
        dec (hl)                                ; update lineScore(). -1 for red
        ld a, (hl)                              ; get the new value
        ld b, a                                 ; save lineScore() in B for LUT
        add hl, 69                              ; point to lineCount()
        inc (hl)                                ; add 1 to lineCount()
        ld a, (hl)                              ; get the new value
        ld c, a                                 ; save lineCount() in C for LUT

        ; we only score for lineScores(?) of  -1, -2, -3 & -4 (red moves can make it negative)
        ld a, b
        bit 7, a
        jr z, .end

        ; make it positive
        neg
        ld b, a

        ; calculate offset to lookup table
        ; this is 5 rows of 5 values, row is lineCount(?), column is lineScore(?)
        ld hl, redLUT                           ; point to base of table
        call readLUT                            ; determine LUT score to use in DE

        ld hl, (boardScore)                     ; get current score value
        add hl, de                              ; add in this winlines score
        ld (boardScore), hl                     ; otherwise store higher value

.end:
        ld a, b                                 ; return with lineScore (+4 indicates a win)
        ret


readLUT:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   Read Look up table (LUT) so that given lineScore & lineCount a strengh value is determined
;             red and yellow have different LUT values
; Input   :   HL = base of LUT 
;             B  = lineScore(winline)
;             C  = lineCount(winline)
; Output  :   DE = LUT delta to apply to lineScore(column)
; Updates :   none
;---------------------------------------------------------------------------------------------------------------
        push bc

        ; calculate offset to lookup table
        ; this is 5 rows of 5 values, row is lineCount(?), column is lineScore(?)
        ld d, c                                 ; return the lineCount(winline) 0..4 chips
        ld e, 5                                 ; ready to multiply by 5
        mul d, e                                ; do it
        ld  a, e                                ; get the result
        add b                                   ; add in the lineScore(winline)
        add a, a                                ; each LUT is 2 bytes, so double

        add hl, a                               ; now point to value needed to score line

        ld e, (hl)                              ; lookup score adjustment (low byte)
        inc hl                                  ; point to high byte
        ld d, (hl)                              ; look up score adjustment (high byte)

        pop bc
        ret

winnerDisplay:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   highlight where a win has been detected
; Input   :   A = winLine index
; Output  :   none
; Updates :   none
;---------------------------------------------------------------------------------------------------------------
        ld ix, winLines                         ; point to base of winLines array
        ld d, a                                 ; set D = winline where win detected
        ld e, 4                                 ; each row is 4 bytes
        mul d, e                                ; multiply out to get the index offset
        add ix, de                              ; add to base

        call killSprites
        call spritesOnTop

        ld b, 4                                 ; there are 4 entries to process
nextWinChip:
        ld a, (spriteIndex)                     ; get the current sprite #
        add 4                                   ; each big chip is 4 sprites
        ld (spriteIndex), a                     ; save it back

        ld a, (ix)                              ; get cell (0..41) in the winLine
        call displayWinChip                     ; draw our big chip
        inc ix                                  ; advance to next cell in the winline
        djnz nextWinChip                        ; repeat if more to do
        ret                                     ; done !

displayWinChip:
        call calcRowCol                         ; given A=0..41 return D (row 1-6) and E (col 1-7)
        ld a, d                                 ; get row
        call getYPixel                          ; calculate Y pixel
        ld h, 0
        ld l, a
        ld (chipY), hl                          
        ld a, e                                 ; get col
        call getXPixel                          ; calculate X pixel
        ld h, 0
        ld l, a
        ld (chipX), hl
        call displaychip                        ; show the chip
        ret

calcRowCol:
; A = 0 .. 41
; return D = row (1-6)
;        E = col (1-7)
        push bc
        ld b, 0
div7:
        cp 7
        jr c, div7Done
        sub 7
        inc b
        jr div7
div7Done:
        ld e, a
        ld d, b
        inc d
        pop bc
        ret

drawTilechip:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   when done with sprite, we replace it with the equivalent tiles
; Input   :   none
; Output  :   none
; Updates :   none
;---------------------------------------------------------------------------------------------------------------

; first calculate tile position
; row part is ((row *4) + 4) * 40
        ld a, (rowSelected)                     ; get row final (1..6)
        ld d, a                                 ; save in D
        ld e, 4                                 ; we multiply by 4
        mul d, e                                ; calc (row * 4)
        add de, 4                               ; calc ((row * 4) + 4)

        ld d, 40                                ; ready multiply by 40
        mul d, e                                ; calc R = ((row * 4) + 4) * 40

; column part is (column * 4) + 1
        ld a, (columnSelected)                  ; get column (0..6)
        inc a                                   ; make it 1..7
        add a, a                                ; calc (column * 2)
        add a, a                                ; calc (column * 4)
        inc a                                   ; calc C = (column * 4) + 1
        add de, a                               ; complete offset is R + C

; we now have the offset into the tilemap, so add in tilemap        
        add de, tileMapData                     ; add to tilemap base

        ld b, yellowTile                        ; assume yellow chip
        ld a, (whoseGo)
        dec a
        cp yellowChip
        jr z, .replaceSprite
        ld b, redTile                           ; ok, its a red chip

.replaceSprite:
        ld a, b                                 ; get start tile pattern to use
        ex de, hl                               ; we favour HL rather than DE for some reason

; tile line 1
        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a

        add hl, 38
        inc a

; tile line 2
        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a

        add hl, 38
        inc a

; tile line 3
        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a

        ret

;
; Position and show chip given selected column
;
positionChip:
        LD A, (ix +columnNo)                    ; lookup selected column (0-6) from columns
        call getXPixel                          ; lookup X pixel coord for this column

        ld h, 0                                 ; set X coord high byte
        ld l, a                                 ; set X coord low byte
        LD (chipX), hl                          ; store coord for sprite routine

        ld h, 0                                 ; default Y coord high byte
        ld l, 24                                ; default Y coord low byte
        ld (chipY), hl                          ; store coord for sprite routine
        CALL displaychip                        ; finally show the sprite
        ret                                     ; ... and return

movechipDown:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   column selected, drop the chip into appropriate column
; Input   :   (rowSelected) 1..6, (columnSelected) 0..6
; Output  :   none
; Updates :   none
;---------------------------------------------------------------------------------------------------------------
        ld a, (rowSelected)                     ; get the row we drop to (1-6)
        call getYPixel                          ; return in A the y pixel drop position
        ld e, a                                 ; save it for compare
.mcd
        ld hl, (chipY)                          ; get chip current Y pixel 
        inc hl                                  ; add 1 pixel
        ld a, l                                 ; prepare for compare
        cp e                                    ; compare with final drop position
        jr z, .mcdExit                          ; branch if final position reached
        ld (chipY), hl                          ; otherwise save new Y position
        call displaychip                        ; display the chip in new position
        jr .mcd                                 ; and loop
.mcdExit:
        call drawTilechip                       ; final resting place, replace with an equivalent tile
        ret

displaychip:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   display a connect 4 counter on screen
; Input   :   none
; Output  :   none
; Updates :   none
;---------------------------------------------------------------------------------------------------------------
        push bc
        LD A, (spriteIndex)                     ; set the sprite index
        ld c, a                                 ; store in c
        NEXTREG $34, A                          ; set sprite to activate

        LD HL, (chipX)
        LD A, L                                 ; get sprite X lsb
        NEXTREG $35, A                          ; set attr byte 0 of port $0057

        LD A, H                                 ; get sprite X msb
        AND 1                                   ; only need bit 0 of X msb
        NEXTREG $37, A                          ; bits 7-4 palette offset
                                                ;        3 1=enable X mirroring
                                                ;        2 1=enable Y mirroring
                                                ;        1 1=rotate 90 clockwise
                                                ;        0 msb of X
                                                ; this is attr byte 2 of port $0057
        
        LD HL, (chipY)
        LD A, L                                 ; get sprite Y lsb
        NEXTREG $36, A                          ; set attr byte 1 of port $0057

        LD A, (whoseGo)                         ; get whose go it is yellow=1, red=5
        dec a                                   ; -1 to get yellowChip = 0, redChip = 4
        LD B, A
        OR %11000000                            ;
;        OR %10000000                           ;

        NEXTREG $38, A                          ; bits 7 1=make sprite visible
                                                ;      6 1=enable optional attr byte 4
                                                ;      5-0 pattern 0-63, 7th bit in byte 4

        LD A, %00100000                         ; bits 7-6 00 = anchor sprite 8 bit (01=rel)
                                                ; bit    5 1=unified, 0=composite
                                                ; bits 4-3 X axis scale
                                                ; bits 2-1 Y axis scale
                                                ; bit    0 msb of Y coord
        NEXTREG $39, A                          ;

;-----------------------------------------------------------------------------------------------
; first relative sprite
;-----------------------------------------------------------------------------------------------
        inc c                                   ; add 1 to sprite index
        LD A, c                                 ; set first RELATIVE sprite
        NEXTREG $34, A                          ; set sprite to activate

        LD A, 16                                ; X offset from ANCHOR sprite is 16
        NEXTREG $35, A                          ; set attr byte 0 of port $0057

        LD A, 0                                 ; Y offset from ANCHOR sprite is 0
        NEXTREG $36, A                          ; set attr byte 1 of port $0057

        LD A, 1                                 ; use ANCHOR palette offset
        NEXTREG $37, A                          ; set attr byte 2 of port $0057

        INC B                                   ; add 1 to chip pattern
        LD A, %11000000                         ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                          ; set attr byte 3 of port $0057

        LD A, %01000000                         ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                          ; set attr byte 4 of port $0057

;-----------------------------------------------------------------------------------------------
; second relative sprite
;-----------------------------------------------------------------------------------------------
        inc c
        LD A, c                                 ; set second RELATIVE sprite
        NEXTREG $34, A                          ; set sprite to activate

        LD A, 0                                 ; X offset from ANCHOR sprite is 0
        NEXTREG $35, A                          ; set attr byte 0 of port $0057

        LD A, 16                                ; Y offset from ANCHOR sprite is 16
        NEXTREG $36, A                          ; set attr byte 1 of port $0057

        LD A, 1                                 ; use ANCHOR palette offset
        NEXTREG $37, A                          ; set attr byte 2 of port $0057

        INC B
        LD A, %11000000                         ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                          ; set attr byte 3 of port $0057

        LD A, %01000000                         ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                          ; set attr byte 4 of port $0057

;-----------------------------------------------------------------------------------------------
; third relative sprite
;-----------------------------------------------------------------------------------------------
        inc c
        LD A, c                                 ; set third RELATIVE sprite
        NEXTREG $34, A                          ; set sprite to activate

        LD A, 16                                ; X offset from ANCHOR sprite is 0
        NEXTREG $35, A                          ; set attr byte 0 of port $0057

        LD A, 16                                ; Y offset from ANCHOR sprite is 16
        NEXTREG $36, A                          ; set attr byte 1 of port $0057

        LD A, 1                                 ; use ANCHOR palette offset
        NEXTREG $37, A                          ; set attr byte 2 of port $0057

        INC B
        LD A, %11000000                         ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                          ; set attr byte 3 of port $0057

        LD A, %01000000                         ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                          ; set attr byte 4 of port $0057

        pop bc
        RET

initCellWinLines:
;---------------------------------------------------------------------------------------------------------------
; Purpose :   create cellWinLines table, i.e. 42 rows of between 3 to 13 winLines values
; Input   :   none
; Output  :   none
; Updates :   none
;---------------------------------------------------------------------------------------------------------------
	call initCellWinLineRows                ; initialise 42 line/mask pair values with 255
	ld iy, cellWinLines                     ; point to this table
	ld b, 0                                 ; B is the cell board index (BI) to search for

l4:	ld ix, winLines                         ; point to the table holding what cells are applicable to each of the 69 win lines 
	push iy                                 ; save the winPairLookup table
	ld c, 0                                 ; C is the win line counter (WL)

l5:	call chkLineForB                        ; check winning line for an occurance of B
	ld de, 4                                ; prepare to point to the next win line
	add ix, de                              ; do it
	inc c                                   ; update winline counter
	ld a, c                                 ; get the winline counter
	cp 69                                   ; there are 69 (0..68)
	jr nz, l5                               ; branch if more winlines to process for this BI
	pop iy                                  ; this BI value complete, so restore WL/mask pointer to start for this BI
	ld de, 14                               ; max 13 entries with a 255 end marker, so add 14
	add iy, de                              ; do the add
	inc b                                   ; BI = BI + 1
	ld a, b                                 ; put in A to check
	cp 42                                   ; are we at the end (0..41)
	jr nz, l4                               ; branch if more to do
	ret                                     ; otherwise we are complete
		
chkLineForB:	
	push ix                                 ; save current winning line
	push bc                                 ; save the BI and WL
	ld d, b                                 ; temporarily use D for BI
	ld b, 4                                 ; each winning line is 4 values
l2:	ld a, (ix)                              ; get one of the winning line values
	cp d                                    ; is it BI
	jr z, foundIndex                        ; branch if it is
	inc ix                                  ; otherwise point to the next
	djnz l2                                 ; and repeat for all 4 values
l3:	pop bc                                  ; done, so restore BI, WL
	pop ix                                  ; and the current winning line
	ret                                     ; and return
    
foundIndex:	
	ld a, c                                 ; get the win line index
	ld (iy), a                              ; save the win line index in cellWinLines
	inc iy                                  ; and advance IY for the next winline write
	jr l3                                   ; jump back, done with this winning line
			
	
initCellWinLineRows:
	ld bc, 42*14                            ; any position (0-41) can exist in up to 13 win lines (add 1 extra, 14, for end marker)
	ld hl, cellWinLines                     ; point to cellWinlines table
.loop:	ld a, 255                           ; we will initially set everything to 255 end marker
	ld (hl), a                              ; set to default 255 end marker
	inc hl                                  ; point to next item in table
	dec bc                                  ; decrease count
	ld a, b                                 ; test for BC=0
	or c
	jr nz, .loop                            ; loop if not zero
	ret                                     ; ... otherwise return