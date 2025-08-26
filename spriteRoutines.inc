spriteIndex     db 0
chipX           defw 36
chipY           defw 24
chipPattern     db 0

columnSelected  db 1                            ; holds last column 1 - 7
rowSelected     db 0                            ; holds row used    0 - 6 (0 when selecting column)
lastPosition    db 0                            ; board index 0 - 41 used
lastGo          db 0                            ; last board value used 1 or 5

columns         defs 7, 0                       ; how many chips are in each column
board           defs 42, 0
linesYellow     defs 69, 0
linesRed        defs 69, 0

maxColumn       equ 7
minColumn       equ 1
maxRow          equ 6

; sprite patterns
yellowChip      equ 0                           ; sprites 0 - 3
redChip         equ 4                           ; sprites 4 - 7

; tile patterns
yellowTile      equ 1                           ; tiles 1 - 9
redTile         equ 24                          ; tiles 24 - 32

; board array values
yellowGo        equ yellowChip +1
redGo           equ redChip +1
winValue        equ %00001111
yellowWin       equ 4 * yellowGo
redWin          equ 4 * redGo

INCLUDE "lookupTables.inc"

initialiseBoard:
        ld a, 0
        ld hl, columns
        ld b,  7 +42 +69 +69
ib1:
        ld (hl), a
        inc hl
        djnz ib1
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
        inc e
        ld d, b
        inc d
        pop bc
        ret

getYPixel:
; A= row value, where 0 = positioning chip before dropping
; return in A the Y pixel coord of the sprite
        push de
        ld de, spritePixelY
        add de, a
        ld a, (de)
        pop de
        ret

getXPixel:
; A= column value, 1 - 7
; return in A the X pixel coord of the sprite
        push de
        ld de, spritePixelX
        add de, a
        dec de
        ld a, (de)
        pop de
        ret
newGo:
        ld a, 0
        ld (spriteIndex), a

        ld a, 0
        ld (rowSelected), a        
        call getYPixel
        ld h, 0
        ld l, a
        ld (chipY), hl

        ld a, 1
        ld (columnSelected), a
        call getXPixel
        ld h, 0
        ld l, a
        ld (chipX), hl
        
        ld a, (chipPattern)
        xor 4
        ld (chipPattern), a

        call displaychip

; wait until space bar is NOT being pressed
; otherwise might take as a go when user not ready
ng1:
        LD BC, $7ffe
        IN A, (C)
        AND %00000001
        jr z, ng1
        ret


setSlotValue:
; point to the list of lines that contain the board position
; there could be up to 13 lines
        ld ix, winPairLookupBase                ; point to the WL/mask base table
        ld a, (lastPosition)                    ; get last BI
        ld d, a                                 ; save in D ready for offset to add to IX
        ld e, 28                                ; each WL/mask line is 28 bytes for each BI
        mul d, e                                ; do the multiplication
        add ix, de                              ; and hence point IX to thr WL/mask for this BI

        ld a, (lastGo)
        cp yellowGo
        ld de, linesYellow
        jr z, .baseSet
        ld de, linesRed
.baseSet:
.next
        or a                                    ; reset carry flag
        ld a, (ix)                              ; get the line this chip is in
        cp 255                                  ; are we at the end of the list
        ret z                                   ; exit if so

        ld c, a                                 ; save the line in case of a win for winnerDisplay
        ld l, a                                 ; point to slot
        ld h, 0
        add hl, de

        ld a, (hl)                              ; get current slot value
        or (ix+1)                               ; add new chip
        ld (hl), a                              ; and save it back
        cp winValue                             ; is a win detected
        scf                                     ; set carry in anticpation of win
        ret z                                   ; and exit if so
        ld bc, 2                                ; otherwise carry on with list
        add ix, bc
        jr .next



;checkWin:
;        ld ix, winningLines
;        ld c, winningCombos        
;nextLine:
;        xor a
;        ld d, 0
;        ld b, 4
;        call checkLine
;        ld a, d
;        cp yellowWin
;        jr z, winner
;        cp redWin
;        jr z, winner
;        dec c
;        jr nz, nextLine
;        xor a
;        ret
;
;winner:
;        scf
;        ret
;
;checkLine:        
;        ld a, (ix +0)                           ; pick up board element we want
;        ld hl, board                            ; set base
;        add hl, a                               ; add in element offset
;        ld a, d                                 ; get current line value
;        add (hl)                                ; add in this element
;        ld d, a                                 ; store back in d
;        inc ix                                  ; next element in line
;        djnz checkLine                          ; repeat for 4 elements
;        ret

winnerDisplay:
        call killSprites
        call spritesOnTop
        ld ix, winningLines
        ld d, c
        ld e, 4
        mul d, e
        add ix, de
;        dec ix
;        dec ix
;        dec ix
;        dec ix
        ld b, 4
nextWinChip:
        ld a, (spriteIndex)
        add 4
        ld (spriteIndex), a
        ld a, (ix)
        call displayWinChip
        inc ix
        djnz nextWinChip
        ret
displayWinChip:
        call calcRowCol
        ld a, d
        call getYPixel
        ld h, 0
        ld l, a
        ld (chipY), hl
        ld a, e
        call getXPixel
        ld h, 0
        ld l, a
        ld (chipX), hl
        call displaychip
        ret

drawTilechip:
;----------------------------------------------------------------------------
; replace the sprite with tile equivalent
;----------------------------------------------------------------------------

; first calculate tile position
; row part is ((row *4) + 4) * 40
        ld a, (rowSelected)
        ld d, a
        ld e, 4
        mul d, e
        add de, 4
        ld d, 40
        mul d, e
; column part is (column * 4) + 1
        ld a, (columnSelected)
        add a, a
        add a, a
        inc a
        add de, a
; we now have the offset into the tilemap, so add in tilemap        
        add de, tileMapData

        ld b, yellowTile                        ; assume yellow chip
        ld a, (chipPattern)
        cp yellowChip
        jr z, replaceSprite
        ld b, redTile                           ; ok, its a red chip

replaceSprite:
        ld a, b
        ex de, hl

        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        add hl, 38
        inc a

        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        add hl, 38
        inc a

        ld (hl), a
        inc hl
        inc a
        ld (hl), a
        inc hl
        inc a
        ld (hl), a

; determine board aray element
        ld a, (rowSelected)                     ; get row
        dec a                                   ; make 0 based
        ld d, a                                 ; put in D ready for multiplication
        ld e, maxColumn                         ; multiply by 7
        mul d, e                                ; do it
        ld a, (columnSelected)                  ; get the column 
        dec a                                   ; make 0 based, a= 0 - 41
        add a, e

        ld (lastPosition), a                    ; save it for later
        add de, a                               ; hence offset into array
        add de, board                           ; and add array base

        ld a, (chipPattern)                     ; get chipPattern, 0 or 4
        inc a                                   ; board value is 1 or 5
        ld (de), a
        ld (lastGo), a

        ret


movechipLR:
        LD A, (columnSelected)                  ; get column selected (1 - 7)
        call getXPixel
        ld h, 0
        ld l, a
        LD (chipX), hl
        CALL displaychip
        ret

movechipDown:
        ld hl, (chipY)
        inc hl
        ld a, l
        cp e
        jr z, mcd1
        ld (chipY), hl
        call displaychip
        jr movechipDown
mcd1:
        call drawTilechip
        ret

displaychip:
        push bc
        LD A, (spriteIndex)             ; set the sprite index
        ld c, a                         ; store in c
        NEXTREG $34, A                  ; set sprite to activate

        LD HL, (chipX)
        LD A, L                         ; get sprite X lsb
        NEXTREG $35, A                  ; set attr byte 0 of port $0057

        LD A, H                         ; get sprite X msb
        AND 1                           ; only need bit 0 of X msb
        NEXTREG $37, A                  ; bits 7-4 palette offset
                                        ;        3 1=enable X mirroring
                                        ;        2 1=enable Y mirroring
                                        ;        1 1=rotate 90 clockwise
                                        ;        0 msb of X
                                        ; this is attr byte 2 of port $0057
        
        LD HL, (chipY)
        LD A, L                         ; get sprite Y lsb
        NEXTREG $36, A                  ; set attr byte 1 of port $0057

        LD A, (chipPattern)          ; get pattern index to use
        LD B, A
        OR %11000000                    ;
;        OR %10000000                   ;

        NEXTREG $38, A                  ; bits 7 1=make sprite visible
                                        ;      6 1=enable optional attr byte 4
                                        ;      5-0 pattern 0-63, 7th bit in byte 4

        LD A, %00100000                 ; bits 7-6 00 = anchor sprite 8 bit (01=rel)
                                        ; bit    5 1=unified, 0=composite
                                        ; bits 4-3 X axis scale
                                        ; bits 2-1 Y axis scale
                                        ; bit    0 msb of Y coord
        NEXTREG $39, A                  ;


        inc c
        LD A, c                         ; set first RELATIVE sprite
        NEXTREG $34, A                  ; set sprite to activate

        LD A, 16                        ; X offset from ANCHOR sprite is 16
        NEXTREG $35, A                  ; set attr byte 0 of port $0057

        LD A, 0                         ; Y offset from ANCHOR sprite is 0
        NEXTREG $36, A                  ; set attr byte 1 of port $0057

        LD A, 1                         ; use ANCHOR palette offset
        NEXTREG $37, A                  ; set attr byte 2 of port $0057

        INC B
        LD A, %11000000                 ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                  ; set attr byte 3 of port $0057

        LD A, %01000000                 ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                  ; set attr byte 4 of port $0057

        inc c
        LD A, c                         ; set second RELATIVE sprite
        NEXTREG $34, A                  ; set sprite to activate

        LD A, 0                         ; X offset from ANCHOR sprite is 0
        NEXTREG $35, A                  ; set attr byte 0 of port $0057

        LD A, 16                        ; Y offset from ANCHOR sprite is 16
        NEXTREG $36, A                  ; set attr byte 1 of port $0057

        LD A, 1                         ; use ANCHOR palette offset
        NEXTREG $37, A                  ; set attr byte 2 of port $0057

        INC B
        LD A, %11000000                 ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                  ; set attr byte 3 of port $0057

        LD A, %01000000                 ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                  ; set attr byte 4 of port $0057

        inc c
        LD A, c                         ; set second RELATIVE sprite
        NEXTREG $34, A                  ; set sprite to activate

        LD A, 16                         ; X offset from ANCHOR sprite is 0
        NEXTREG $35, A                  ; set attr byte 0 of port $0057

        LD A, 16                        ; Y offset from ANCHOR sprite is 16
        NEXTREG $36, A                  ; set attr byte 1 of port $0057

        LD A, 1                         ; use ANCHOR palette offset
        NEXTREG $37, A                  ; set attr byte 2 of port $0057

        INC B
        LD A, %11000000                 ; set visible and enable byte 4, patt offset 1
        OR B
        NEXTREG $38, A                  ; set attr byte 3 of port $0057

        LD A, %01000000                 ; set as RELATIVE and use relative pattern
        NEXTREG $79, A                  ; set attr byte 4 of port $0057

        pop bc
        RET

initWinPairLookup:
	call initLineMaskPair                   ; initialise 42 line/mask pair values with 255
	ld iy, winPairLookupBase                ; point to this table
	ld b, 0                                 ; B is the board index (BI) to search for

l4:	ld ix, winningLines                     ; point to the winning lines table
	push iy                                 ; save the winPairLookup table
	ld c, 0                                 ; C is the winning line counter (WL)

l5:	call chkLineForB                        ; check winning line for an occurance of B
	ld de, 4                                ; prepare to point to the next winning line
	add ix, de                              ; do it
	inc c                                   ; update winline counter
	ld a, c                                 ; get the winline counter
	cp 69                                   ; there are 69 (0..68)
	jr nz, l5                               ; branch if more winlines to process for this BI
	pop iy                                  ; this BI value complete, so restore WL/mask pointer to start for this BI
	ld de, 28                               ; max 13 entries with a 255 end marker, so add 28
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
	call setMask                            ; determine the mask value in E, i.e. 8, 4, 2, 1
	ld a, c                                 ; get the winning line index
	ld (iy), a                              ; save the winning line number
	inc iy                                  ; and advance IY for the mask value
	ld a, e                                 ; get the mask value
	ld (iy), a                              ; save the mask value
	inc iy                                  ; and advance for next WL/mask pair for this BI
	jr l3                                   ; jump back, done with this winning line
		
setMask:	
	ld a, b                                 ; which value matched BI in the winning line lookup
	cp 4                                    ; was it 1st
	jr z, p0                                ; branch if so
	cp 3                                    ; was it 2nd
	jr z, p1                                ; branch if so
	cp 2                                    ; etc ..
	jr z, p2
	cp 1
	jr z, p3
	ret
p0:	ld e, 8                                 ; 1st spot, so set mask as %0000 1000
	ret
p1:	ld e, 4                                 ; 2nd spot, so set mask as %0000 0100
	ret
p2:	ld e, 2
	ret
p3:	ld e, 1
	ret
	
	
initLineMaskPair	
	ld bc, 42*28
	ld hl, winPairLookupBase
l1:	ld a, 255
	ld (hl), a
	inc hl
	dec bc 
	ld a, b
	or c
	jr nz, l1 
	ret