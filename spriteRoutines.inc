chipX           defw 36
chipY           defw 24
chipPattern     db 0

columnSelected  db 1
rowSelected     db 0
lastGo          db 0

columns         defs 7, 0
board           defs 42, 0

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
yellowGo        equ 1                               
redGo           equ 5
yellowWin       equ 4 * yellowGo
redWin          equ 4 * redGo

winningCombos   equ 69
winningLines
                DB 0,1,2,3
                DB 1,2,3,4
                DB 2,3,4,5
                DB 3,4,5,6
                DB 7,8,9,10
                DB 8,9,10,11
                DB 9,10,11,12
                DB 10,11,12,13
                DB 14,15,16,17
                DB 15,16,17,18
                DB 16,17,18,19
                DB 17,18,19,20
                DB 21,22,23,24
                DB 22,23,24,25
                DB 23,24,25,26
                DB 24,25,26,27
                DB 28,29,30,31
                DB 29,30,31,32
                DB 30,31,32,33
                DB 31,32,33,34
                DB 35,36,37,38
                DB 36,37,38,39
                DB 37,38,39,40
                DB 38,39,40,41

                DB 0,7,14,21
                DB 7,14,21,28
                DB 14,21,28,35
                DB 1,8,15,22
                DB 8,15,22,29
                DB 15,22,29,36
                DB 2,9,16,23
                DB 9,16,23,30
                DB 16,23,30,37
                DB 3,10,17,24
                DB 10,17,24,31
                DB 17,24,31,38
                DB 4,11,18,25
                DB 11,18,25,32
                DB 18,25,32,39
                DB 5,12,19,26
                DB 12,19,26,33
                DB 19,26,33,40
                DB 6,13,20,27
                DB 13,20,27,34
                DB 20,27,34,41

                DB 0,8,16,24
                DB 1,9,17,25
                DB 2,10,18,26
                DB 3,11,19,27
                DB 7,15,23,31
                DB 8,16,24,32
                DB 9,17,25,33
                DB 10,18,26,34
                DB 14,22,30,38
                DB 15,23,31,39
                DB 16,24,32,40
                DB 17,25,33,41

                DB 21,15,9,3
                DB 22,16,10,4
                DB 23,17,11,5
                DB 24,18,12,6
                DB 28,22,16,10
                DB 29,23,17,11
                DB 30,24,18,12
                DB 31,25,19,13
                DB 35,29,23,17
                DB 36,30,24,18
                DB 37,31,25,19
                DB 38,32,26,20


initialiseBoard:
        ld a, 0
        ld hl, columns
        ld b, 7+42
ib1:
        ld (hl), a
        inc hl
        djnz ib1
        ret

newGo:
        ld a, 36
        ld (chipX), a
        ld a, 24
        ld (chipY), a
        ld a, 1
        ld (columnSelected), a

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


checkWin:
        ld ix, winningLines
        ld c, winningCombos        
nextLine:
        xor a
        ld d, 0
        ld b, 4
        call checkLine
        ld a, d
        cp yellowWin
        jr z, winner
        cp redWin
        jr z, winner
        dec c
        jr nz, nextLine
        xor a
        ret

winner:
        scf
        ret

checkLine:        
        ld a, (ix +0)                           ; pick up board element we want
        ld hl, board                            ; set base
        add hl, a                               ; add in element offset
        ld a, d                                 ; get current line value
        add (hl)                                ; add in this element
        ld d, a                                 ; store back in d
        inc ix                                  ; next element in line
        djnz checkLine                          ; repeat for 4 elements
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
        dec a                                   ; make 0 based
        add de, a                               ; hence offset into array
        add de, board                           ; and add array base

; determine value to set in the array
        ld b, yellowGo
        ld a, (chipPattern)
        cp yellowChip
        jr z, setBoardChip
        ld b, redGo
setBoardChip
        ld a, b
        ld (de), a
        ld (lastGo), a
        call newGo        

        ret


movechipLR:
        LD A, (columnSelected)                  ; get column selected (1 - 7)
        LD D, A                                 ; calculate E = (column * 32) + 4
        LD E, 32
        MUL D, E
        ADD DE, 4
        EX DE, HL
        LD (chipX), HL
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
        LD A, 0                         ; set the sprite index
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


        LD A, 1                         ; set first RELATIVE sprite
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


        LD A, 2                         ; set second RELATIVE sprite
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


        LD A, 3                         ; set second RELATIVE sprite
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

;        HALT
        RET




