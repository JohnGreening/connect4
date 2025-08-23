counterX defw 36
counterY defw 24
counterPattern db 0
columnSelected db 1
rowSelected db 0
columns defs 7, 0
board defs 42, 0


initialiseBoard
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
        ld (counterX), a
        ld a, 24
        ld (counterY), a
        ld a, 1
        ld (columnSelected), a
        ret

displayCounter:
        LD A, 0                         ; set the sprite index
        NEXTREG $34, A                  ; set sprite to activate

        LD HL, (counterX)
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
        
        LD HL, (counterY)
        LD A, L                         ; get sprite Y lsb
        NEXTREG $36, A                  ; set attr byte 1 of port $0057

        LD A, (counterPattern)          ; get pattern index to use
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




