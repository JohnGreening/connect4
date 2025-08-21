newGame
        CALL killSprites
        CALL createLevel
        RET


createLevel
        LD HL, page000
        LD DE, tileMapData      ; HL points to the tileMap address
CTM2
        LD A, (HL)              ; get tile to display
        INC HL                  ; point to the tile count to display
        LD B, (HL)              ; put count in B
        INC HL                  ; ready for next tile
CTM1
        LD (DE), A              ; put tile on the screen
        INC DE                  ; point to next address
        DJNZ CTM1               ; loop until this tile complete
        LD A, D
        CP $65                  ; at end DE=$6500
        JR NZ, CTM2             ; loop for next tile
CTME
        RET                      ; return



endProg
        CALL killSprites

        LD A, $6b               ; ready to read tilemap control $6b
        CALL ReadNextReg        ; do the read
        AND %01111111           ; just run off bit 7 to disable tilemap
        NEXTREG $6b, A          ; do the write

        LD A, $15               ; ready to read sprites and layers system $15
        CALL ReadNextReg        ; do the read
        AND %11111110           ; just turn off bit 0 to disable sprites
        NEXTREG $15, A          ; do the write

        ; turn off sound from all 3 AY chips
        LD A, 255                
        LD BC, AYSelect
        OUT (C), A
        LD D,7
        LD BC, AYSelect
        OUT (C), D
        LD BC, AYrw
        LD A, 255
        OUT (C), A

        LD A, 254
        LD BC, AYSelect
        OUT (C), A
        LD D, 7
        LD BC, AYSelect
        OUT (C), D
        LD A, 255
        LD BC, AYrw
        OUT (C), A

        LD A, 253
        LD BC, AYSelect
        OUT (C), A
        LD D, 7
        LD BC, AYSelect
        OUT (C), A
        LD A, 255
        LD BC, AYrw
        OUT (C), A

        CALL ULAon
        RET

killSprites
        LD A, 0                             ; start at sprite 0
        LD B, 127                           ; loop for all 127 sprites
KS1
        NEXTREG $34, A
        NEXTREG $35, 0
        NEXTREG $36, 0
        NEXTREG $37, 0
        NEXTREG $38, 0
        NEXTREG $39, 0

        INC A
        DJNZ KS1
        RET


