
; NEXTREG is used to write to Next/TBBlue feature control registers
; but there isn't a equivalent instruction to do a read
; instead 
;    we write to port $243B to select the register to write to
;    and then read the value back from port $253B
ReadNextReg
        PUSH BC
        LD BC, $243b
        OUT (C), A
        INC B
        IN A, (C)
        POP BC
        RET

ULAoff
        LD A, $68
        CALL ReadNextReg
        OR %10000000
        NEXTREG $68, A
        RET
ULAon
        LD A, $68
        CALL ReadNextReg
        AND %01111111
        NEXTREG $68, A
        RET

tileMapOnTop
        NEXTREG $15, %00010111
        ; bit 7 1=enable lo-res 
        ; bit 6 1=flip sprite rendering priority
        ; bit 5 1=change clipping over border mode
        ; bit 4-2 000 = S L U   Sprites over Layer 2 over ULA
        ;         001 = L S U
        ;         010 = S U L
        ;         011 = L U S
        ;         100 = U S L
        ;         101 = U L S
        ;         110 = S(U+L)
        ;         111 = S(U+L -5)
        ; bit 1 1=enable sprites over border
        ; bit 0 1=enable sprite visibility
        ret
spritesOnTop
        NEXTREG $15, %00000011
        ret

keyPause
        ld b, keyPauseLength
kp1
        halt
        djnz kp1
        ret

random1
        PUSH HL
        PUSH DE
        PUSH BC
        LD HL, LFSRSeed +4
        LD E, (HL)
        INC HL
        LD D, (HL)
        INC HL
        LD C, (HL)
        INC HL
        LD A, (HL)
        LD B, A
        RL E
        RL D
        RL C
        RL A
        RL E
        RL D
        RL C
        RL A
        RL E
        RL D
        RL C
        RL A
        LD H, A
        RL E
        RL D
        RL C
        RL A
        XOR B
        RL E
        RL D
        XOR H
        XOR C
        XOR D
        LD HL, LFSRSeed +6
        LD DE, LFSRSeed +7
        LD BC, 7
        LDDR 
        LD (DE), A
        POP BC
        POP DE
        POP HL
        RET

random
        LD A, (rseed)
        LD D, A
        RRCA 
        RRCA 
        RRCA 
        XOR $1f
        ADD A, D
        SBC A, 255
        LD (rseed),A
        RET

randomY
        CALL random1
        AND %00001111
        LD HL, rndY
        ADD HL, A
        LD A, (HL)
        RET
randomX
        CALL random1
        AND %00001111
        ADD A, A
        LD HL, rndX
        ADD HL, A
        LD A, (HL)
        LD D, A
        INC HL
        LD A, (HL)
        LD H, A
        LD L, D
        RET

keyC:
        push bc
        push af
        LD BC, $fefe                        ; port for key
.loop
        IN A, (C)                           ; read port
        bit 3, a
        jr nz, .loop

        pop af
        pop bc
        ret

keyV:
        push af
        push bc
        push de
        push hl
        push ix
        push iy
        LD BC, $fefe                        ; port for key
.loop:
        IN A, (C)                           ; read port
        bit 4, a
        jr nz, .loop
.loop1:
        in a, (c)
        bit 4, a
        jr z, .loop1
        pop iy
        pop ix
        pop hl
        pop de
        pop bc
        pop af
        ret

LFSRSeed DB $12, $34, $56, $78, $9a, $bc, $de, $f0
rseed    DB 1
rndY     DB 24,32,48,64,80,96,104,120,136,152,168,176,192,208,224,240
rndX     DW 0,16,40,64,80,104,128,144,168,192,208,232,256,272,296,320
