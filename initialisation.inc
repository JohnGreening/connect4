inkBit EQU 7
paperBit EQU 0

; this needs to be 4096 - (number of chars * 32)
mychars EQU TilePatterns + 2752

convertChars:
             LD IX, mychars
             LD DE, charSet
convertChars1:
             LD A, (DE)
             CP 255
             RET Z
             CALL convertChar
             INC DE
             JR convertChars1
             RET

convertChar:
             PUSH DE
             SUB 32
             LD D, A
             LD E, 8
             MUL D, E
             LD IY, 15616
             ADD IY, DE
             LD C, 8
cc0:
             LD B, 4
             LD A, (IY)
cc1:
             LD H, 0
             SLA A
             CALL setBit
             SLA A
             CALL setBit
             LD (IX), H
             INC IX
             DJNZ cc1
             INC IY
             DEC C
             JR NZ, cc0
             POP DE
             RET

setBit
             LD L, A
             JR C, bitSet
             JR bitNotSet
sb1          LD A, L
             RET
bitSet
             SLA H
             SLA H
             SLA H
             SLA H
             LD A, H
             OR inkBit
             LD H, A
             JR sb1
bitNotSet
             SLA H
             SLA H
             SLA H
             SLA H
             LD A, H
             OR paperBit
             LD H, A
             JR sb1

