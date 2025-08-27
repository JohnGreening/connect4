
DispA
; pass IY with location to display A
; routine will display A (retained)
        PUSH HL
        PUSH DE
        PUSH BC
        PUSH AF
        PUSH IX
        LD H, 0
        LD L, A
        CALL DispA3Digit
        POP IX
        POP AF
        POP BC
        POP DE
        POP HL
        RET
DispA1
        PUSH AF
        LD H, 0
        LD L, A
        CALL DispA1Digit
        POP AF
        RET
DispA2
        PUSH AF
        LD H, 0
        LD L, A
        CALL DispA2Digit
        POP AF
        RET

DispIX
; pass IY with location to display IX
; routine will display IX (retained)
        PUSH HL
        PUSH BC
        PUSH AF
        PUSH IX

        PUSH IX
        POP HL
        CALL DispHL1
        POP IX
        POP AF
        POP BC
        POP HL
        RET

DispHL
; pass IY with location to display HL
; routine will display HL (retained)
        PUSH HL
        PUSH BC
        PUSH AF
        CALL DispHL1
        POP AF
        POP BC
        POP HL
        RET

DispHL1
             LD BC, -10000
             CALL num1
             LD BC, -1000
             CALL num1
DispA3Digit                                 ; use this for 3 digits
             LD BC, -100
             CALL num1
DispA2Digit
             LD BC, -10
             CALL num1
DispA1Digit                                 ; use this for 1 digit
             LD BC, -1
num1
;            114 is the tilemap for 0
             LD A, digitTileStart -1
num2
             INC A
             ADD HL, BC
             JR C, num2
             SBC HL, BC

             LD (IY), A
             INC IY
             RET

DispT
; pass IY pointing to text to display
; format is Y, X, "text", $FF
; cutdown charset, as below is used
             LD A, (IY)                     ; get the Y line
             LD D, A                        ; put in D
             LD E, 40                       ; get ready to multiply by 40 (chars per line)
             MUL D,E                        ; do it
             INC IY                         ; point to X row
             LD A, (IY)                     ; get it
             ADD DE, A                      ; add to result
             LD HL, tileMapData             ; tilemap
             ADD HL, DE                     ; point to char position
DispT1
             INC IY                         ; point to character to print
             LD A, (IY)                     ; get it
             CP $ff                         ; are we finished
             RET Z                          ; exit if so
DispTAlpha
             CP " "
             JR Z, charSpace
             CP 128
             JR NC, showTile
             LD C, A
             LD DE, charSet
             LD B, 0
searchChar
             LD A, (DE)
             CP 255
             JR Z, charNotFound
             CP C
             JR Z, charFound
             INC DE
             INC B
             JR searchChar

charSpace
             LD A, 0
             JR DispT2
showTile
             SUB 128
             JR DispT2
charNotFound
             LD A, lastTile
             JR DispT2
charFound
             LD A, B
             ADD textTileStart
DispT2
             LD (HL), A                     ; put tile A in HL
             INC HL                         ; point to next tile
             JR DispT1                      ; and repeat


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

inkBit EQU 4
paperBit EQU 0

; this needs to point to the first "text" tile
mychars EQU TilePatterns + (textTileStart * 32)


textTileStart EQU 33
digitTileStart equ charEnd + textTileStart - charSet
lastTile  EQU 127

charSet
DB "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"
DB "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"
DB "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N"
DB "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"
DB ".", ",", "'", "!", ":", "?"

charEnd
DB "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"

DB 255

txtTitle        db 0, 4, "C O N N E C T  4", 255
txtInstruction1 db 1, 4, "O: LEFT   P: RIGHT   space: DROP", 255
