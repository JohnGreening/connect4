spriteIndex     db 0                            ; the sprite index, normally 0, but can be >0 when winner displayed
chipX           defw 36                         ; X coord of sprite to be displayed
chipY           defw 24                         ; Y coord of sprite to be displayed
chipPattern     db 4                            ; the sprite pattern to be used (yellowChip or redChip)

columnSelected  db 1                            ; holds last column 0 - 6
rowSelected     db 0                            ; holds row used    1 - 6 (0 when selecting column)
whoseGo         db 0                            ; last board value used 1 or 5
startGo         db 0                            ; the player who will start next game first
winLine         db 0                            ; holds last win line where detected or 255


; the connect 4 board data
columns         db   0, 0                       ; column index, chip count
                db   1, 0
                db   2, 0
                db   3, 0
                db   4, 0
                db   5, 0
                db   6, 0
                db   255, 0                     ; this is the quit row
lineScore       defs 69, 0                      ; for each win line, a score (each yellow +1, each red -1, +4/-4 = win)
lineCount       defs 69, 0                      ; for each win line, a tally of chips in that line
boardEnd        db 0
boardStateLen   equ boardEnd - columns
columnNo        equ 0                           ; IX +columnNo  = column number 0-6
columnCnt       equ 1                           ; IX +columnCnt = count of counters in that column 0-6

; sprite patterns
yellowChip      equ 0                           ; sprites 0 - 3
redChip         equ 4                           ; sprites 4 - 7

; tile patterns
yellowTile      equ 1                           ; tiles 1 - 9
redTile         equ 24                          ; tiles 24 - 32

; board array values
yellowGo        equ yellowChip +1
redGo           equ redChip +1

INCLUDE "lookupTables.inc"


AYSelect       EQU $FFFD
AYrw           EQU $bffd
AYPortC        EQU $FD      ; Port low byte, stays in C
AYPortSelectB  EQU $FF      ; B value for AYSelect port ($FFFD)
AYPortWriteB   EQU $BF      ; B value for AYrw port ($BFFD)
channel1ToneL  equ 0
channel1ToneH  equ 1
channel2ToneL  equ 2
channel2ToneH  equ 3
channel3ToneL  equ 4
channel3ToneH  equ 5
channel1Volume equ 8
channel2Volume equ 9
channel3Volume equ 10
mixerFlag      equ 7

keyPauseLength equ 10


mixerFlags  
    db %11111111, %11111110, %11110111, %11110110
    db %11111111, %11111101, %11101111, %11101101
    db %11111111, %11111011, %11011111, %11011011
    
