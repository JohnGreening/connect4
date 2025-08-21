
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

mixerFlags  
    db %11111111, %11111110, %11110111, %11110110
    db %11111111, %11111101, %11101111, %11101101
    db %11111111, %11111011, %11011111, %11011011
    
