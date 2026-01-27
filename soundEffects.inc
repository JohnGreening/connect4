playsound:
; Input : A = sound fx to play
;
; routine will choose next available channel and copy sound fx to that channel
; IM2 will then process that data
        RET
        PUSH HL
        PUSH DE
        PUSH BC

        DEC A
        ADD A, A
        ADD A, A
        ADD A, A
        ADD A, A

        LD HL, sound1
        ADD HL, A

        LD A, (nextCH)                      ; whats the nextCH to use
        CP 0                                ; is it 0?
        JR Z, playCh1                       ; jump if so
        CP 1                                ; is it 1?
        JR Z, playCh2                       ; jump if so
        LD A, 255                           ; its 2, set next so its 0
        LD DE, Ch3                          ; set AY channel to 3
        JR play                             ; and branch to do the sound setup
playCh1:
        LD DE, Ch1                          ; set the AY channel to 1
        JR play                             ; and branch to do the sound setup
playCh2:
        LD DE, Ch2                          ; set the AY channel to 2
play:
        INC A                               ; roll on nextCH
        LD (nextCH), A                      ; and save it
        LD BC, 10                           ; we copy 10 bytes of sound setup
        LDIR                                ; do the copy

        POP BC
        POP DE
        POP HL
        RET

sound1:
; initial tone period
; tone period delta
; initial volme (*256)
; volume delta
; tone, noise, envelope flags
; length in frames (50hz)
; filler to make each 16 bytes long

; dropping chip sound
        DW 16000
        DW 100
        DW 15 * 256
        DW 20
        DB %00000001
        DB 15
        DB 0,0,0,0,0,0

; move left/right sound
sound2:
        DW 212
        DW -6
        DW 14*256
        DW -10
        DB %0000001
        DB 4
        DB 0,0,0,0,0,0

; tada 1
sound3:
        DW 212
        DW -6
        DW 15*256
        DW -10
        DB 1
        DB 4
        DB 0,0,0,0,0,0
; tada 2
sound4:
        DW 168
        DW -4
        DW 15*256
        DW -10
        DB 1
        DB 4
        DB 0,0,0,0,0,0

; tada 3
sound5:
        DW 141
        DW -1
        DW 15*256
        DW -10
        DB 1
        DB 12
        DB 0,0,0,0,0,0

; kill skull sound
sound6:
        DW 500
        DW 10
        DW 15*256
        DW 20
        DB %11111111
        DB 100
        DB 0,0,0,0,0,0

nextCH:      DB 0

Ch1:
Ch1Pitch:    DW 0
Ch1PitchD:   DW 0
Ch1Volume    DW 00
Ch1VolumeD:  DW 00
Ch1Flags:    DB 0
Ch1Length:   DB 0

Ch2:
Ch2Pitch:    DW 00
Ch2PitchD:   DW 00
Ch2Volume    DW 00
Ch2VolumeD:  DW 00
Ch2Flags:    DB 0
Ch2Length:   DB 0

Ch3:
Ch3Pitch:    DW 00
Ch3PitchD:   DW 00
Ch3Volume    DW 00
Ch3VolumeD:  DW 00
Ch3Flags:    DB 0
Ch3Length:   DB 0

soundDrop      EQU 1
soundKey       EQU 2
soundTada1     EQU 3
soundTada2     EQU 4
soundTada3     EQU 5
soundKillskull EQU 6
