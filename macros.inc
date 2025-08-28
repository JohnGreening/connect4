    macro displayText textlabel
        ld iy, textlabel
        call DispT
    ENDM

    macro playSoundEffect soundlabel
        ld a, soundlabel
        call playsound
    ENDM