    macro readKey port, mask
        ld bc, port
        in a, (c)
        and mask
    ENDM


    macro displayText textlabel
        push iy
        ld iy, textlabel
        call DispT
        pop iy
    ENDM

    macro playSoundEffect soundlabel
        ld a, soundlabel
        call playsound
    ENDM



    macro call_preserve_IX routine
        push ix

        call routine

        pop ix
    ENDM