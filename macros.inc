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

    macro calculateBoardIndex
    ; calculate BI (0..41)
    ; Input     :  IX +0 = column, IX +1 = chip count
    ; Output    :  A = BI (0..41) i.e. (7 * row) + column
        push de

        ld a, (ix +0)
        ld c, a
        ld a, (ix +1)
        ld b, a

        ld a, 6                                 ; default column value
        sub b                                   ; adjust for chips in the column
        ld d, a                                 ; save in D for multiplication
        ld e, 7                                 ; .. by 7
        mul d, e                                ; do the multiplication
        ld a, e                                 ; we only need the low byte for 0..41
        add c                                   ; add in column (0..6) and return BI

        pop de
    ENDM


    macro call_preserve_IX routine
        push ix

        call routine

        pop ix
    ENDM