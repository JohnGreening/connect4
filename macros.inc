    macro displayText textlabel
        ld iy, textlabel
        call DispT
    ENDM

    macro playSoundEffect soundlabel
        ld a, soundlabel
        call playsound
    ENDM

    macro calculateBoardIndex
    ; calculate BI (0..41)
    ; Input     :  B = number of chips in a given column C
    ;           :  C = column (0..6)
    ; Output    :  A = BI (0..41) i.e. (7 * row) + column
        ld a, 6                                 ; default column value
        sub b                                   ; adjust for chips in the column
        ld d, a                                 ; save in D for multiplication
        ld e, 7                                 ; .. by 7
        mul d, e                                ; do the multiplication
        ld a, e                                 ; we only need the low byte for 0..41
        add c                                   ; add in column (0..6) and return BI
    ENDM
