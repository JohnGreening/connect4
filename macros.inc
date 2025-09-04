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
    ; Input     :  B = number of chips in a given column C
    ;           :  C = column (0..6)
    ; Output    :  A = BI (0..41) i.e. (7 * row) + column
        push bc
        push de

        ld a, 6                                 ; default column value
        sub b                                   ; adjust for chips in the column
        ld d, a                                 ; save in D for multiplication
        ld e, 7                                 ; .. by 7
        mul d, e                                ; do the multiplication
        ld a, e                                 ; we only need the low byte for 0..41
        add c                                   ; add in column (0..6) and return BI

     ;   cp 42
     ;   jp nc, 0

        pop de
        pop bc
    ENDM

    macro useRedColScore
    ; point IY to colScore(column)
    ; Input     :  C  = column being tested
    ; Output    :  IY = colScore(column)

        ld a, c                                 ; get column being tested
     ;   cp 7
     ;   jp nc, 0

        add a, a                                ; double since colScore values are 16 bit words
        ld iy, colScore                         ; point to base of colScore()
        ld d, 0                                 ; default D
        ld e, a                                 ; E = column * 2
        add iy, de                              ; hence IY points to colScore(column)
    ENDM

    macro call_preserve_HL_BC routine
        push hl
        push bc

        call routine

        pop bc
        pop hl
    ENDM