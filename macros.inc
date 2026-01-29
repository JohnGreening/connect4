    macro readKey port, mask
    ; BIT        0   1     2   3   4
    ; &FEFE [shift] [Z]   [X] [C] [V]
    ; &FDFE     [A] [S]   [D] [F] [G]
    ; &FBFE     [Q] [W]   [W] [R] [T]
    ; &F7FE     [1] [2]   [3] [4] [5]
    ; &EFFE     [0] [9]   [8] [7] [6]
    ; &DFFE     [P] [O]   [I] [U] [Y]
    ; &BFFE [ENTER] [L]   [K] [J] [H]
    ; &7FFE [SPACE] [SYM] [M] [N] [B]

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