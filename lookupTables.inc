spritePixelX    db 36, 68, 100, 132, 164, 196, 228
spritePixelY    db 24, 60, 92, 124, 156, 188, 220

; there are 69 win lines in total for a 7 x 6 board
winningCombos   equ 69

; this table is 69 rows and details which cells (0..41) are in that win line
winLines
                DB 0,1,2,3                      ; win line 0
                DB 1,2,3,4                      ; win line 1
                DB 2,3,4,5                      ; win line 2
                DB 3,4,5,6                      ; win line 3
                DB 7,8,9,10                     ; win line 4
                DB 8,9,10,11                    ; win line 5
                DB 9,10,11,12                   ; win line 6
                DB 10,11,12,13                  ; win line 7
                DB 14,15,16,17                  ; win line 8
                DB 15,16,17,18                  ; win line 9
                DB 16,17,18,19                  ; win line 10
                DB 17,18,19,20                  ; win line 11
                DB 21,22,23,24                  ; win line 12
                DB 22,23,24,25                  ; win line 13
                DB 23,24,25,26                  ; win line 14
                DB 24,25,26,27                  ; win line 15
                DB 28,29,30,31                  ; win line 16
                DB 29,30,31,32                  ; win line 17
                DB 30,31,32,33                  ; win line 18
                DB 31,32,33,34                  ; win line 19
                DB 35,36,37,38                  ; win line 20
                DB 36,37,38,39                  ; win line 21
                DB 37,38,39,40                  ; win line 22
                DB 38,39,40,41                  ; win line 23

                DB 0,7,14,21                    ; win line 24
                DB 7,14,21,28                   ; win line 25
                DB 14,21,28,35                  ; win line 26
                DB 1,8,15,22                    ; win line 27
                DB 8,15,22,29                   ; win line 28
                DB 15,22,29,36                  ; win line 29
                DB 2,9,16,23                    ; win line 30
                DB 9,16,23,30                   ; win line 31
                DB 16,23,30,37                  ; win line 32
                DB 3,10,17,24                   ; win line 33
                DB 10,17,24,31                  ; win line 34
                DB 17,24,31,38                  ; win line 35
                DB 4,11,18,25                   ; win line 36
                DB 11,18,25,32                  ; win line 37
                DB 18,25,32,39                  ; win line 38
                DB 5,12,19,26                   ; win line 39
                DB 12,19,26,33                  ; win line 40
                DB 19,26,33,40                  ; win line 41
                DB 6,13,20,27                   ; win line 42
                DB 13,20,27,34                  ; win line 43
                DB 20,27,34,41                  ; win line 44

                DB 0,8,16,24                    ; win line 45
                DB 1,9,17,25                    ; win line 46
                DB 2,10,18,26                   ; win line 47
                DB 3,11,19,27                   ; win line 48
                DB 7,15,23,31                   ; win line 49
                DB 8,16,24,32                   ; win line 50
                DB 9,17,25,33                   ; win line 51
                DB 10,18,26,34                  ; win line 52
                DB 14,22,30,38                  ; win line 53
                DB 15,23,31,39                  ; win line 54
                DB 16,24,32,40                  ; win line 55
                DB 17,25,33,41                  ; win line 56

                DB 21,15,9,3                    ; win line 57
                DB 22,16,10,4                   ; win line 58
                DB 23,17,11,5                   ; win line 59
                DB 24,18,12,6                   ; win line 60
                DB 28,22,16,10                  ; win line 61
                DB 29,23,17,11                  ; win line 62
                DB 30,24,18,12                  ; win line 63
                DB 31,25,19,13                  ; win line 64
                DB 35,29,23,17                  ; win line 65
                DB 36,30,24,18                  ; win line 66
                DB 37,31,25,19                  ; win line 67
                DB 38,32,26,20                  ; win line 68

cellWinLines
; table holding, for each cell (0..41) which winLines it is in
; a given cell can be in between 3 and 13 win lines, we terminate the row with one or more 255 markers
    defs 42 * 14, 0
