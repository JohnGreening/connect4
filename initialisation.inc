device ZXSPECTRUMNEXT

initCellWinLines
;---------------------------------------------------------------------------------------------------------------
; Purpose :   create cellWinLines table, i.e. 42 rows of between 3 to 13 winLines values
;---------------------------------------------------------------------------------------------------------------
        ld iy, cellWinLines                     ; point to this table
	    ld h, 0                                 ; H is the cell 0..41 board index (BI) to search for

l4:	
        ld ix, winLines                         ; point to the table holding what cells are applicable to each of the 69 win lines 
	    push iy                                 ; save the winPairLookup table
	    ld l, 0                                 ; L is the win line counter (WL)

l5:	
        call chkLineForB                        ; check winning line for an occurance of B

	    inc l                                   ; update winline counter
	    ld a, l                                 ; get the winline counter
	    cp 69                                   ; there are 69 (0..68)
	    jr nz, l5                               ; branch if more winlines to process for this BI

	    pop iy                                  ; this BI value complete, so restore WL/mask pointer to start for this BI
	    ld de, 14                               ; max 13 entries with a 255 end marker, so add 14
	    add iy, de                              ; do the add
	    inc h                                   ; BI = BI + 1
	    ld a, h                                 ; put in A to check
	    cp 42                                   ; are we at the end (0..41)
	    jr nz, l4                               ; branch if more to do
	    ret                                     ; otherwise we are complete
		
chkLineForB:	
; check a single winline to see if H is present in it
	    ld b, 4                                 ; each winning line has 4 values
.loop	
        ld a, (ix)                              ; get one of the winning line values
	    cp h                                    ; is it BI
        call z, foundIndex                      ; if found, update cellWinLines and continue (wastefully until end of loop)
	    inc ix                                  ; otherwise point to the next item in this winline
	    djnz .loop                              ; and repeat for all 4 values
	    ret                                     ; and return

foundIndex:	
	    ld a, l                                 ; get the win line index
	    ld (iy), a                              ; save the win line index in cellWinLines
	    inc iy                                  ; and advance IY for the next winline write
        ret
			
