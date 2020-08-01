; Routines contained in file:
;   loadpal
;   loadmap




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            loadpal                                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
loadpal:
    ; x should already be 0 (at start). Loop 32 times, incrementing each
    ; iteration while telling $2007 that value (pre-increment)
    ; that value in order to assign numbers to each of the colors in
    ; the palette that was loaded
		lda tilepal, x 	; load tilepal+($x) into register A
		sta $2007 		; Store the value of register A in $2007 which will,
		inx 			; a number to each color of the palette.
		cpx #32 		; Has there been 32 (16 colors for sprs and 16 colors for bkgs) iterations?
		bne loadpal 	; if X isn’t =32, goto “loadpal:” line.
        ;jmp waitblank
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of loadpal                                          _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/






; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            loadpal                                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
loadbgpal:
    ; x should already be 0 (at start). Loop 32 times, incrementing each
    ; iteration while telling $2007 that value (pre-increment)
    ; that value in order to assign numbers to each of the colors in
    ; the palette that was loaded
		lda tilepal, x 	; load tilepal+($x) into register A
		sta $2007 		; Store the value of register A in $2007 which will,
		inx 			; a number to each color of the palette.
		cpx #16 		; Has there been 32 (16 colors for sprs and 16 colors for bkgs) iterations?
		bne loadpal 	; if X isn’t =32, goto “loadpal:” line.
        ;jmp waitblank
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of loadpal                                          _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/







; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            loadpal                                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
loadsprpal:
    ; x should already be 0 (at start). Loop 32 times, incrementing each
    ; iteration while telling $2007 that value (pre-increment)
    ; that value in order to assign numbers to each of the colors in
    ; the palette that was loaded
		lda tilepal, x 	; load tilepal+($x) into register A
		sta $2007 		; Store the value of register A in $2007 which will,
		inx 			; a number to each color of the palette.
		cpx #16 		; Has there been 32 (16 colors for sprs and 16 colors for bkgs) iterations?
		bne loadpal 	; if X isn’t =32, goto “loadpal:” line.
        ;jmp waitblank
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of loadpal                                          _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/







SunsetPalette_ShiftCheck:
    lda Sunset_DelayCounter
    adc #01
    cmp Sunset_Delay
    ;cmp #04
    sta Sunset_DelayCounter
    beq SunsetPalette_PreShift
    rts

SunsetPalette_PreShift:
    inc Sunset_PaletteNumber
    lda Sunset_PaletteNumber
    cmp #04
    beq Reset_SunsetPaletteNumber

    Sunset_Palette_PerformShift:
        ;ldx #13     ; Set up the X register for index-addressing
        ;lda , X 

        ; First range (set) of attribute bytes to be adjusted
            lda $2002

		    lda #$23
		    sta $2006
		
		    lda #$D3
		    sta $2006
		
		    ;ldx #$00
		    ldx Sunset_PaletteNumber
            lda PalCycle_0, X
    
	        ;adc #$55                        ; 0x55 = 0101 0101   -- this -should- have the effect of increasing each quadrant in the block by 1
	        ; For each block (4 tiles across by 4 tiles down), there is a single attribute address (since a single attribute byte assigns
	        ; the palette to each quadrant in a tileset). If there are two addresses, there should be two bytes, so forth and so on...
		        sta $2007
		        sta $2007
		        sta $2007
		        sta $2007
		        
		        sta $2007
		        sta $2007
		        sta $2007
		        sta $2007



        ; Second range (set) of attribute bytes to be adjusted
	        lda $2002
	
		    lda #$23
		    sta $2006
		
		    lda #$DB
		    sta $2006
		
		    ldx #$00

            ldx Sunset_PaletteNumber
            lda PalCycle_0, X
            ;adc #$55                        ; 0x55 = 0101 0101   -- this -should- have the effect of increasing each quadrant in the block by 1
            ; Again, for each block (4 tiles across by 4 tiles down), there is a single attribute address (since a single attribute byte assigns
	        ; the palette to each quadrant in a tileset). If there are two addresses, there should be two bytes, so forth and so on...
		        sta $2007
		        sta $2007
		        sta $2007
		        sta $2007

                sta $2007
		        sta $2007
		        sta $2007
		        sta $2007

        lda #00
        sta Sunset_DelayCounter

        rts

    Reset_SunsetPaletteNumber:
        lda #00
        sta Sunset_PaletteNumber

        jmp Sunset_Palette_PerformShift


