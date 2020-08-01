; Routines contained in file:
;   Set_PPU_PtrnAndNT_TableAddrLayout
;   Set_PPU_DisplayProperties
;   Set_PPU_OAM




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Set_PPU_PtrnAndNT_TableAddrLayout                       _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Set_PPU_PtrnAndNT_TableAddrLayout:
    ; Set the first PPU memory-mapped registers flags
	    ;  0    - non-maskable interrupts are NOT enabled on VBlank
	    ;  0    - PPU selection (flag not used)
	    ;  0    - The sprite size is 8x8
	    ;  0    - The bkg pattern table address is $0000 (VRAM)
	    ;  1    - The spr pattern table address is $1000 (VRAM)
	    ;  0    - Increment PPU addresses by 1
	    ; 00    - The name table address is $2000 (VRAM)
			lda #%10001000
			sta $2000
			rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Set_PPU_PtrnAndNT_TableAddrLayout                _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/








; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Set_PPU_DisplayProperties                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Set_PPU_DisplayProperties:
    ; Now set the second PPU memory-mapped registers flags
        ; 000   - The bkg color defaults to black
        ;   1   - Enable the displaying of sprites
        ;   1   - Enable the displaying of backgrounds
        ;   1   - Disable sprite clipping
        ;   1   - Disable background clipping
        ;   0   - Set the display type to color
			lda #%00011110 	; talked about
			sta $2001 		; on a previous day
            rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Set_PPU_DisplayProperties                        _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/







; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Set_PPU_BGPal_Addr                                      _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Set_PPU_BGPal_Addr:
    ; As per: https://nerdy-nights.nes.science/#main_tutorial-4
    lda $2002


    ; From https://patater.com/nes-asm-tutorials/day-4/
    ; "These 4 lines tell $2006 that we want the stuff we load $2007 with
    ;  to start at memory location $3F00. [...] since we can only store
    ;  a byte at a time, we store twice"
		lda #$3F    ; have $2006 tell
		sta $2006   ; $2007 to start
		;lda #$00    ; at $3F00 (where the palette will load to).
		lda #$00    ; at $3F00 (where the palette will load to).
		sta $2006
	; NOTE: When memory-mapped registers, that come in a pair, are written to in
	;       successive calls, the final ending detination address that is written to
	;       by the register is automatically incremented upon each write operation
	;       to the register!
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Set_PPU_BGPal_Addr                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/





; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Set_PPU_Pal_StartAddr                                   _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Set_PPU_Pal_StartAddr:
    ; As per: https://nerdy-nights.nes.science/#main_tutorial-4
    lda $2002   ; read PPU status to reset the high/low latch   -- uhm, ok...?


    ; From https://patater.com/nes-asm-tutorials/day-4/
    ; "These 4 lines tell $2006 that we want the stuff we load $2007 with
    ;  to start at memory location $3F00. [...] since we can only store
    ;  a byte at a time, we store twice"
		lda #$3F    ; have $2006 tell
		sta $2006   ; $2007 to start
		;lda #$00    ; at $3F00 (where the palette will load to).
		lda #$00    ; at $3F00 (where the palette will load to).
		sta $2006
	; NOTE: When memory-mapped registers, that come in a pair, are written to in
	;       successive calls, the final ending detination address that is written to
	;       by the register is automatically incremented upon each write operation
	;       to the register!
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Set_PPU_Pal_StartAddr                            _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/






; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Set_PPU_SprPal_Addr                                     _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Set_PPU_SprPal_Addr:
    ; As per: https://nerdy-nights.nes.science/#main_tutorial-4
    lda $2002   ; read PPU status to reset the high/low latch   -- uhm, ok...?


    ; From https://patater.com/nes-asm-tutorials/day-4/
    ; "These 4 lines tell $2006 that we want the stuff we load $2007 with
    ;  to start at memory location $3F00. [...] since we can only store
    ;  a byte at a time, we store twice"
		lda #$3F    ; have $2006 tell
		sta $2006   ; $2007 to start
		;lda #$00    ; at $3F00 (where the palette will load to).
		lda #$10    ; at $3F00 (where the palette will load to).
		sta $2006
	; NOTE: When memory-mapped registers, that come in a pair, are written to in
	;       successive calls, the final ending detination address that is written to
	;       by the register is automatically incremented upon each write operation
	;       to the register!
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Set_PPU_SprPal_Addr                              _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/