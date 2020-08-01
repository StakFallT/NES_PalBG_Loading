; Routines contained in file:
;   Inc_Sprite1_Tile
;   Dec_Sprite1_Tile
;   DisplaySprite




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Inc_Sprite1_Tile                                        _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Inc_Sprite1_Tile:
    lda Sprite1_Tile
    adc #1
    sta Sprite1_Tile
    rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Inc_Sprite1_Tile                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/








; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Dec_Sprite1_Tile                                        _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Dec_Sprite1_Tile:
    lda Sprite1_Tile
    sbc #1
    sta Sprite1_Tile
    rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Dec of Inc_Sprite1_Tile                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/






; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            DisplaySprite                                           _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
DisplaySprite:
    sta Unsafe_Var1
    ; Since this function was called using the stack for parameters, the return address
    ; first needs to be pulled off the stack to get to those parameters. However,
    ; the return address needs to be preserved for the end of the routine, when
    ; rts is called.
	    pla
	    sta PC_A
	    pla
	    sta PC_B
    lda Unsafe_Var1

    pha
	    lda #$00            ; these lines tell $2003
		sta $2003           ; to tell
		lda SpriteCount     ; $2004 to start
		sta $2003           ; at $0000.
    pla

    ; Every sprite has 4 bytes of information. They are:
    ; Y - The vertical position on the screen
    ; N - the 8x8 tile number in the .spr file that was included
    ; C - 
    ; X - The horizontal position on the screen
    ; Note: As per: https://patater.com/nes-asm-tutorials/day-5/ there can be
    ;       64 sprites' info stored here
;		lda #50  	; load Y value
;		sta $2004 	; store Y value

;		lda SpriteNumber 	; tile number 0
;		sta $2004 	; store tile number

;		lda #$00 	; no special junk
;		sta $2004 	; store special junk

;		lda #20  	; load X value
;		sta $2004 	; store X value

		; and yes, it MUST go in that order.

        stx Unsafe_Var1
        ldx #00

        DS_4Params:
            pla
            sta $2004

            inx
            cpx #04
            beq DS_Done
            jmp DS_4Params

        DS_Done:
            ldx Unsafe_Var1

            sta Unsafe_Var1
            ;Restore the return address back onto the stack
	            lda PC_B
	            pha
	            lda PC_A
	            pha
            lda Unsafe_Var1

            rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of DisplaySprite                                    _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/