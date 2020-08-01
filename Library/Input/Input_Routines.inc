; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Strobe_Pad1                                             _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Strobe_Pad1:
    pha

    lda #$01
    sta $4016

    lda #$00
    sta $4016

    pla
    rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Strobe_Pad1                                      _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Pad1_InputDetection                                     _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Pad1_InputDetection:
    pha

    jsr Strobe_Pad1

    lda $4016
    sta Pad1_A

    lda $4016
    sta Pad1_B

    lda $4016
    sta Pad1_SELECT

    lda $4016
    sta Pad1_START

    lda $4016
    sta Pad1_UP

    lda $4016
    sta Pad1_DOWN

    lda $4016
    sta Pad1_LEFT

    lda $4016
    sta Pad1_RIGHT

    pla
    rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Pad1_InputDetection                              _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Pad1_InputLoop                                          _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
Pad1_InputLoop:
    pha


    ; First, clamp the input polling rate by checking that the delay has been met
	    sta Unsafe_Var1
	        lda Input_Delay0    ; Read delay counter into register X
	        adc #$01
	        sta Input_Delay0    ; Store newly incremented value of delay back in the Input_Delay0 variable

	        ;cmp #$04         ; 1E = 30 in decimal, 78 = 120 in decimal
	        cmp FrameCount_InputDelay

    beq Pad1_BeginInputChecks
    jmp Pad1_InputCheck_Done



    Pad1_BeginInputChecks:
        ; Reset the input delay back to 0
		    stx Unsafe_Var1
		        ldx #$00
		        stx Input_Delay0
		    ldx Unsafe_Var1

	; Up check
		Pad1_UpCheck:	
		    ; The delay has been met. As a result, begin to poll the controller
			    lda Pad1_UP
			    and #1
		        cmp #1
			    bne Pad1_DownCheck        
	
	        ; Instead of incrementing the sprite tile, let's actually make the up and down move the sprite up and down instead now
			    ;Add1_SprNumber:
			    ;    jsr Inc_Sprite1_Tile

            Move_SpriteUp:
			        ; Clear the negative flag (indirect way since there is no instruction to clear the negative flag specifically)
			        ;    lda #0
	
				    lda Sprite1_Y
				    cmp #0                          ; Avoid using BMI to check for negative values since the negative flag seems to ALWAYS be set for some reason!
	
				    beq Move_SprUp_WrapY
			
					sbc #1
				    sta Sprite1_Y

	                    lda Sprite1_Y               ; Set the 1st sprite's y-coord value to Sprite1_Y
	                    pha
	                    lda #1                      ; Updating the 1st sprite
	                    pha
                    jsr Update_OAM_SpriteY

				    jmp Pad1_DownCheck
	
				Move_SprUp_WrapY:
					    ;lda #$FE
					    lda #235
					    sta Sprite1_Y




	; Down check
		Pad1_DownCheck:
			    lda Pad1_DOWN
			    and #1
		        cmp #1
			    bne Pad1_LeftCheck
	
	        ; Instead of incrementing the sprite tile, let's actually make the up and down move the sprite up and down instead now
			    ;Dec1_SprNumber:
			    ;	    jsr Dec_Sprite1_Tile

            Move_SpriteDown:
			        ; Clear the negative flag (indirect way since there is no instruction to clear the negative flag specifically)
			        ;    lda #0
	
				    lda Sprite1_Y
				    cmp #235                          ; Avoid using BMI to check for negative values since the negative flag seems to ALWAYS be set for some reason!
	
				    beq Move_SprDown_WrapY
			
					adc #1
				    sta Sprite1_Y

	                    lda Sprite1_Y               ; Set the 1st sprite's y-coord value to Sprite1_Y
	                    pha
	                    lda #1                      ; Updating the 1st sprite
	                    pha
                    jsr Update_OAM_SpriteY

				    jmp Pad1_LeftCheck
	
				Move_SprDown_WrapY:
					    ;lda #$FE
					    lda #0
					    sta Sprite1_Y



	; Left check
		Pad1_LeftCheck:
			    lda Pad1_LEFT
			    and #1
		        cmp #1
			    bne Pad1_RightCheck
	
			Move_SpriteLeft:
			        ; Clear the negative flag (indirect way since there is no instruction to clear the negative flag specifically)
			        ;    lda #0
	
				    lda Sprite1_X
				    cmp #0                          ; Avoid using BMI to check for negative values since the negative flag seems to ALWAYS be set for some reason!
	
				    beq Move_SprLeft_WrapX
			
					sbc #1
				    sta Sprite1_X

	                    lda Sprite1_X               ; Set the 1st sprite's x-coord value to Sprite1_X
	                    pha
	                    lda #1                      ; Updating the 1st sprite
	                    pha
                    jsr Update_OAM_SpriteX

				    jmp Pad1_RightCheck
	
				Move_SprLeft_WrapX:
					    ;lda #$FE
					    lda #254
					    sta Sprite1_X





	; Right check
		Pad1_RightCheck:
			    lda Pad1_RIGHT
			    and #1
		        cmp #1
			    bne Pad1_InputCheck_Done
	
			Move_SpriteRight:
				    lda Sprite1_X
					    adc #1
					    cmp #254
					    beq Move_SprRight_WrapX
				    sta Sprite1_X

	                    lda Sprite1_X               ; Set the 1st sprite's x-coord value to Sprite1_X
	                    pha
                        lda #1                      ; Updating the 1st sprite
	                    pha
                    jsr Update_OAM_SpriteX

				    jmp Pad1_InputCheck_Done
	
				Move_SprRight_WrapX:
					    lda #0
					    sta Sprite1_X




; Pad1 Input checking done...
Pad1_InputCheck_Done:
	    pla
	    rts ; Despite being a label the code-flow "fell" into, the initial call to Pad1_InputLoop
	        ; should have been initiated via a jsr instruction. As such, an rts will be required
	        ; here
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Pad1_InputLoop                                   _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/





; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Display_Pad1_Input                                      _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

Display_Pad1_Input:
    pha

	DP1I_UpCheck:
	    lda Pad1_UP
	    and #1
	    cmp #1
	    beq DP1I_UpShow
	    bne DP1I_DownCheck
	
		DP1I_UpShow:
			lda Pad_UpSpr  	    ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_DownCheck:
	    lda Pad1_DOWN
	    and #1
	    cmp #1
	    beq DP1I_DownShow
	    bne DP1I_LeftCheck

		DP1I_DownShow:
			lda Pad_DwnSpr      ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_LeftCheck:
	    lda Pad1_LEFT
	    and #1
	    cmp #1
	    beq DP1I_LeftShow
	    bne DP1I_RightCheck

		DP1I_LeftShow:
			lda Pad_LeftSpr     ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_RightCheck:
	    lda Pad1_RIGHT
	    and #1
	    cmp #1
	    beq DP1I_RightShow
	    bne DP1I_ACheck

		DP1I_RightShow:
			lda Pad_RightSpr    ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_ACheck:
	    lda Pad1_A
	    and #1
	    cmp #1
	    beq DP1I_AShow
	    bne DP1I_BCheck

		DP1I_AShow:
			lda Pad_ASpr  	    ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_BCheck:
	    lda Pad1_B
	    and #1
	    cmp #1
	    beq DP1I_BShow
	    bne DP1I_SelCheck

		DP1I_BShow:
			lda Pad_BSpr  	    ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_SelCheck:
	    lda Pad1_SELECT
	    and #1
	    cmp #1
	    beq DP1I_SelShow
	    bne DP1I_StartCheck

		DP1I_SelShow:
			lda Pad_SelSpr      ; tile number 
			sta Sprite2_Tile
			jmp DP1I_Done



	DP1I_StartCheck:
	    lda Pad1_START
	    and #1
	    cmp #1
	    beq DP1I_StartShow
        ; No input detected.
	        lda #00
	        sta Sprite2_Tile
	        jmp DP1I_Done

	    bne DP1I_Done

		DP1I_StartShow:
			lda Pad_StartSpr    ; tile number 
			sta Sprite2_Tile



DP1I_Done:
        pla
        rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of Display_Pad1_Input                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/