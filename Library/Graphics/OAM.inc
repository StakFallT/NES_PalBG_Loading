Inc_SpriteCount:
    inc SpriteCount
    rts

Reset_SpriteCount:
    pha
    lda #00
    sta SpriteCount
    pla
    rts

OAM_Copy:
    lda #3
    sta $4014
    rts
    
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Update_OAM_Sprite                                       _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	Update_OAM_Sprite:
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
	
	
	    ; ...
	
	
	
	    U_OAM_Spr_Done:
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
; _/            End of Update_OAM_Sprite                                _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/




; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Update_OAM_SpriteY                                      _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; Stack parameters:
; Param 1: Sprite number to update
; Param 2: The new value to set the sprite's y-coord to
	Update_OAM_SpriteY:
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
	
	    stx Unsafe_Var1             ; About to trample the X register, so store it "safely" away for now
		    ldx #%0
		    stx Unsafe_Counter1     ; Zero out the Unsafe_Counter1 variable
		    stx Unsafe_Offset1      ; Zero out the Unsafe_Offset1 variable

	    pla     ; Retrieve the sprite number off the stack
	    sta Unsafe_Var2

	    U_OAM_SprY_Locate:
		        dec Unsafe_Var2
		        lda Unsafe_Var2

		    U_OAM_SprY_LocateLoop:
		        cmp Unsafe_Counter1
		        beq U_OAM_SprY_Located
		
		        ; Add 4 bytes further more since each sprite chunk is 4 bytes (4 fields -- y, tile, special, x)
			        inx
			        inx
			        inx
			        inx
		            stx Unsafe_Counter1

		        adc Unsafe_Counter1
		        jmp U_OAM_SprY_LocateLoop
		
			    U_OAM_SprY_Located:
			            stx Unsafe_Counter1
			        pla     ; Retrieve the value to set the sprite number's y-coord
			        sta $0300,x
	
	    U_OAM_SprY_Done:
	        ; Clear out the volatile variables used -- not that it should matter, but just to ensure stability and consistency
		        ldx #%0
		        stx Unsafe_Counter1
		        stx Unsafe_Offset1

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
; _/            End of Update_OAM_SpriteY                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/






; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Update_OAM_SpriteTile                                   _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; Stack parameters:
; Param 1: Sprite number to update
; Param 2: The new value to set the sprite's tile to
	Update_OAM_SpriteTile:
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
	
	    stx Unsafe_Var1             ; About to trample the X register, so store it "safely" away for now
		    ldx #%0
		    stx Unsafe_Counter1     ; Zero out the Unsafe_Counter1 variable
		    stx Unsafe_Offset1      ; Zero out the Unsafe_Offset1 variable

	    pla     ; Retrieve the sprite number off the stack
	    sta Unsafe_Var2

	    U_OAM_SprTile_Locate:
		        dec Unsafe_Var2
		        lda Unsafe_Var2

		    U_OAM_SprTile_LocateLoop:
		        cmp Unsafe_Counter1
		        beq U_OAM_SprTile_Located
		
		        ; Add 4 bytes further more since each sprite chunk is 4 bytes (4 fields -- y, tile, special, x)
			        inx
			        inx
			        inx
			        inx
		            stx Unsafe_Counter1

		        adc Unsafe_Counter1
		        jmp U_OAM_SprTile_LocateLoop
		
			    U_OAM_SprTile_Located:
			            inx
			            stx Unsafe_Counter1
			        pla     ; Retrieve the value to set the sprite number's tile-coord
			        sta $0300,x
	
	    U_OAM_SprTile_Done:
	        ; Clear out the volatile variables used -- not that it should matter, but just to ensure stability and consistency
		        ldx #%0
		        stx Unsafe_Counter1
		        stx Unsafe_Offset1

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
; _/            End of Update_OAM_SpriteTile                            _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/







; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Update_OAM_SpriteSpecial                                _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; Stack parameters:
; Param 1: Sprite number to update
; Param 2: The new value to set the sprite's special value to
	Update_OAM_SpriteSpecial:
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
	
	    stx Unsafe_Var1             ; About to trample the X register, so store it "safely" away for now
		    ldx #%0
		    stx Unsafe_Counter1     ; Zero out the Unsafe_Counter1 variable
		    stx Unsafe_Offset1      ; Zero out the Unsafe_Offset1 variable

	    pla     ; Retrieve the sprite number off the stack
	    sta Unsafe_Var2

	    U_OAM_SprSpecial_Locate:
		        dec Unsafe_Var2
		        lda Unsafe_Var2

		    U_OAM_SprSpecial_LocateLoop:
		        cmp Unsafe_Counter1
		        beq U_OAM_SprSpecial_Located
		
		        ; Add 4 bytes further more since each sprite chunk is 4 bytes (4 fields -- y, tile, special, x)
			        inx
			        inx
			        inx
			        inx
		            stx Unsafe_Counter1

		        adc Unsafe_Counter1
		        jmp U_OAM_SprSpecial_LocateLoop
		
			    U_OAM_SprSpecial_Located:
			            inx
			            inx
			            stx Unsafe_Counter1
			        pla     ; Retrieve the value to set the sprite number's special value
			        sta $0300,x
	
	    U_OAM_SprSpecial_Done:
	        ; Clear out the volatile variables used -- not that it should matter, but just to ensure stability and consistency
		        ldx #%0
		        stx Unsafe_Counter1
		        stx Unsafe_Offset1

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
; _/            End of Update_OAM_SpriteSpecial                         _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/







; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            Update_OAM_SpriteX                                      _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; Stack parameters:
; Param 1: Sprite number to update
; Param 2: The new value to set the sprite's x-coord to
	Update_OAM_SpriteX:
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
	
	    stx Unsafe_Var1             ; About to trample the X register, so store it "safely" away for now
		    ldx #%0
		    stx Unsafe_Counter1     ; Zero out the Unsafe_Counter1 variable
		    stx Unsafe_Offset1      ; Zero out the Unsafe_Offset1 variable

	    pla     ; Retrieve the sprite number off the stack
	    sta Unsafe_Var2

	    U_OAM_SprX_Locate:
		        dec Unsafe_Var2
		        lda Unsafe_Var2

		    U_OAM_SprX_LocateLoop:
		        cmp Unsafe_Counter1
		        beq U_OAM_SprX_Located
		
		        ; Add 4 bytes further more since each sprite chunk is 4 bytes (4 fields -- y, tile, special, x)
			        inx
			        inx
			        inx
			        inx
		            stx Unsafe_Counter1

		        adc Unsafe_Counter1
		        jmp U_OAM_SprX_LocateLoop
		
			    U_OAM_SprX_Located:
			        ; Add 3 bytes further more to get to the x field
				        inx
				        inx
				        inx
			            stx Unsafe_Counter1
			        pla     ; Retrieve the value to set the sprite number's x-coord
			        sta $0300,x
	
	    U_OAM_SprX_Done:
	        ; Clear out the volatile variables used -- not that it should matter, but just to ensure stability and consistency
		        ldx #%0
		        stx Unsafe_Counter1
		        stx Unsafe_Offset1

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
; _/            End of Update_OAM_SpriteX                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/