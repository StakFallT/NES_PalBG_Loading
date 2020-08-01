; Pseudo-stack testing code
	; Code tested and works!
		    ; This works PERFECTLY!!!!
			    lda #50
			    sta stack_mem
			
			    ; Testing code for x86 push and pop simulated procedures
			        ldx #$00
			
			            lda #$0A
			            sta ax, x
			
			
			        ldx #$01
			
			            lda #$0B
			            sta ax, x
			
			
			        lda #$12
			        ldx #$34
			
			        jsr push_ax
		
		
		            ldx #$00
		                lda #$0C
		                sta ax, x
		            ldx #$01
		                lda #$0D
		                sta ax, x
		
		            lda #$12
		            ldx #$34
		            jsr push_ax
		
		    ; This works seems to work PERFECTLY!!!!
		        ; First clear out the ax register
		            lda #$00
			            ldx #$00
			            sta ax, x
			            ldx #$01
			            sta ax, x
		
		            lda #$12
		            ldx #$34
		
		        jsr pop_ax