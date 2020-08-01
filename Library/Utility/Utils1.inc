
; Only call this routine from a "Root-level" procedure, otherwise you could wind up overwriting
; the value to a previous Outer set of variables and do NOT use it in helper / utility functions
; as a call to that utility function would cause the same issue!!!
; SFPA_Ret = Save fuinction prlogue and return
SFPA_Ret:
    sta Reg_Outer_AValue
    stx Reg_Outer_XValue
    sty Reg_Outer_YValue

        pla
        sta PC_Inner_A
        pla
        sta PC_Inner_B

        pla
        sta PC_Outer_A
        pla
        sta PC_Outer_B

        lda PC_Inner_B
        pha
        lda PC_Inner_A
        pha


    ; At this point, the previous function call's return address has been saved in PC_Outer A and B,
    ; and this function's return address has been restored. Now all that is left is to restore
    ; register A's value, and rts :) 
    ;    lda Reg_AValue
    ;    ldx Reg_XValue


    rts







; Jmp to this at the end of any function that handled storing the stack safely so the stack can
; be used for function parameters (like PC architecture style with pushes and then a call)
;
; Allows usage of register A, and X
;
; This code makes 2 assumptions; read before using!!!!
;
;       Assumption 1:  code will NOT use Unsafe_Var1 and Unsafe_Var2 in code prior to calling this!!!
;                      NOTE: This behavior may adapt at a later point so that variables specifically for
;                            restoring the prologue will be used instead. Right now it's pretty unsafe!
;       Assumption 2:  register A was stored in Unsafe_Var1 and register X was stored in Unsafe_Var2
;
; RFPA_Ret = Restore function prologue and return
; Only call this routine from a "Root-level" procedure, otherwise you could wind up overwriting
; the value to a previous Outer set of variables and do NOT use it in helper / utility functions
; as a call to that utility function would cause the same issue!!!
RFPA_Ret:
    ;stx Reg_XValue
	;sta Reg_AValue
	        ;Restore the return address back onto the stack
	            lda PC_Outer_B
	            pha
	            lda PC_Outer_A
	            pha
	
	; Don't return JUST yet, we need to restore register A's value now too. We couldn't do it earlier, because
	; we needed to still be able to use register A to push a byte-value of the return address back onto the
	; stack (and there's no instruction to push a variable onto the stack, only a register!). So let's do that
	; now (restore register A's value)
        ;lda Reg_AValue
        ;ldx Reg_XValue

        lda Reg_Outer_AValue
        ldx Reg_Outer_XValue
        ldy Reg_Outer_YValue
    
    ; NOW we should be safe to rts :)
	    rts






; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/    x86 simulating procedures                                       _/
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
inc_sp:
    pha
    lda sp
    adc #%01
    sta sp
    pla

    rts


dec_sp:
    pha
    lda sp
    sbc #%01
    sta sp
    pla

    rts

; These are correct, however what happens is once the push procedure returns
; it DOES restore in the correct order (infact everything about this works
; correctly) however, what is at the top of the stack is the values of the
; simulated register (as what should be since that's the purpose of this
; procedure). However, the NES will wind up using that as the return address!
;
; Store the 2 bytes of ax on the stack
push_ax:
    ; Since the temporary variables are about to be cleared out, the values need
    ; to first be preserved since they hold the original A and X register values
	    sta push_temp
	    stx pushX_temp

    ; Set ourselves to the beginning address of the eax register variable
	    ldx #$00
	    lda ax, x
        ;pha
        ldx sp
        sta stack,x
        jsr inc_sp  ; Technically, I suppose bp could be set to the offset address instead of containing the offset amount


    ; Increment the offset address by 1 and set ourselves to that offset
    ; into the eax register variable
	    ldx #$01
	    lda ax, x
	    ;pha
	    ldx sp
	    sta stack,x
	    jsr inc_sp  ; Technically, I suppose bp could be set to the offset address instead of containing the offset amount

 
    jsr Store_ax_Word

    rts
;push_ah:
;push_al:


; Restore the 2 bytes of ax from the stack
pop_ax:

    ; Since the temporary variables are about to be cleared out, the values need
    ; to first be preserved since they hold the original A and X register values
	    sta push_temp
	    stx pushX_temp

    jsr Restore_ax_Word

    ; Not needed, since the sp register was being adjusted as the register is being restored
        ;jsr dec_sp  ; Technically, I suppose bp could be set to the offset address instead of containing the offset amount

    rts
;push_ah:
;push_al:


Store_ax_Word:
	    ldx #$00
		    lda push_temp,x
		    pha

        ldx #$01
            lda push_temp,x
            pha



        ldx #$00
		    lda pushX_temp,x
		    pha

        ldx #$01
            lda pushX_temp,x
            pha


    ; Clear out the temporary variable values
	    ldx #$00
	    sta push_temp,x
	    sta pushX_temp,x
	    ldx #$01
	    sta pushX_temp,x
	    sta pushX_temp,x


    ; Finally, restore the register values from the stack where they were
    ; preserved prior to clearing out the values in the temporary variables
        pla
        pla     ; Since the NES registers are only 8-bits, no need to offset to + 1 when restoring the native register values
        tax

        pla
        pla

    rts


Restore_ax_Word:
    ; <----- Essentially stack to ax ------>
        ; First, pre-subtract one from sp since the current location in the stack that the sp register would be pointing to would be a new element
            dec sp
            ldx sp

	    ; Take one byte in from the stack, and store it at the end (simulating little-endian behavior) of the register
	        lda stack, x
	            ldx #$01
	            sta ax, x
	
	    ; Take one byte in from the stack, and store it at the position previous to the current position (simulating little-endian behavior) of the register
		    dec sp
		    ldx sp
	
	        lda stack, x
	            ldx #$00
	            sta ax, x


    ; Pull the values from the temporary values (which should represent the A and X register values respectively) and
    ; place them on the real stack so that these temporary variables can be cleared back out.
	    lda push_temp,x
	    pha
	    lda pushX_temp,x
	    pha

    ; Clear the temporary variables now that their contents have been pushed to the real stack
	    ldx #$00
	    lda #$00
	    sta push_temp,x
	    sta pushX_temp,x
    ; Now, restore the A and X registers by pulling back off from the real stack
        pla
        tax
        pla

    rts



; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/    wcmp                                                            _/
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; Compare the values of two word sized variables. This procedure works
; by using an offset of +1 to both addresses to compare the last byte
; after performing the comparison of the first byte
;
;wcmp:











; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/    Debug procedures                                                _/
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

; Since this function was called using the stack for parameters, the return address
; first needs to be pulled off the stack to get to those parameters. However,
; the return address needs to be preserved for the end of the routine, when
; rts is called.
;
; This function takes a single stack parameter of what address to begin writting to
; and will write that "DEADBEEF" in little-ending form into memory starting at that
; memory address. Useful for debugging as it can be used as a sort of "tracer-dye"
; when analyzing memory.
Write_DEADBEEF:
    jmp SFPA_Ret
    ; We should now be sitting at the function's first parameter (with register A and register X being available for use
    ; as it will be restored at the end when jumping to RFPA_Ret :) )



    pla     ; Pop the function's first parameter into register A
    ;tax     ; Transfer register A's value to register X for index-addressing
    sty Function_Param1

    lda #$FE
    sta (Function_Param1), X
    inx

    lda #$EB
    sta (Function_Param1), X
    inx

    lda #$DA
    sta (Function_Param1), X
    inx

    lda #$ED
    sta (Function_Param1), X
    inx



    jmp RFPA_Ret    ; Restore the function prologue and return (jsr)