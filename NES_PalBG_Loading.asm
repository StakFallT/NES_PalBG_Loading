; INES header stuff
	; Specify the bank layout
		.inesprg 1   ; 1 bank of code
		.ineschr 1   ; 1 bank of spr/bkg data
		.inesmir 1   ; 1 bank of mirror - always 1
	
	; Set any other settings that may be set prior to initialization of varios entities
		.inesmap 0   ; we use mapper 0
	
	; nesasm.exe no like these .rs instruction...
	;    .rsset $0000
	;    FrameCount_InputDelay .rs 4
	
	; Set up the code bank
		.bank 0   	; bank 0 - our place for code.
		.org $0000  ; The start of free memory (CPU)    Variable ORG location $0000
		; Normally variables defined here...

		    FrameCount_InputDelay:
		            .db 0
		    FileOffset:
		            .dw 0
		    VBlankOrNo:
		            .db 0
            Sunset_Delay:
	                .dw 5
	        Sunset_DelayCounter:
	                .dw 0
	        Sunset_PaletteNumber:
	                .db 0
			Unsafe_Var1:
			        .db 0
			Unsafe_Var2:
			        .db 0
            Function_Param1:
                    .dw 0
			Reg_AValue:
			        .dw 0
			Reg_BValue:
			        .dw 0
			Reg_XValue:
			        .dw 0
			Reg_Outer_AValue:
			        .dw 0
			Reg_Outer_BValue:
			        .dw 0
			Reg_Outer_XValue:
			        .dw 0
			Reg_Outer_YValue:
			        .dw 0
			Unsafe_Counter1:
			        .db 0
		    Unsafe_Offset1:
		            .db 0
			SpriteNumber:
			        .db 0
			SpriteCount:
			        .db 0

            Pad1_A:
			        .db 0
			Pad1_B:
			        .db 0
			Pad1_SELECT:
			        .db 0
			Pad1_START:
			        .db 0
			Pad1_UP:
			        .db 0
			Pad1_DOWN:
			        .db 0
			Pad1_LEFT:
			        .db 0
			Pad1_RIGHT:
			        .db 0

			Pad_UpSpr:
			        .db 0
			Pad_DwnSpr:
			        .db 0
			Pad_LeftSpr:
			        .db 0
			Pad_RightSpr:
			        .db 0
			Pad_ASpr:
			        .db 0
			Pad_BSpr:
			        .db 0
			Pad_SelSpr:
			        .db 0
			Pad_StartSpr:
			        .db 0

            ;PC = Program Counter -- variables used to store and restore the return
            ;                        address (for functions that utilize the stack for parameters)
				PC_A:
				        .db 0
				PC_B:
				        .db 0

                PC_Outer_A:
				        .db 0
				PC_Outer_B:
				        .db 0

                PC_Inner_A:
				        .db 0
				PC_Inner_B:
				        .db 0

            Input_Delay0:
			        .db 0

            PalCycle_0:
                    .db 0
            PalCycle_1:
                    .db 0
            PalCycle_2:
                    .db 0
            PalCycle_3:
                    .db 0

            ; Simulated x86 arch
                ; Stack mechanics
            stack:
                .db 0,0,0,0,0,0,0,0,0,0
                .db 0,0,0,0,0,0,0,0,0,0
                .db 0,0,0,0,0,0,0,0,0,0
                .db 0,0,0,0,0,0,0,0,0,0
                .db 0,0,0,0,0,0,0,0,0,0

            stack_mem:
                .db 50  ; 50 bytes of stack space

            bp:
                .db 0   ; Stack has to be less than 256 bytes otherwise this
                        ; variable has to be a larger data type size and that
                        ; would require much more complex code.

            sp:
                .db 0   ; Stack has to be less than 256 bytes otherwise this
                        ; variable has to be a larger data type size and that
                        ; would require much more complex code.

        ; Overhead /maintenance variables
            push_temp:
                .db 0
                .db 0
            pushX_temp:
                .db 0

            pop_temp:
                .db 0
                .db 0
            popX_temp:
                .db 0

            ; Simulated x86 registers
	            eax:
	                .db 0
	                .db 0
		            ax:
			            ah:         ; +2 bytes from eax, just like x86 
			                .db 0
			            al:
			                .db 0

	            ebx:
	                .db 0
	                .db 0
		            bx:
			            bh:         ; +2 bytes from ebx, just like x86
			                .db 0
			            bl:
			                .db 0

	            ecx:
	                .db 0
	                .db 0
		            cx:
			            ch:         ; +2 bytes from ecx, just like x86
			                .db 0
			            cl:
			                .db 0

	            edx:
	                .db 0
	                .db 0
		            dx:
			            dh:         ; +2 bytes from edx, just like x86
			                .db 0
			            dl:
			                .db 0

    .org $0300  ; OAM Copy location $0300
	    ; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
	    ; _/                            OAM set up                                      _/
	    ; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
		        Base_OAM:
				Sprite1_Y:
				    .db 0
				Sprite1_Tile:
				    .db 0
				Sprite1_Special:
				    .db 0
				Sprite1_X:
				    .db 0
			
			    Sprite2_Y:
				    .db 0
				Sprite2_Tile:
				    .db 0
				Sprite2_Special:
				    .db 0
				Sprite2_X:
				    .db 0




    .org $8000  ; code ORG starts at $8000

Start:
;Reset:

    jsr Initialize_Variables


vblankwait1:
;WaitForVBlank:
    ;dec VBlankOrNo
	;lda VBlankOrNo
	
	;bne WaitForVBlank
	
	BIT $2002
	bpl vblankwait2

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x    ;move all sprites off screen
  INX
  BNE clrmem


;vblankwait2 is essentially the main game loop in a sense? (At least it seems like it winds up being that)
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            waitblank                                               _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
;waitblank:
vblankwait2:
    ;jsr OAM_Copy

    BIT $2002
    bpl vblankwait2
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of waitblank                                        _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/


    jsr Set_PPU_BGPal_Addr      ; Set up the address for where data written to $2006 gets stored
	ldx #$00                    ; Set x (the offset into the palette file data) to 0
	jsr loadbgpal

	jsr Set_PPU_SprPal_Addr     ; Set up the address for where data written to $2006 gets stored
    ldx #16                     ; Set x (the offset into the palette file data) to 16 bytes in, since the bg palettes should already have been loaded in now
	jsr loadsprpal




    jsr SunsetPalette_ShiftCheck



    ; The chr file is injected into the bytes of data by the assembler via an .incbin instruction
    ; after org'ing to $0000 after switching to bank 2
    ;...
    
    ; Next up is the background (nametable - tile map / attribute table)
    

    jsr LoadBackground


    jsr Set_PPU_PtrnAndNT_TableAddrLayout
	jsr Set_PPU_DisplayProperties

infin:
    ; All of the above code finished, so fall down here until a non-maskable interrupt has occured.
    ; Think of the NMI routine as our frame-updating hook; without it, the game would just be
    ; a static imageand wouldn't change since this infin routine just jumps to itself. The interrupt
    ; is what allows the code to actually jump out of that loop temporarily (returning back to it
    ; afterwards) providing an opportunity to do things

	jmp infin 	; JuMP to infin. note that this loop never ends. :)


; Called via an interrupt that was set up at the start of the rom (the code is located near the bottom of this file)
NMI:
    lda #$00
    sta $2003
    lda #$03
    sta $4014


    ; Reset the PPU address registers
        lda #$00
        sta $2006
        sta $2006

	
	    JSR OAM_Copy



        jsr Set_PPU_PtrnAndNT_TableAddrLayout
	    jsr Set_PPU_DisplayProperties


    ; Add other regular game engine code (input handling, etc.) here
        jsr Pad1_InputDetection
		jsr Pad1_InputLoop    
	
	    jsr Display_Pad1_Input


        jsr SunsetPalette_ShiftCheck

    ; Finally, return back to whereever the codeflow was before the interrupt was triggered (likely the infin routine loop)
        rti


    .bank 1
    .org $E000

    ;include asm code files here
        .include "Startup.inc"
        .include "Library/Utility/Utils1.inc"

        .include "Library/Graphics/OAM.inc"

	    .include "Library/Input/Input_Routines.inc"

        .include "Library/Graphics/PPU_TableLoading.inc"
	    .include "Library/Graphics/PPU.inc"
	    .include "Library/Graphics/PPU_Utils.inc"
	    .include "Library/Graphics/GFX_Utils.inc"




    ; Palette files and nametable files go here 
        tilepal: .incbin "Assets/Default/Palettes/0/Hybrid/Hope2020.pal"

        ;tilepal:
        ;    .db $0F,$30,$12,$15,$0F,$30,$12,$15,$0F,$30,$12,$15,$0F,$30,$12,$15
        ;    .db $0F,$30,$32,$33,$0F,$30,$32,$33,$0F,$30,$32,$33,$0F,$30,$32,$33

        Hope2020_Ntbl0_Part1:     .incbin "Assets/Default/NameTables/0/TileMaps/0/Hope2020_ntbl0_part1.tbl"                                 ; 255 bytes
        Hope2020_Ntbl0_Part2:     .incbin "Assets/Default/NameTables/0/TileMaps/0/Hope2020_ntbl0_part2.tbl"                                 ; 255 bytes
        Hope2020_Ntbl0_Part3:     .incbin "Assets/Default/NameTables/0/TileMaps/0/Hope2020_ntbl0_part3.tbl"                                 ; 255 bytes
        Hope2020_Ntbl0_Part4:     .incbin "Assets/Default/NameTables/0/TileMaps/0/Hope2020_ntbl0_part4.tbl"                                 ; 195 bytes ($C0)
        ;                                                                                                                                   ------------
        ;                                                                                                                                     960 bytes
        ;                                                                                                                                   ------------
        Hope2020_Attrtbl0:     .incbin "Assets/Default/NameTables/0/AttributeTables/0/Hope2020_ntbl0_attr.tbl"                              ;               -64 bytes-
        ;                                                                                                                                   ============
        ;                                                                                                                                    1 KB

		;Hope2020_Ntbl1:     .incbin "Assets/Chr/Backgrounds/Hope2020_ntbl1.tbl"
		;Hope2020_Ntbl2:     .incbin "Assets/Chr/Backgrounds/Hope2020_ntbl2.tbl"
		;Hope2020_Ntbl3:     .incbin "Assets/Chr/Backgrounds/Hope2020_ntbl3.tbl"





    .org $FFFA  ; start at $FFFA
	
	    ; Set three interrupts
			.dw NMI             ; Address to execute upon VBlank
			.dw Start           ; Address to execute upon RESET
			.dw 0               ; Address to execute during a BRK instruction

	; Set up the background and sprite bank
	.bank 2   	; switch to bank 2
	.org $0000  ; start at $0000
	
	    ;chr file is BOTH (BG and Spr) pattern tables (8KB total)
		    Hope2020_CHR:   .incbin "Assets/Default/Chr (Pattern Tables)/0/Hybrid/Hope2020.chr"