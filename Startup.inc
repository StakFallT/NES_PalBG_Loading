Initialize_Variables:
    ;lda #400
    ;sta Sunset_Delay


    lda #$00
    sta PalCycle_0

    ;lda #$55
    adc #$55
    sta PalCycle_1

    ;lda #$AA
    adc #$55
    sta PalCycle_2

    ;lda #$FF
    adc #$55
    sta PalCycle_3



    lda #$04
    sta FrameCount_InputDelay



    lda #$00
    sta SpriteCount


    lda #$01
    sta SpriteNumber



    lda #$32                ; 50 (decimal)
    sta Sprite1_Y

    lda #$01
    sta Sprite1_Tile

    lda #$00
    sta Sprite1_Special

    lda #$14                ; 20 (decimal)
    sta Sprite1_X



    lda #$95                ; 50 (decimal)
    sta Sprite2_Y

    lda #$00
    sta Sprite2_Tile

    lda #$00
    sta Sprite2_Special

    lda #$78                ; 20 (decimal)
    sta Sprite2_X



    lda #$2C
    sta Pad_UpSpr

    lda #$2D
	sta Pad_DwnSpr

    lda #$2E
	sta Pad_LeftSpr

    lda #$2F
	sta Pad_RightSpr

    lda #$30
	sta Pad_ASpr

    lda #$31
	sta Pad_BSpr

    lda #$32
	sta Pad_SelSpr

    lda #$33
	sta Pad_StartSpr

    rts