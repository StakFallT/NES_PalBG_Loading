LoadBackground:
;NTbl_MemTest:



    lda $2002


    lda #$20
    sta $2006

    lda #$00
    sta $2006

    ldx #$00

 
 
    ; Load all 4 parts of the first 960 bytes of the nametable (tilemap)
		jsr Load_Ntbl0
		ldx #$00
	    jsr Load_Ntbl0_Tilemap_pt2
	    ldx #$00
	    jsr Load_Ntbl0_Tilemap_pt3
	    ldx #$00
	    jsr Load_Ntbl0_Tilemap_pt4

    ; Next, load in the remaining 64 bytes of the nametable (attribute table)
	    ldx #$00
	    jsr Load_Ntbl0_AttrTbl0




    ;ldx #$00
    ;ldy #$00
    ;lda Hope2020_Ntbl1
    ;sta FileOffset
	;jsr Load_Ntbl1

    ;ldx #$00
    ;ldy #$00
    ;lda Hope2020_Ntbl2
    ;sta FileOffset
	;jsr Load_Ntbl2

    ;ldx #$00
    ;ldy #$00
    ;lda Hope2020_Ntbl3
    ;sta FileOffset
	;jsr Load_Ntbl3


    rts


Load_Ntbl0:
Load_Ntbl0_Tilemap_pt1:
    lda Hope2020_Ntbl0_Part1, X
	sta $2007
	inx
	cpx #255
	bne Load_Ntbl0_Tilemap_pt1
    rts

Load_Ntbl0_Tilemap_pt2:
    lda Hope2020_Ntbl0_Part2, X
	sta $2007
	inx
	cpx #255
	bne Load_Ntbl0_Tilemap_pt2
    rts

Load_Ntbl0_Tilemap_pt3:
    lda Hope2020_Ntbl0_Part3, X
	sta $2007
	inx
	cpx #255
	bne Load_Ntbl0_Tilemap_pt3
    rts

Load_Ntbl0_Tilemap_pt4:
    lda Hope2020_Ntbl0_Part4, X
	sta $2007
	inx
	cpx #195    ; ($C0)
	bne Load_Ntbl0_Tilemap_pt4
    rts

Load_Ntbl0_AttrTbl0:
    lda Hope2020_Attrtbl0, X
	sta $2007
	inx
	cpx #64    ; ($40)
	bne Load_Ntbl0_AttrTbl0
    rts