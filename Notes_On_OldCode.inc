Load_Ntbl0:
Load_Ntbl0_Tilemap_pt1:
    ;PROBLEM!!!!: While this loop mathematically does appear to look as though it loops through all 960 bytes, x will eventually roll
    ;             over after the first 255, and what will happen is it will repeat the same 255 bytes 3 times since the offset (X register)
    ;             will never be able to be above 255 since it's datatype size is a single byte!!!! This is why the name tables need
    ;             need to be split up into 255 and then looped until 960 bytes are read
    lda Hope2020_Ntbl0, x
	sta $2007
	inx
	cpx #255
	beq Load_Ntbl0_IncY
	jmp Load_Ntbl0

    Load_Ntbl0_IncY:
        iny
        ;cpy #4          ; (255 * 4 = 1,024 bytes -- 1Kb)
        cpy #3          ; (255 * 3 = 960 bytes -- Tile map)
        ldx #$00
		bne Load_Ntbl0 ; if not all 64 done, loop and do some more

        rts









; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            LoadMap                                                 _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
loadmap:
; As per: https://patater.com/nes-asm-tutorials/day-13/
;   Here is a brief quote that contains some information about BGs and Name Tables (maps)
;       "To make the BG, open your "our.bkg" in Tile Layer Pro and draw some tiles. Now, make
;        a map out of the tiles on paper with the tile numbers, how do we get the tile
;        numbers? It's quite simple actually, starting with the first tile is #0 it goes
;        like this (assuming you have Tile Layer at 16 tiles per line):
;
;           $0  $1  $2  $3  $4  $5  $6  $7  $8  $9  $A  $B  $C  $D  $E  $F
;           $10 $11 $12 $13 $14...
;        So if #$1 was the top-left 1/4th of a smile face, #$2 is the top-right,
;       #$11 is the bottom -left and #$12 is the bottom right 1/4th, you would write on the
;       paper or in a text file:
;               1  2  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;               11 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
;               ..."
;
;       [An asside]
;       My annotation: [Essentially, you would draw out the background on a piece of paper (or digital art program), divide / carve
;        up the image into section (the size of what each tile is), then take those tile numbers and place them in
;        the arrangement to re-make the background again]
;
;       [Continuation from https://patater.com/nes-asm-tutorials/day-13/]
;       "Name Tables
;           Name Tables is what originally made me avoid NES Assembly, I just couldn't get
;           it from the sparce and vague and overly technical docs that currenty exist.
;
;           To put it simply "Name" means Tile Number and "Table" basically means Map, so
;           the Name Tables is where we want to put our map in PPU memory. There are two
;           Name Tables; typically you'll want to use the first one so we will too. Name
;           Table #1 is at $2020 in PPU memory, OK... that's a lie, it actually starts at
;           $2000 in PPU memory, but since US NESs (and most emulators) don't display the
;           first line (and last for that matter) we have to start the load at $2020.
;
;       Note: The NES Screen is about 32x32 tiles."

    lda $2002

	lda #$20
	sta $2006 ; give $2006 both parts of address $2020.
	lda #$00
	sta $2006

	ldx #$00
	ldy #00

	; Names = Tiles
	loadNameTableEntries:
		;lda Hope2020_Map, X ; load A with a byte from address (Hope2020.map + X)
		;lda Hope2020_BG_Ptbl, X
		lda Hope2020_Ntbl0, X
		inx
		sta $2007
		;cpx #64 ; map in previous section 64 bytes long
		cpx #255
		beq Load_NTE_IncY

    Load_NTE_IncY:
        iny
        cpy #16
		bne loadNameTableEntries ; if not all 64 done, loop and do some more

    rts
; _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
; _/            End of LoadMap                                          _/
;_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/