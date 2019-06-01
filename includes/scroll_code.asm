; Scroll the playfield attribute RAM (unrolled for speed)
scroll_update	lda scroll_x
		clc
		adc #$05
		sta scroll_x

		and #$07
		eor #$17
		sta d016_mirror
		sta d016_scrl_mr

		lda scroll_x
		cmp #$08
		bcs su_go
		rts

su_go		and #$07
		sta scroll_x

		jsr ul_code_dest

; Build a column from the current tiles
su_column_draw	ldy tile_count

		lda (tile_read_1),y
		sta column_buffer+$00
		lda (tile_read_2),y
		sta column_buffer+$04
		lda (tile_read_3),y
		sta column_buffer+$08
		lda (tile_read_4),y
		sta column_buffer+$0c
		lda (tile_read_5),y
		sta column_buffer+$10

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$01
		lda (tile_read_2),y
		sta column_buffer+$05
		lda (tile_read_3),y
		sta column_buffer+$09
		lda (tile_read_4),y
		sta column_buffer+$0d
		lda (tile_read_5),y
		sta column_buffer+$11

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$02
		lda (tile_read_2),y
		sta column_buffer+$06
		lda (tile_read_3),y
		sta column_buffer+$0a
		lda (tile_read_4),y
		sta column_buffer+$0e
		lda (tile_read_5),y
		sta column_buffer+$12

		tya
		clc
		adc #$04
		tay

		lda (tile_read_1),y
		sta column_buffer+$03
		lda (tile_read_2),y
		sta column_buffer+$07
		lda (tile_read_3),y
		sta column_buffer+$0b
		lda (tile_read_4),y
		sta column_buffer+$0f
		lda (tile_read_5),y
		sta column_buffer+$13

; Count how far into the tile we've got
		ldx tile_count
		inx
		stx tile_count
		cpx #$04
		beq *+$05
		jmp su_cd_exit

; Read the next column of tiles
		ldy #$00
		lda (map_position),y		; tile row $00
		cmp #$ff
		bne su_no_end

; $ff found, so clear map_flag to signal the end has been reached
		lda #$00
		sta map_flag

; Carry on with unpacking the column of tiles
; (If any are $b3, change the background colours and set to $00)
su_no_end	cmp #$b3
		bne su_colour_skp_1

		inc level_col_count
		lda #$00

su_colour_skp_1	sta tile_read_1+$00
		lda #$00
		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		asl tile_read_1+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_1+$01

		lda tile_read_1+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_1+$01
		sta tile_read_1+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $01
		cmp #$b3
		bne su_colour_skp_2

		inc level_col_count
		lda #$00

su_colour_skp_2	sta tile_read_2+$00
		lda #$00
		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		asl tile_read_2+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_2+$01

		lda tile_read_2+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_2+$01
		sta tile_read_2+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $02
		cmp #$b3
		bne su_colour_skp_3

		inc level_col_count
		lda #$00

su_colour_skp_3	sta tile_read_3+$00
		lda #$00
		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		asl tile_read_3+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_3+$01

		lda tile_read_3+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_3+$01
		sta tile_read_3+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $03
		cmp #$b3
		bne su_colour_skp_4

		inc level_col_count
		lda #$00

su_colour_skp_4	sta tile_read_4+$00
		lda #$00
		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		asl tile_read_4+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_4+$01

		lda tile_read_4+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_4+$01
		sta tile_read_4+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

		lda (map_position),y		; tile row $04
		cmp #$b3
		bne su_colour_skp_5

		inc level_col_count
		lda #$00

su_colour_skp_5	sta tile_read_5+$00
		lda #$00
		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		asl tile_read_5+$00
		rol
		clc
		adc #>tile_data
		sta tile_read_5+$01

		lda tile_read_5+$00
		clc
		adc #<tile_data
		bcc *+$04
		inc tile_read_5+$01
		sta tile_read_5+$00

		inc map_position+$00
		bne *+$04
		inc map_position+$01

; Reset the tile counter
		lda #$00
		sta tile_count

su_cd_exit	rts

; Reset the map and tile readers to the start of their data
scroll_reset	lda #<level_data
		sta map_position+$00
		lda #>level_data
		sta map_position+$01

		lda #<tile_data
		sta tile_read_1+$00
		sta tile_read_2+$00
		sta tile_read_3+$00
		sta tile_read_4+$00
		sta tile_read_5+$00

		lda #>tile_data
		sta tile_read_1+$01
		sta tile_read_2+$01
		sta tile_read_3+$01
		sta tile_read_4+$01
		sta tile_read_5+$01

		lda #$00
		sta tile_count

		lda #$01
		sta map_flag

; Fetch the first column of data
		jsr su_column_draw

		rts


; Code unroller - builds the background scroll routine at $8000
scroll_unroll	lda #<ul_code_dest
		sta ul_code_write+$00
		lda #>ul_code_dest
		sta ul_code_write+$01

		lda #$14
		sta ul_row_count

; Code builder's outer loop
ul_code_gen	lda #$26
		sta ul_column_count

; Copy the first code block to RAM
ul_code_copy_1	lda #<ul_code_block_1
		sta ul_code_read+$00
		lda #>ul_code_block_1
		sta ul_code_read+$01

		ldx #$00
		ldy #$00
ul_c1c_loop	lda (ul_code_read),y
		sta (ul_code_write),y

		inc ul_code_read+$00
		bne *+$04
		inc ul_code_read+$01

		inc ul_code_write+$00
		bne *+$04
		inc ul_code_write+$01

		inx
		cpx #$0c
		bne ul_c1c_loop

; Get the current read/write locations
		lda ul_code_block_1+$01
		sta ul_code_block_2+$04
		lda ul_code_block_1+$02
		sta ul_code_block_2+$05

		lda ul_code_block_1+$07
		sta ul_code_block_2+$0a
		lda ul_code_block_1+$08
		sta ul_code_block_2+$0b

; Bump the locations and see if the loop is done
		jsr ul_cb_bump

		dec ul_column_count
		bne ul_code_copy_1

; Copy the second code block to RAM
ul_code_copy_2	lda #<ul_code_block_2
		sta ul_code_read+$00
		lda #>ul_code_block_2
		sta ul_code_read+$01

		ldx #$00
		ldy #$00
ul_c2c_loop	lda (ul_code_read),y
		sta (ul_code_write),y

		inc ul_code_read+$00
		bne *+$04
		inc ul_code_read+$01

		inc ul_code_write+$00
		bne *+$04
		inc ul_code_write+$01

		inx
		cpx #$0c
		bne ul_c2c_loop

; Nudge everything forwards a little before starting a new row
		inc ul_code_block_2+$01
		bne *+$05
		inc ul_code_block_2+$02

		jsr ul_cb_bump
		jsr ul_cb_bump
		dec ul_row_count
		beq *+$05
		jmp ul_code_gen

; Add a final RTS to our unrolled code
		lda #$60
		ldy #$00
		sta (ul_code_write),y

		rts

; Bump all the memory locations in the code block
ul_cb_bump	inc ul_code_block_1+$01
		bne *+$05
		inc ul_code_block_1+$02

		inc ul_code_block_1+$04
		bne *+$05
		inc ul_code_block_1+$05

		inc ul_code_block_1+$07
		bne *+$05
		inc ul_code_block_1+$08

		inc ul_code_block_1+$0a
		bne *+$05
		inc ul_code_block_1+$0b

		rts


; First code fragment that's repeatedly modified and copied to RAM
ul_code_block_1	lda screen_ram+$c9
		sta screen_ram+$c8
		lda colour_ram+$c9
		sta colour_ram+$c8

; Second code fragment that's repeatedly modified and copied to RAM
ul_code_block_2	ldy column_buffer
		sty screen_ram
		lda tile_col_data,y
		sta colour_ram
