;
; STERCORE XD
;

; Programming, graphics, sound and other digital atrocities by
; Jason

; Cheat mode - $00 is normal game, $01 disables sprite collisions
cheat		= $00


; A second pass at converting a simple scrolling shoot 'em up
; which was coded for the CSS Crap Game Competition 2018, this
; time with character-based backgrounds and hardware sprites!
; Released at C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file, crunch it
; and bolt on the cartridge header of the release version.


; Select an output filename
		!to "stercore_xd.prg",cbm

; "Print" macro
!macro print .text_src, .text_dest, .text_len, .text_col {
		ldx #$00
		ldy #.text_col
.print_loop	lda .text_src,x
		sta screen_ram+.text_dest,x
		tya
		sta colour_ram+.text_dest,x
		inx
		cpx #.text_len
		bne .print_loop}


; Pull in the binary data
		* = $0800
music		!binary "binary\music.prg",,2

		* = $6000
		!binary "binary\sprites.spr"

		* = $7000
		!binary "binary\background.chr"

		* = $7800
		!binary "binary\characters.chr"


; Constants
raster_1_pos	= $00
raster_2_pos	= $56
raster_3_pos	= $dd
raster_4_pos	= $ed

; General label assignments
raster_num	= $50		; raster split counter
sync		= $51		; raster sync for runtime code
rt_store_1	= $52		; temporary store for runtime code

flash_timer	= $53		; timer for text flashing
flash_state	= $54		; $00 is normal, $01 is inverse

ctrl_buffer	= $55		; current joystick value
coll_temp	= $56		; collision work space - $04 bytes used

scroll_x	= $5a		; horizontal scroll position

d015_mirror	= $5b		; value for $d015 at the top of the screen
d016_mirror	= $5c		; value for $d016 at the start of the play area
d016_scrl_mr	= $5d		; value for $d016 where the scroller is
d018_mirror	= $5e		; value for $d018 at the start of the play area
d021_mirror	= $5f		; playfield colour (gets reset once a frame)

anim_timer	= $60		; sprite animation timer

wave_timer	= $61		; time to next speed change
wave_read	= $62		; attack wave read position - $02 bytes used

; Labels for the background scroller
tile_count	= $64		; column count in the current tile
map_flag	= $65		; end of map marker flag

map_position	= $66		; current read position for the map - $02 bytes used

tile_read_1	= $68		; tile row 1 read position - $02 bytes used
tile_read_2	= $6a		; tile row 2 read position - $02 bytes used
tile_read_3	= $6c		; tile row 3 read position - $02 bytes used
tile_read_4	= $6e		; tile row 4 read position - $02 bytes used
tile_read_5	= $70		; tile row 5 read position - $02 bytes used

level_col_count	= $72		; current level colour pointer

; Labels for the titles scroller
t_scrl_count	= $68		; where to fetch scroll text from - $02 bytes used
t_scrl_x	= $6a		; scroll register

t_colour_count	= $6b		; colour counter for the logo effect
t_luma_count	= $6c		; luminance counter for the logo effect


; Colour and luma effect work spaces
t_colour_work	= $0340
t_luma_work	= $0360

; Where to find the screen
screen_ram	= $6c00
colour_ram	= $d800


; Temporary labels for the code unroller (shared with the ones above)
ul_code_read	= $50		; $02 bytes
ul_code_write	= $52		; $02 bytes

ul_column_count	= $54
ul_row_count	= $55

; Destination for the code unroller
ul_code_dest	= $8000		; start of unrolled scroller code


; Entry point for the code
		* = $0e20

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$0b
		sta $d011

		lda #$35
		sta $01

; Call the unroller for the background scroller
		jsr scroll_unroll

; Repoint the NMI and IRQ interrupts
		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$0b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Clear the zero page and initialise some labels
		ldx #$50
		lda #$00
clear_zp	sta $00,x
		inx
		bne clear_zp

		lda #$01
		sta raster_num

; Select video bank 1
		lda #$c6
		sta $dd00

; Set up the music driver
		lda #$00
		jsr music+$00

; Restart the interrupts
		cli


; Titles page entry point
titles_init

; Clear the screen and set up the status bar
		lda #$0b
		sta $d011

		jsr screen_init

; Set up the scroller's colours
		ldx #$00
		ldy #$26
t_scrl_col_copy	lda t_scrl_c_data,x
		sta colour_ram+$370,x
		sta colour_ram+$370,y
		dey
		inx
		cpx #$14
		bne t_scrl_col_copy

; Reset the logo's colour effects
		lda #$00
		sta t_colour_count
		sta t_luma_count

; Clear the logo's colour and luma buffers
		ldx #$00
t_clr_logo_cl	lda t_colour_data
		sta t_colour_work,x
		lda t_luma_data
		sta t_luma_work,x
		inx
		cpx #$20
		bne t_clr_logo_cl

; Reset the scrolling message
		jsr t_scrl_reset
		lda #$00
		sta t_scrl_x

; Print the Stercore XD logo
		+print t_logo_1, $0f4, $1e, $00
		+print t_logo_2, $11c, $1e, $00
		+print t_logo_3, $144, $1e, $00

		+print t_logo_4, $177, $08, $01
		+print t_logo_5, $19f, $08, $01
		+print t_logo_6, $1c7, $08, $01

; Print "A C64CD Studios Production"
		+print t_credit_4, $20a, $22, $07

; Print "Converted and reworked..."
		+print t_credit_5, $258, $26, $0a
		+print t_credit_6, $284, $1e, $08

; Print "Design, coding, graphics, sounds..."
		+print t_credit_1, $2f8, $26, $0e
		+print t_credit_2, $320, $1e, $03
		+print t_credit_3, $341, $05, $0d

; Print "Press Fire to Start!"
		+print t_text_4, $3c9, $14, $0c

; Reset the flash timer and state labels
		lda #$00
		sta flash_timer
		sta flash_state

; Select status chars for the main play area and disable sprites
		lda #$07
		sta d016_mirror
		sta d016_scrl_mr
		lda #$be
		sta d018_mirror

		lda #$00
		sta d015_mirror

; Wait for a moment, then turn the screen back on
		ldy #$10
		jsr sync_wait_long
		lda #$1b
		sta $d011


; Titles page
titles_loop	jsr sync_wait

; Update the logo's colour wash effect
		ldx #$00
t_colour_move	lda t_colour_work+$01,x
		sta t_colour_work+$00,x
		inx
		cpx #$1f
		bne t_colour_move

		ldx t_colour_count
		lda t_colour_data,x
		sta t_colour_work+$1f
		inx
		cpx #$37
		bne *+$04
		ldx #$00
		stx t_colour_count

; Update the logo's luma wash effect
		ldx #$1f
t_luma_move	lda t_luma_work+$00,x
		sta t_luma_work+$01,x
		dex
		cpx #$ff
		bne t_luma_move

		ldx t_luma_count
		lda t_luma_data,x
		sta t_luma_work+$00
		inx
		cpx #$80
		bne *+$04
		ldx #$00
		stx t_luma_count

; Merge the logo's colour and luma data
		ldx #$00
t_colour_merge	lda t_colour_work,x
		clc
		adc t_luma_work,x
		tay
		lda t_colour_trans,y
		sta colour_ram+$0f4,x
		sta colour_ram+$11c,x
		sta colour_ram+$144,x

		sta colour_ram+$3c4,x
		inx
		cpx #$1e
		bne t_colour_merge

; Copy some of the colour effect down for the XD
		ldx #$0b
		ldy #$11
t_colour_copy	lda colour_ram+$0f4,y
		sta colour_ram+$16c,x
		sta colour_ram+$194,x
		sta colour_ram+$1bc,x
		dey
		inx
		cpx #$13
		bne t_colour_copy

; Update the scrolling message
		jsr t_scroller
		jsr t_scroller

; Start the game if fire has been pressed
		lda $dc00
		and #$10
		beq *+$05
		jmp titles_loop


; Clear the screen and set up the status bar
main_init	lda #$0b
		sta $d011

		jsr screen_init

; Reset the background scroll engine
		jsr scroll_reset

		lda #$00
		sta level_col_count

; Reset player and bullet co-ordinates
		lda #$24
		sta player_x
		lda #$9d
		sta player_y

		lda #$00
		sta bullet_x
		lda #$ff
		sta bullet_y

; Reset enemy co-ordinates and states
		ldx #$00
enemy_reset	lda sprite_x_dflt+$02,x
		sta enemy_1_x,x
		lda sprite_y_dflt+$02,x
		sta enemy_1_y,x

		lda #$01
		sta enemy_state,x
		inx
		cpx #$06
		bne enemy_reset

; Animation system reset
		ldx #$00
anim_reset	lda anim_start_dflt,x
		sta sprite_dp,x
		sta anim_start,x
		lda anim_end_dflt,x
		sta anim_end,x
		inx
		cpx #$08
		bne anim_reset

; Reset the attack wave engine
		jsr wave_reset
		jsr eu_wave_fetch

; Zero the score and set the lives counter
		ldx #$00
		txa
score_reset	sta player_score,x
		inx
		cpx #$05
		bne score_reset

		lda #$03
		sta player_lives

; Set the player shield
		lda #$32
		sta player_shield

; Reset the status bar
		jsr status_update

; Select in-game chars for the main play area and enable sprites
		lda #$17
		sta d016_mirror
		sta d016_scrl_mr
		lda #$bc
		sta d018_mirror

		lda #$ff
		sta d015_mirror

; Wait for a moment, then turn the screen back on
		ldy #$10
		jsr sync_wait_long
		lda #$1b
		sta $d011


; Main game loop
main_loop	jsr sync_wait

; Call the various subroutines that process the game
		jsr player_update
		jsr bullet_update

		jsr nasty_update

		jsr anim_update

		jsr scroll_update

		jsr bump_score
		jsr status_update

; Check for the player's death flag
		lda player_d_flag
		beq no_death

; Conditional assembly - if cheat is $01 then skip the player collisions
!if cheat=$01 {
		jmp no_death
}

; Set the player shield
		lda #$32
		sta player_shield

; Strobe the screen yellow on the next frame
		lda #$07
		sta d021_mirror

; Trigger the player explosion sound
		lda #<plyr_death_sfx
		ldy #>plyr_death_sfx
		ldx #$0e
		jsr music+$06

; Decrease and check player's lives counter
		lda player_lives
		sec
		sbc #$01
		sta player_lives

		bne *+$05
		jmp game_over_init

; Check to see if the end of map flag is set
no_death	lda map_flag
		cmp #$01
		bne game_done_init
		jmp main_loop


; Game completion
game_done_init	jsr status_update

; Select status chars for the main play area and disable sprites
		lda #$07
		sta d016_mirror
		sta d016_scrl_mr
		lda #$be
		sta d018_mirror

		lda #$00
		sta d015_mirror

; Completion text
		+print completion_1, $1c2, $12, $05
		+print completion_2, $216, $0a, $05

; Play the completion sound effect
		lda #<completion_sfx
		ldy #>completion_sfx
		ldx #$0e
		jsr music+$06

; Flash the completion message for a few seconds
		lda #$a0
		sta rt_store_1

		lda #$00
		sta flash_timer
		sta flash_state

game_done_loop	jsr sync_wait

		jsr gd_text_flash

		dec rt_store_1
		bne game_done_loop

; Wait for a fire press, then flip back to the titles
		+print fire_txt, $378, $16, $0c

game_done_loop2	jsr sync_wait

		jsr gd_text_flash

		lda $dc00
		and #$10
		bne game_done_loop2

		jmp titles_init

; Flash both of the completion messages
gd_text_flash	lda #$05
		ldx flash_state
		beq *+$04
		lda #$0d

		ldx #$00
gdf_loop_1	sta colour_ram+$1c2,x
		inx
		cpx #$14
		bne gdf_loop_1

		lda #$03
		ldx flash_state
		beq *+$04
		lda #$0e

		ldx #$00
gdf_loop_2	sta colour_ram+$216,x
		inx
		cpx #$0c
		bne gdf_loop_2

		rts


; Game over
game_over_init	jsr status_update

; Play the game over sound effect
		lda #<game_over_sfx
		ldy #>game_over_sfx
		ldx #$0e
		jsr music+$06

; Make the ship an explosion
		lda #$80
		sta sprite_dp
		lda #$86
		sta anim_start
		lda #$87
		sta anim_end

		lda #$80
		sta rt_store_1

; The game over loop, updates everything apart from the player
game_over_loop	jsr sync_wait

; Call the various subroutines that process the game
		jsr bullet_update

		jsr nasty_update

		jsr anim_update

		jsr scroll_update

		jsr bump_score
		jsr status_update

		dec rt_store_1
		bne game_over_loop

		jmp titles_init


; Scroller for the titles
t_scroller	ldx t_scrl_x
		inx
		cpx #$08
		bne t_no_new_char

; We need to move the message
		ldx #$00
t_mover		lda screen_ram+$371,x
		sta screen_ram+$370,x
		inx
		cpx #$26
		bne t_mover

; Read a new character
t_mread		ldy #$00
		lda (t_scrl_count),y
		cmp #$ff
		bne t_okay

		jsr t_scrl_reset
		jmp t_mread

; Write that character to the screen
t_okay		sta screen_ram+$396

; Bump the text counter to the next character
		inc t_scrl_count+$00
		bne *+$04
		inc t_scrl_count+$01

; Skip to here if a new character isn't needed
		ldx #$00
t_no_new_char	stx t_scrl_x

		txa
		eor #$07
		sta d016_scrl_mr

		rts

; Reset the scroll reader
t_scrl_reset	lda #<t_scrl_text
		sta t_scrl_count+$00
		lda #>t_scrl_text
		sta t_scrl_count+$01

		rts


; Everything to do with updating the player
player_update	lda $dc00
		sta ctrl_buffer

; Check for joystick up
pu_up		lda ctrl_buffer
		and #$01
		bne pu_down

		lda player_y
		sec
		sbc #$02
		cmp #$5a
		bcs *+$04
		lda #$5a
		sta player_y

; Check for joystick down
pu_down		lda ctrl_buffer
		and #$02
		bne pu_left

		lda player_y
		clc
		adc #$02
		cmp #$e6
		bcc *+$04
		lda #$e5
		sta player_y

; Check for joystick left
pu_left		lda ctrl_buffer
		and #$04
		bne pu_right

		lda player_x
		sec
		sbc #$01
		cmp #$10
		bcs *+$04
		lda #$10
		sta player_x

; Check for joystick right
pu_right	lda ctrl_buffer
		and #$08
		bne pu_fire

		lda player_x
		clc
		adc #$01
		cmp #$70
		bcc *+$04
		lda #$6f
		sta player_x

; Check for joystick fire
pu_fire		lda ctrl_buffer
		and #$10
		bne pu_bullet_out

; Launch the player bullet (if it's not busy)
		lda bullet_y
		cmp #$ff
		bne pu_bullet_out

; Set bullet X, Y and duration
		lda player_x
		sta bullet_x
		lda player_y
		sta bullet_y

		lda #$10
		sta bullet_duration

; Set the SFX driver if it's not busy
pu_bullet_out

; Player to nasty collision checks
; Offset the player position to work out if it's collided
		lda player_x
		sec
		sbc #$04
		sta coll_temp+$00
		clc
		adc #$09
		sta coll_temp+$01

		lda player_y
		sec
		sbc #$0a
		sta coll_temp+$02
		clc
		adc #$16
		sta coll_temp+$03

; Reset the death flag
		lda #$00
		sta player_d_flag

; Enemy to player collision check
		ldx #$00
pu_coll_loop	lda enemy_state,x
		beq pu_coll_skip

		lda enemy_1_x,x
		cmp coll_temp+$00
		bcc pu_coll_skip
		cmp coll_temp+$01
		bcs pu_coll_skip

		lda enemy_1_y,x
		cmp coll_temp+$02
		bcc pu_coll_skip
		cmp coll_temp+$03
		bcs pu_coll_skip

; Enemy has collided, so react accordingly
		lda #$00
		sta enemy_state,x

		lda #$80
		sta sprite_dp+$02,x
		lda #$86
		sta anim_start+$02,x
		lda #$87
		sta anim_end+$02,x

; Flag that the player has collided and exit the loop
		inc player_d_flag
		jmp pu_coll_out

; Finish the loop
pu_coll_skip	inx
		cpx #$06
		bne pu_coll_loop

; Update the player shield
pu_coll_out	lda player_shield
		beq pu_exit

; If the shield is on, decrease it and zero the death flag
		dec player_shield

		lda #$00
		sta player_d_flag

		ldy #$ff
		lda player_shield
		and #$01
		beq *+$04
		ldy #$fe
		sty d015_mirror

pu_exit		rts


; Update the player bullet's position and check for right border
bullet_update	lda bullet_x
		clc
		adc #$04
		sta bullet_x
		cmp #$d0
		bcs bu_remove

; Update the bullet's counter and check if it has expired
		dec bullet_duration
		beq bu_remove

		jmp bu_okay

; Remove the bullet
bu_remove	lda #$00
		sta bullet_x
		lda #$ff
		sta bullet_y

; Reset the score bonus
bu_okay		lda #$00
		sta score_bonus

; Bullet to nasty collision checks
		lda bullet_y
		cmp #$ff
		bne *+$05
		jmp bu_coll_exit

; Offset the bullet position to work out if it's collided
		sec
		sbc #$13
		sta coll_temp+$02
		clc
		adc #$26
		sta coll_temp+$03

		lda bullet_x
		sec
		sbc #$0b
		sta coll_temp+$00
		clc
		adc #$16
		sta coll_temp+$01

; Enemy to bullet collision check
		ldx #$00
bu_coll_loop	lda enemy_state,x
		beq bu_coll_skip

		lda enemy_1_x,x
		cmp coll_temp+$00
		bcc bu_coll_skip
		cmp coll_temp+$01
		bcs bu_coll_skip

		lda enemy_1_y,x
		cmp coll_temp+$02
		bcc bu_coll_skip
		cmp coll_temp+$03
		bcs bu_coll_skip

; Enemy has been shot, so react accordingly
		lda #$00
		sta enemy_state,x

		lda #$80
		sta sprite_dp+$02,x
		lda #$86
		sta anim_start+$02,x
		lda #$87
		sta anim_end+$02,x

		lda #$ff
		sta bullet_y

		lda score_bonus
		clc
		adc #$08
		sta score_bonus

; Strobe the screen orange on the next frame
		lda #$08
		sta d021_mirror

		jmp bu_coll_exit

; Finish the loop
bu_coll_skip	inx
		cpx #$06
		bne bu_coll_loop

; Jumped to if the bullet is inactive
bu_coll_exit	rts


; Update enemy positions
nasty_update	ldx #$00
nu_loop		lda enemy_state,x
		bne nu_process

; Check to see if the nasty has reached the end of an explosion
		lda sprite_dp+$02,x
		cmp #$86
		bne nu_splode_skip

		lda #$00
		sta enemy_1_x,x

		lda anim_start_dflt+$02,x
		sta sprite_dp+$02,x
		sta anim_start+$02,x

		lda anim_end_dflt+$02,x
		sta anim_end+$02,x

		lda #$01
		sta enemy_state,x

		jmp nu_nudge_skip

; Enemy is active, so update it's X and Y positions
nu_process	lda enemy_1_x,x
		sec
		sbc enemy_x_speeds,x
		sta enemy_1_x,x

		lda enemy_1_y,x
		clc
		adc enemy_y_speeds,x
		sta enemy_1_y,x

; Nudge the sprites around in Y if they're at X $fe/$ff
nu_splode_skip	lda enemy_1_x,x
		cmp #$fe
		bcc nu_nudge_skip

		lda enemy_1_y,x
		clc
		adc enemy_y_nudges,x
		sta enemy_1_y,x

nu_nudge_skip	inx
		cpx #$06
		bne nu_loop


; Make sure none of the X positions are over $cf
		ldx #$00
nu_x_cf_clip	lda enemy_1_x,x
		cmp #$cf
		bcc nu_x_cf_skip

		lda enemy_1_y,x
		sec
		sbc #$43
		sta enemy_1_y,x

		lda #$cf
nu_x_cf_skip	sta enemy_1_x,x
		inx
		cpx #$06
		bne nu_x_cf_clip

; Check to see if a nasty is exploding to trigger a sound
		lda score_bonus
		beq nu_exit

		lda #<enemy_death_sfx
		ldy #>enemy_death_sfx
		ldx #$0e
		jsr music+$06

nu_exit

; Find out if an enemy speed change is due
		dec wave_timer
		bne eu_wt_okay

; It's time, so get the length of this wave ($00 means reset the wave engine)
eu_wave_fetch	ldy #$00
eu_wt_fetch	lda (wave_read),y
		bne eu_wf_okay
		jsr wave_reset
		jmp eu_wt_fetch

eu_wf_okay	sta wave_timer
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

; Fetch twelve bytes of enemy speed data
		ldx #$00
		ldy #$00
eu_wf_loop	lda (wave_read),y
		sta enemy_x_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		lda (wave_read),y
		sta enemy_y_speeds,x
		inc wave_read+$00
		bne *+$04
		inc wave_read+$01

		inx
		cpx #$06
		bne eu_wf_loop

eu_wt_okay	rts

; Reset the attack wave reader
wave_reset	lda #<wave_data
		sta wave_read+$00
		lda #>wave_data
		sta wave_read+$01
		rts


; Animate the sprites
anim_update	ldx anim_timer
		dex
		cpx #$ff
		bne au_exit

		ldx #$00
au_loop		lda sprite_dp,x
		clc
		adc #$01
		cmp anim_end,x
		bne au_skip
		lda anim_start,x
au_skip		sta sprite_dp,x
		inx
		cpx #$08
		bne au_loop

		ldx #$03
au_exit		stx anim_timer
		rts


; Set up the screen
screen_init

; Zero the screen and colour RAM
		ldx #$00
		txa
screen_clear	sta screen_ram+$000,x
		sta screen_ram+$100,x
		sta screen_ram+$200,x
		sta screen_ram+$2e8,x

		sta colour_ram+$000,x
		sta colour_ram+$100,x
		sta colour_ram+$200,x
		sta colour_ram+$2e8,x
		inx
		bne screen_clear

; Draw a mask across the screen where the status bar ends and the
; play area starts
		ldx #$00
mask_draw	lda #$ff
		sta screen_ram+$0a0,x
		lda #$00
		sta colour_ram+$0a0,x
		inx
		cpx #$28
		bne mask_draw

; Debounce the fire button (screen is blank at this point)
fire_debounce	lda $dc00
		and #$10
		beq fire_debounce

; Status text
		+print status_text_1, $029, $05, $08
		+print status_text_3, $048, $05, $04


; Status bar logo
		+print status_logo_1, $030, $16, $0d
		+print status_logo_2, $058, $16, $05

; Update the status area (called from multiple places)
status_update

; Update the score counter before it's rendered
		ldx #$00
su_score_upd	lda player_score,x
		clc
		adc #$30
		sta status_text_2,x
		inx
		cpx #$05
		bne su_score_upd

; Update the lives counter before it's rendered
		lda player_lives
		clc
		adc #$30
		sta status_text_4

; Plot the score counter onto the status bar
		+print status_text_2, $051, $05, $0a

; Plot the lives counter onto the status bar (no point in using
; the print macro for one character!)
		lda status_text_4
		sta screen_ram+$074

		lda #$0e
		sta colour_ram+$074

		rts

; Add to the player's score
bump_score	lda score_bonus
		beq bs_exit
		tay

bs_outer_loop	ldx #$03
bs_loop		lda player_score,x
		clc
		adc #$01
		sta player_score,x

		cmp #$0a
		bcc bs_skip

		lda #$00
		sta player_score,x

		dex
		cpx #$ff
		bne bs_loop

bs_skip		dey
		bne bs_outer_loop

bs_exit		rts


; Bring in the background scroller
		!src "includes\scroll_code.asm"


; Wait for near the end of the screen (rout4 is the trigger point)
sync_wait	lda #$00
		sta sync

sw_loop		cmp sync
		beq sw_loop
		rts

; Longer delay - the Y register says how long to wait
sync_wait_long	jsr sync_wait
		dey
		bne sync_wait_long

		rts


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num

		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3

		cmp #$04
		bne *+$05
		jmp irq_rout4


; Raster split 1
irq_rout1

; Status bar video registers
		lda #$00
		sta $d020
		lda #$0b
		sta $d021

		lda #$07
		sta $d016
		lda #$be
		sta $d018

; And the current multicolours for the play area
		ldx level_col_count
		lda d022_values,x
		sta $d022
		lda d023_values,x
		sta $d023

; Set up and position the hardware sprites
		lda d015_mirror
		sta $d015
		sta $d01b
		sta $d01c

; Set sprite X and Y positions
		ldx #$00
		ldy #$00
xploder_1	lda sprite_x,x
		asl
		ror $d010
		sta $d000,y
		lda sprite_y,x
		sta $d001,y
		iny
		iny
		inx
		cpx #$08
		bne xploder_1

; Set sprite colours and aim their data pointers at a blank sprite
		ldx #$00
xploder_2	lda sprite_col,x
		sta $d027,x
		lda #$86
		sta screen_ram+$3f8,x
		inx
		cpx #$08
		bne xploder_2

		lda #$0b
		sta $D025
		lda #$01
		sta $d026

; Update the flash timer
		ldx flash_timer
		inx
		cpx #$10
		bcc ft_xb

		lda flash_state
		clc
		adc #$01
		and #$01
		sta flash_state

		ldx #$00
ft_xb		stx flash_timer

; Play the music
		jsr music+$03

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2

; Screen to black, hides any potential glitches when $d016 changes
		lda #$00
		sta $d021

; Set scroll register and select the character set/screen RAM
		lda d016_mirror
		sta $d016
		lda d018_mirror
		sta $d018

; Set the background colour and...
		lda d021_mirror
		sta $d021

; ...zero the mirror so strobed colours go to black after a frame
		lda #$00
		sta d021_mirror

; Copy the sprite data pointers
		ldx #$00
sdp_copy	lda sprite_dp,x
		sta screen_ram+$3f8,x
		inx
		cpx #$08
		bne sdp_copy

; Set interrupt handler for split 3
		lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
irq_rout3

; Set the scroll register for the title scroller
		lda d016_scrl_mr
		sta $d016

; Set interrupt handler for split 4
		lda #$04
		sta raster_num
		lda #raster_4_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 4
irq_rout4	lda #$01
		sta sync

; Set the scroll register for the playfield
		lda d016_mirror
		sta $d016

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012

; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Labels for player status
player_score	!byte $00,$06,$05,$01,$00
player_lives	!byte $03

score_bonus	!byte $00

player_shield	!byte $00
player_d_flag	!byte $00

; Default sprite values
sprite_x_dflt	!byte $00,$00,$20,$34,$5b,$91,$6f,$78
sprite_y_dflt	!byte $00,$00,$ae,$56,$c1,$75,$9c,$41
anim_start_dflt	!byte $87,$8f,$90,$98,$a0,$90,$98,$a0
anim_end_dflt	!byte $8f,$90,$98,$a0,$ad,$98,$a0,$ad

; Player, bullet and enemy sprite X positions
sprite_x

player_x	!byte $00
bullet_x	!byte $00

enemy_1_x	!byte $00
enemy_2_x	!byte $00
enemy_3_x	!byte $00
enemy_4_x	!byte $00
enemy_5_x	!byte $00
enemy_6_x	!byte $00

; Player, bullet and enemy sprite X positions
sprite_y

player_y	!byte $00
bullet_y	!byte $00

enemy_1_y	!byte $00
enemy_2_y	!byte $00
enemy_3_y	!byte $00
enemy_4_y	!byte $00
enemy_5_y	!byte $00
enemy_6_y	!byte $00

; Sprite colour and data pointers
sprite_col	!byte $0e,$07,$0d,$03,$05,$0e,$04,$0a
sprite_dp	!byte $87,$8f,$90,$98,$a0,$90,$98,$a0

; Sprite animation start and end positions
anim_start	!byte $87,$8f,$90,$98,$a0,$90,$98,$a0
anim_end	!byte $8f,$90,$98,$a0,$ad,$98,$a0,$ad

; Enemy state counters - $00 means exploding, $01 is active
enemy_state	!byte $00,$00,$00,$00,$00,$00

; Enemy sprite X and Y movement speeds
enemy_x_speeds	!byte $00,$00,$00,$00,$00,$00
enemy_y_speeds	!byte $00,$00,$00,$00,$00,$00

; Bring in the attack wave data
wave_data	!src "includes\waves.asm"


; Nudge values for when an enemy wraps around
enemy_y_nudges	!byte $30,$cd,$1d,$dd,$4a,$97

; Lifespan counter for the bullet
bullet_duration	!byte $00


; Columm buffer for the background scroller
column_buffer	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00


; Status bar logo
status_logo_1	!byte $60,$61,$62,$63,$64,$65,$66,$67
		!byte $68,$69,$6a,$6b,$66,$67,$64,$65
		!byte $00,$00,$6c,$6d,$23,$6b

status_logo_2	!byte $70,$71,$72,$73,$74,$75,$76,$77
		!byte $78,$79,$7a,$7b,$76,$77,$74,$75
		!byte $00,$00,$7c,$7d,$24,$7b

; Status bar text
status_text_1	!scr "Score"
status_text_2	!scr "     "
status_text_3	!scr "Lives"
status_text_4	!scr " "


; Titles logo data - Stercore
t_logo_1	!byte $6e,$5c,$5c,$5d,$5b,$5e,$5d,$5d
		!byte $6e,$5c,$5c,$6f,$6e,$5c,$5c,$6e
		!byte $5c,$5c,$6f,$6e,$5c,$5c,$6f,$6e
		!byte $5c,$5c,$6e,$5c,$5c,$6f

t_logo_2	!byte $7e,$5c,$5c,$6f,$20,$1c,$20,$20
		!byte $1e,$5c,$5c,$1f,$1c,$20,$20,$1c
		!byte $20,$20,$00,$1c,$20,$20,$1c,$1c
		!byte $20,$20,$1e,$5c,$5c,$1f

t_logo_3	!byte $5f,$5c,$5c,$7f,$20,$1d,$20,$20
		!byte $7e,$5c,$5c,$5f,$1d,$20,$20,$7e
		!byte $5c,$5c,$7f,$7e,$5c,$5c,$7f,$1d
		!byte $20,$20,$7e,$5c,$5c,$5f

; Titles logo data - XD
t_logo_4	!byte $1b,$00,$00,$1b,$3c,$5c,$5c,$6f

t_logo_5	!byte $2a,$5c,$5c,$40,$1c,$00,$00,$1c

t_logo_6	!byte $1d,$00,$00,$1d,$5f,$5c,$5c,$7f

; Titles logo effect colour data
t_colour_data	!byte $0d,$0d,$0d,$0c,$0d,$0c,$0c,$0c
		!byte $0b,$0c,$0b,$0b,$0b,$0a,$0b,$0a
		!byte $0a,$0a,$09,$0a,$09,$09,$09,$00
		!byte $09,$00,$00,$00,$01,$00,$01,$01
		!byte $01,$02,$01,$02,$02,$02,$03,$02
		!byte $03,$03,$03,$04,$03,$04,$04,$04
		!byte $05,$04,$05,$05,$05,$0d,$05

; Titles logo effect luma data
t_luma_data	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00,$01,$00,$00
		!byte $01,$00,$01,$01,$00,$01,$01,$01
		!byte $01,$02,$01,$01,$02,$01,$02,$02
		!byte $01,$02,$02,$02,$02,$01,$02,$02
		!byte $01,$02,$01,$02,$01,$01,$01,$01
		!byte $00,$01,$01,$00,$01,$00,$00,$01

; Titles logo colour translation data
t_colour_trans	!byte $00,$06,$0b,$04,$0e,$03,$0d,$01
		!byte $00,$09,$02,$08,$0a,$0f,$07,$01

; Titles text
t_credit_1	!scr "Design, coding, graphics, sounds, data"
t_credit_2	!scr "wrangling and other gubbins by"
t_credit_3	!scr "Jason"

t_credit_4	!scr "A C64CD Studios Production in 2019"
t_credit_5	!scr "Converted and reworked on the C64 from"
t_credit_6	!scr "the Spectrum CSSCGC 2018 entry"

t_text_4	!scr "Press Fire To Start!"

; Titles scroller text
t_scrl_text	!scr "Hello and welcome to"
		!scr "        "

		!scr "-=- STERCORE XD -=-"
		!scr "        "


		!scr "A scrolling shoot 'em up from the C64CD "
		!scr "software mines, originally as a CSSCGC "
		!scr "2018 release for the Sinclair ZX Spectrum "
		!scr "and reworked on the C64 in 2019 for the "
		!scr "RGCD 16K compeition."
		!scr "        "

		!scr "All the development-y type thingies done "
		!scr "by Jason"
		!scr "        "

		!scr "Launch your battered spaceship and fly "
		!scr "headlong into a brightly coloured world "
		!scr "filled with dumb enemies that need to "
		!scr "be obliterated for... reasons?"
		!scr "        "

		!scr "It's a shoot 'em up, nobody really reads "
		!scr "the instructions for these things so just "
		!scr "start the game and go cause havoc already!"
		!scr "        "

		!scr "100 percent machine washable (apart from "
		!scr "the bits that aren't) - this game is "
		!scr "fully compatible with TheC64 Mini and "
		!scr "C64DTV2...  probably?"
		!scr "        "

		!scr "C64CD greetings blast out towards:  "
		!scr "1001 Crew, "
		!scr "Ash And Dave, "
		!scr "Black Bag, "
		!scr "Copy Service Stuttgart, "
		!scr "Borderzone Dezign Team, "
		!scr "Dynamic Duo, "

		!scr "Four Horsemen Of The Apocalypse, "
		!scr "Happy Demomaker, "
		!scr "Harlow Cracking Service, "
		!scr "High-tech Team, "
		!scr "Ikari, "
		!scr "Jewels, "

		!scr "Kernal, "
		!scr "Laxity, "
		!scr "Mean Team, "
		!scr "Paul, Shandor and Matt, "
		!scr "Pulse Productions, "
		!scr "Reset 86, "

		!scr "Rob Hubbard, "
		!scr "Scoop, "
		!scr "Slipstream, "
		!scr "Stoat And Tim, "
		!scr "Tangent, "
		!scr "Thalamus, "

		!scr "The Commandos, "
		!scr "The GPS, "
		!scr "The Six Pack, "
		!scr "We Music, "
		!scr "Xess, "
		!scr "Yak, "

		!scr "and Yeti Factories."
		!scr "      "

		!scr "And of course the now traditional anti-greeting "
		!scr "to C64hater because we might as well whilst "
		!scr "here..."
		!scr "        "

		!scr "And that's everything sorted, so here we are "
		!scr "signing off on 2019-06-01 - goodbye for now "
		!scr "and enjoy shooting stuff... .. .  ."
		!scr "        "

		!scr $ff		; end of text marker

; Colours for the titles scroller
t_scrl_c_data	!byte $09,$09,$02,$09,$02,$02,$08,$02
		!byte $08,$08,$0a,$08,$0a,$0a,$0f,$0a
		!byte $0f,$0f,$0f,$0f


; Completion text
completion_1	!scr "Mission Completed!"
completion_2	!scr "Well Done!"

; Press Fire prompt text
fire_txt	!scr "Press Fire To Continue"


; Sound effect data
plyr_death_sfx	!byte $00,$fb,$08,$b8,$81

		!byte $a4,$41,$a0,$b4,$81,$98,$92,$9c
		!byte $90,$95,$9e,$92,$80,$94,$8f,$8e
		!byte $8d,$8c,$8d,$8e,$8f,$8e,$8d,$8c
		!byte $8d,$9e,$92,$80,$94,$8f,$8e,$8d
		!byte $8c,$8d,$8e,$8f,$8e,$8d,$8c,$8d
		!byte $00

enemy_death_sfx	!byte $00,$fa,$08,$b8,$81

		!byte $a4,$41,$a8,$bc,$81,$a0,$9a,$a4
		!byte $98,$9d,$a6,$9a,$80,$9c,$97,$96
		!byte $97,$94,$95,$96,$97,$96,$95,$94
		!byte $95,$00

game_over_sfx	!byte $0b,$00,$02,$a9,$21

		!byte $b9,$b9,$a4,$a4,$b4,$b4,$b0,$b0
		!byte $b0,$b0,$a9,$a9,$b9,$b9,$a4,$a4
		!byte $b4,$b4,$a0,$a0,$a8,$a8,$b0,$b0
		!byte $a9,$a9,$94,$94,$a4,$a4,$a0,$a0
		!byte $a0,$a0,$99,$99,$a9,$a9,$94,$94
		!byte $a4,$a4,$90,$90,$a8,$a8,$b0,$b0
		!byte $99,$99,$84,$84,$94,$94,$90,$90
		!byte $90,$90,$89,$89,$99,$99,$84,$84
		!byte $94,$94,$80,$80,$a8,$a8,$b0,$b0

		!byte $b9,$b9,$a4,$a4,$b4,$b4,$b0,$b0
		!byte $b0,$b0,$a9,$a9,$b9,$b9,$a4,$a4
		!byte $b4,$b4,$a0,$a0,$a8,$a8,$b0,$b0
		!byte $a9,$a9,$94,$94,$a4,$a4,$a0,$a0
		!byte $a0,$a0,$99,$99,$a9,$a9,$94,$94
		!byte $a4,$a4,$90,$90,$a8,$a8,$b0,$b0
		!byte $99,$99,$84,$84,$94,$94,$90,$90
		!byte $90,$90,$89,$89,$99,$99,$84,$84
		!byte $94,$94,$80,$80,$a8,$a8,$b0,$b0

		!byte $00

completion_sfx	!byte $0c,$00,$06,$b0,$41

		!byte $a0,$a0,$a0,$a0,$b0,$b0,$b0,$b0
		!byte $a4,$a4,$a4,$a4,$b4,$b4,$b4,$b4
		!byte $a9,$a9,$a9,$a9,$b9,$b9,$b9,$b9
		!byte $a4,$a4,$a4,$a4,$b4,$b4,$b4,$b4
		!byte $b2,$b2,$b2,$b2,$b2,$b2,$b2,$b2
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6
		!byte $bb,$bb,$bb,$bb,$bb,$bb,$bb,$bb
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6

		!byte $b4,$b4,$b4,$b4,$b4,$b4,$b4,$b4
		!byte $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
		!byte $bd,$bd,$bd,$bd,$bd,$bd,$bd,$bd
		!byte $b8,$b8,$b8,$b8,$b8,$b8,$b8,$b8
		!byte $b6,$b6,$b6,$b6,$b6,$b6,$b6,$b6
		!byte $ba,$ba,$ba,$ba,$ba,$ba,$ba,$ba
		!byte $bf,$bf,$bf,$bf,$bf,$bf,$bf,$bf
		!byte $ba,$ba,$ba,$ba,$ba,$ba,$ba,$ba

		!byte $00


; Multicolour values for the background
d022_values	!byte $09,$02,$0b,$06,$09,$0b
d023_values	!byte $0c,$0a,$0c,$0e,$05,$0c

; Background tiles data
tile_data	!binary "binary\background.til"

tile_col_data	!binary "binary\background.col"

; Background map data
level_data	!binary "binary\background.map"
		!byte $ff,$00,$00,$00,$00	; end of data marker
		!byte $00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00
		!byte $00,$00,$00,$00,$00
