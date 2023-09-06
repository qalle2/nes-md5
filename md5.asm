; Qalle's MD5 Hasher (NES, ASM6)

; For testing:
; md5("")                         = d41d 8cd9 8f00 b204 e980 0998 ecf8 427e
; md5(01 23 45 67 89 ab cd ef fe) = ad7e 4f41 8d7f 2364 5ab8 1d42 c082 53f0

; --- Constants ---------------------------------------------------------------

; Notes:
; - message = bytestring to hash
; - "dword" = 32-bit little endian integer
; - mode:
;     0 = editing
;     1 = computing and printing 1st line of hash
;     2 = printing 2nd line of hash

; main RAM
msg_bytes       equ $00    ; message/chunk as bytes (64 bytes, 16 dwords)
state           equ $40    ; MD5 state/hash (16 bytes, split in 4 dwords)
state0          equ $40
state1          equ $44
state2          equ $48
state3          equ $4c
msg_digits      equ $50    ; message as hexadecimal digits (20 bytes)
ppu_buffer      equ $64    ; data to copy to PPU on next VBlank (24 bytes)
tempdw          equ $7c    ; temporary dword (4 bytes)
msg_len_digits  equ $80    ; message length in digits
msg_len_bytes   equ $81    ; message length in bytes
run_main_loop   equ $82    ; is main loop allowed to run? ($80-$ff = yes)
pad_status      equ $83    ; joypad status
prev_pad_status equ $84    ; previous joypad status
cursor_pos      equ $85    ; cursor position in hexadecimal digits
mode            equ $86    ; program mode (see above)
ppu_buf_adr_hi  equ $87    ; PPU buffer - high byte of address
ppu_buf_adr_lo  equ $88    ; PPU buffer - low  byte of address
ppu_buf_length  equ $89    ; PPU buffer - length
round           equ $8a    ; MD5 round
temp            equ $8b    ; temporary byte
sprite_data     equ $0200  ; OAM page ($100 bytes)
chunk_indexes   equ $0300  ; chunk index table ($40 bytes)

; memory-mapped registers
ppu_ctrl        equ $2000
ppu_mask        equ $2001
ppu_status      equ $2002
oam_addr        equ $2003
ppu_scroll      equ $2005
ppu_addr        equ $2006
ppu_data        equ $2007
dmc_freq        equ $4010
oam_dma         equ $4014
snd_chn         equ $4015
joypad1         equ $4016
joypad2         equ $4017

; PPU memory space
ppu_nt0         equ $2000
ppu_at0         equ $23c0
ppu_palette     equ $3f00

; colors
COL_BG          equ $0f  ; background (black)
COL_MAIN        equ $30  ; main text (white)
COL_HELP        equ $24  ; help text (pink)
COL_INPUT       equ $27  ; cursor/message (yellow)
COL_OUTPUT      equ $21  ; hash (blue)
COL_UNUSED      equ $00  ; unused (gray)

; ASCII codes
ASCII_SPACE     equ $20
ASCII_HYPHEN    equ $2d
ASCII_ZERO      equ $30
ASCII_A         equ $41

; misc constants
MAX_MSG_BYTES   equ 9    ; maximum length of message in bytes
MD5_TERMINATOR  equ $80  ; first byte of padding

; --- Macros ------------------------------------------------------------------

macro be_word _word
                ; big-endian word (high byte first)
                db >(_word), <(_word)
endm

; --- iNES header -------------------------------------------------------------

                ; see https://wiki.nesdev.org/w/index.php/INES
                base $0000
                db "NES", $1a            ; file id
                db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
                db %00000000, %00000000  ; NROM mapper, horiz. NT mirroring
                pad $0010, $00           ; unused

; --- Interrupt routines (here for page alignment) ----------------------------

                base $c000              ; last 16 KiB of CPU address space
                pad $f800, $ff          ; only use last 2 KiB

nmi             pha                     ; push A, X, Y
                txa
                pha
                tya
                pha

                bit ppu_status          ; reset ppu_scroll/ppu_addr latch

                lda #$00                ; do sprite DMA
                sta oam_addr
                lda #>sprite_data
                sta oam_dma

                jsr flush_ppu_buf       ; flush PPU buffer

                sec                     ; set flag to let main loop run once
                ror run_main_loop

                jsr set_ppu_regs        ; set PPU registers

                pla                     ; pull Y, X, A
                tay
                pla
                tax
                pla

irq             rti                     ; IRQ unused

flush_ppu_buf   ; copy PPU buffer to PPU and empty it

                ldy ppu_buf_length
                beq +                   ; skip if empty

                lda ppu_buf_adr_hi
                sta ppu_addr
                lda ppu_buf_adr_lo
                sta ppu_addr

                ldx #0
-               lda ppu_buffer,x
                sta ppu_data
                inx
                dey
                bne -

                lda #0                  ; reset length
                sta ppu_buf_length

+               rts

set_ppu_regs    lda #0                  ; set scroll
                sta ppu_scroll
                sta ppu_scroll
                lda #%00011110          ; show background and sprites
                sta ppu_mask
                lda #%10000000          ; enable NMI (do this last)
                sta ppu_ctrl
                rts

; --- Initialization ----------------------------------------------------------

reset           ; initialize the NES
                ; see https://wiki.nesdev.org/w/index.php/Init_code
                sei                     ; ignore IRQs
                cld                     ; disable decimal mode
                ldx #%01000000
                stx joypad2             ; disable APU frame IRQ
                ldx #$ff
                txs                     ; initialize stack pointer
                inx
                stx ppu_ctrl            ; disable NMI
                stx ppu_mask            ; disable rendering
                stx dmc_freq            ; disable DMC IRQs
                stx snd_chn             ; disable sound channels

                jsr wait_vbl_start      ; wait until next VBlank starts
                jsr init_ram            ; initialize main RAM

                jsr wait_vbl_start      ; wait until next VBlank starts
                jsr init_ppu_mem        ; initialize PPU memory

                jsr wait_vbl_start      ; wait until next VBlank starts
                jsr set_ppu_regs        ; set PPU registers
                jmp main_loop           ; start main program

wait_vbl_start  bit ppu_status          ; wait until next VBlank starts
-               lda ppu_status
                bpl -
                rts

init_ram        ; initialize main RAM

                ; clear zero page
                lda #$00
                tax
-               sta $00,x
                inx
                bne -

                ; hide all sprites by setting Y coordinates to $ff
                lda #$ff
                ldx #0
-               sta sprite_data,x
                inx
                inx
                inx
                inx
                bne -

                ; copy initial sprite data
                ldx #0
-               lda init_spr_data,x
                sta sprite_data,x
                inx
                cpx #(1*4)
                bne -

                ; set nonzero variables
                lda #(4*2)
                sta msg_len_digits

                ; generate chunk index table (64 bytes, 4 * 16-byte subtable):
                ;   0 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
                ;   1 6 11  0  5 10 15  4  9 14  3  8 13  2  7 12
                ;   5 8 11 14  1  4  7 10 13  0  3  6  9 12 15  2
                ;   0 7 14  5 12  3 10  1  8 15  6 13  4 11  2  9
                ; how to generate:
                ;   indexes 15- 0: start from 15, sub 1, AND 0x0f, repeat
                ;   indexes 31-16: start from 12, sub 5, AND 0x0f, repeat
                ;   indexes 47-32: start from  2, sub 3, AND 0x0f, repeat
                ;   indexes 63-48: start from  9, sub 7, AND 0x0f, repeat
                ;
                ldy #0                  ; which subtable (0-3)
                ;
--              ldx ch_ind_ind,y        ; target index (predecremented)
                lda ch_ind_last_val,y   ; value to write
                pha
                ;
-               dex                     ; subtable loop
                pla
                sta chunk_indexes,x
                sec
                sbc ch_ind_sub_val,y
                and #%00001111
                pha
                txa
                and #%00001111
                bne -
                ;
                pla
                iny
                cpy #4
                bne --

                rts

init_spr_data   ; initial sprite data (Y, tile, attributes, X)
                db 18*8-1, "^", %00000000, 4*8  ; cursor (up arrow)

                ; for each 16-byte chunk index subtable
ch_ind_ind      db 16, 32, 48, 64       ; last target index + 1
ch_ind_last_val db 15, 12,  2,  9       ; last value
ch_ind_sub_val  db  1,  5,  3,  7       ; value to subtract for prev. value

init_ppu_mem    ; initialize PPU memory

                ; set palette (while still in VBlank)
                ;
                ldy #>ppu_palette
                lda #<ppu_palette
                jsr set_ppu_addr        ; Y*$100+A -> address
                ;
                ldx #0
-               lda palette,x
                sta ppu_data
                inx
                cpx #(palette_end-palette)
                bne -

                ; clear NT0 & AT0 ($400 bytes)
                ;
                ldy #>ppu_nt0
                lda #<ppu_nt0
                jsr set_ppu_addr        ; Y*$100+A -> address
                ;
                lda #$00
                ldy #4
                tax
-               sta ppu_data
                inx
                bne -
                dey
                bne -

                ; print NT/AT strings
                ;
                ldx #$ff
--              inx
                lda strings,x           ; address high / end of all strings
                beq +
                sta ppu_addr
                inx
                lda strings,x           ; address low
                sta ppu_addr
                ;
-               inx
                lda strings,x
                beq --                  ; end of string
                sta ppu_data
                jmp -

+               rts

set_ppu_addr    sty ppu_addr            ; Y*$100+A -> address
                sta ppu_addr
                rts

palette         db COL_BG, COL_MAIN,   COL_UNUSED, COL_UNUSED
                db COL_BG, COL_HELP,   COL_UNUSED, COL_UNUSED
                db COL_BG, COL_INPUT,  COL_UNUSED, COL_UNUSED
                db COL_BG, COL_OUTPUT, COL_UNUSED, COL_UNUSED
                db COL_BG, COL_INPUT
palette_end

strings         ; for each: PPU address high/low, bytes, terminator (zero)
                ; after all strings: terminator (negative)

                ; NT0

                be_word ppu_nt0+5*32+7
                db "QALLE'S MD5 HASHER", 0

                be_word ppu_nt0+8*32+7
                db "[ \\  MOVE CURSOR", 0
                be_word ppu_nt0+9*32+7
                db "] ^  DIGIT -1/+1", 0
                be_word ppu_nt0+10*32+4
                db "SELECT  DIGIT +8", 0
                be_word ppu_nt0+11*32+7
                db "B A  LENGTH -1/+1", 0
                be_word ppu_nt0+12*32+5
                db "START  COMPUTE HASH", 0

                be_word ppu_nt0+15*32+12
                db "MESSAGE:", 0

                be_word ppu_nt0+20*32+13
                db "HASH:", 0
                be_word ppu_nt0+22*32+6
                db "---- ---- ---- ----", 0
                be_word ppu_nt0+24*32+6
                db "---- ---- ---- ----", 0

                ; AT0

                ; names of buttons in help text
                be_word ppu_at0+2*8+1
                db %01010101, %01010101, 0
                be_word ppu_at0+3*8+1
                db %01010101, %01010101, 0

                ; message
                be_word ppu_at0+4*8+0
                db %10101010, %10101010, %10101010, %10101010, %10101010
                db %10101010, %10101010, %10101010, 0

                ; hash
                be_word ppu_at0+5*8+1
                db %11110000, %11110000, %11110000, %11110000
                db %11110000, %11110000, 0
                be_word ppu_at0+6*8+1
                db %00001111, %00001111, %00001111, %00001111
                db %00001111, %00001111, 0

                db 0                    ; end all strings

if $ - strings > $100
                error "out of string space"
endif

; --- Main loop - common ------------------------------------------------------

main_loop       bit run_main_loop       ; wait until NMI routine sets flag
                bpl main_loop

                lda mode                ; run sub for the mode we're in
                jsr jump_engine         ; A = sub index (0-2)

                ldx mode                ; update cursor tile
                lda mode2crsr_tile,x
                sta sprite_data+0*4+1

                ldx cursor_pos          ; update cursor X
                lda crsr_pos_to_x,x
                sta sprite_data+0*4+3

                ; clear flag (must be done after writing PPU buffer to ensure
                ; it gets flushed before being written again)
                lsr run_main_loop

                jmp main_loop           ; restart main loop

mode2crsr_tile  ; program mode to cursor sprite tile
                db "^@@"                ; up arrow, clock, clock

crsr_pos_to_x   ; hexadecimal digit index to cursor sprite X
                db  3*8,  4*8
                db  6*8,  7*8
                db  9*8, 10*8
                db 12*8, 13*8
                db 15*8, 16*8
                db 18*8, 19*8
                db 21*8, 22*8
                db 24*8, 25*8
                db 27*8, 28*8

; --- Main loop - mode 0 ------------------------------------------------------

main_mode0      lda pad_status          ; store previous joypad status
                sta prev_pad_status
                jsr read_joypad         ; read joypad
                jsr button_handler      ; handle buttons
                jsr update_msg          ; update message via PPU buffer
                rts

read_joypad     ; read 1st joypad or Famicom expansion port controller
                ; see https://www.nesdev.org/wiki/Controller_reading_code
                ; bits: A, B, select, start, up, down, left, right

                lda #1
                sta joypad1
                sta pad_status
                lsr a
                sta joypad1

-               lda joypad1
                and #%00000011
                cmp #1
                rol pad_status
                bcc -
                rts

button_handler  ; exit if something was pressed on last frame
                lda prev_pad_status
                bne +
                ;
                lda pad_status
                bmi inc_msg_len         ; A
                asl a
                bmi dec_msg_len         ; B
                asl a
                bmi inc_digit_by8       ; select
                asl a
                bmi start_hashing       ; start
                asl a
                bmi inc_digit           ; up
                asl a
                bmi dec_digit           ; down
                asl a
                bmi cursor_left         ; left
                bne cursor_right        ; right
+               rts

inc_msg_len     lda msg_len_digits
                cmp #(MAX_MSG_BYTES*2)
                beq +
                inc msg_len_digits
                inc msg_len_digits
+               rts

dec_msg_len     lda msg_len_digits
                beq +
                dec msg_len_digits
                dec msg_len_digits
                ;
                ; cursor: pos -= 2 if pos >= 2 and pos >= digit count
                lda cursor_pos
                cmp #2
                bcc +
                cmp msg_len_digits
                bcc +
                dec cursor_pos
                dec cursor_pos
                ;
+               rts

inc_digit_by8   lda #8
                jmp +
                ;
inc_digit       lda #1
                jmp +
                ;
dec_digit       lda #$ff
+               ldx cursor_pos
                clc
                adc msg_digits,x
                and #%00001111
                sta msg_digits,x
                rts

cursor_left     lda msg_len_digits      ; prevent if no digits
                beq +
                dec cursor_pos
                bpl +
                lda msg_len_digits
                sec
                sbc #1
                sta cursor_pos
+               rts

cursor_right    lda msg_len_digits      ; prevent if no digits
                beq +
                inc cursor_pos
                lda cursor_pos
                cmp msg_len_digits
                bcc +
                lda #0
                sta cursor_pos
+               rts

start_hashing   inc mode                ; to mode 1
                rts

update_msg      ; update message via PPU buffer

                lda #>(ppu_nt0+17*32+3)
                sta ppu_buf_adr_hi
                lda #<(ppu_nt0+17*32+3)
                sta ppu_buf_adr_lo

                ; write "-- -- "... to buffer
                lda #ASCII_HYPHEN
                ldy #ASCII_SPACE
                ldx #0                  ; target index
-               sta ppu_buffer,x
                inx
                sta ppu_buffer,x
                inx
                sty ppu_buffer,x
                inx
                cpx #(MAX_MSG_BYTES*3)
                bne -

                lda msg_len_digits      ; exit if no actual digits
                beq +

                ; overwrite start of buffer with actual digits: "hh hh "...
                ldx #0                  ; source index
                ldy #0                  ; target index
-               jsr msg_digit2buf
                jsr msg_digit2buf
                iny                     ; skip " " already in buffer
                cpx msg_len_digits
                bne -

+               lda #(MAX_MSG_BYTES*3)  ; length must be set last
                sta ppu_buf_length

                rts

msg_digit2buf   ; copy message digit to PPU buffer
                lda msg_digits,x
                jsr digit_to_ascii
                sta ppu_buffer,y
                inx
                iny
                rts

; --- Main loop - mode 1 ------------------------------------------------------

main_mode1      jsr prepare_msg         ; convert to bytes and pad
                jsr hash_msg

                ; update 1st line of hash via PPU buffer
                ldy #>(ppu_nt0+22*32+6)  ; PPU address high
                lda #<(ppu_nt0+22*32+6)  ; PPU address low
                ldx #0                  ; source index
                jsr upd_hash_line

                inc mode                ; to mode 2
                rts

prepare_msg     ; convert message from hexadecimal digits to bytes and pad it

                ; clear padded message
                lda #$00
                ldx #63
-               sta msg_bytes,x
                dex
                bpl -

                lda msg_len_digits      ; get original length in bytes
                lsr a
                sta msg_len_bytes
                beq +                   ; skip conversion if empty

                ; convert message from hexadecimal digits to bytes
                ldx #0                  ; source index
                ldy #0                  ; target index
-               lda msg_digits,x
                inx
                asl a
                asl a
                asl a
                asl a
                ora msg_digits,x
                inx
                sta msg_bytes,y
                iny
                cpy msg_len_bytes
                bne -

+               lda #MD5_TERMINATOR     ; append terminator
                ldx msg_len_bytes
                sta msg_bytes,x

                ; write original length of message in bits to index 56
                lda msg_len_digits
                asl a
                asl a
                sta msg_bytes+56

                rts

hash_msg        ; see Python program for more info

                ; set initial state of algorithm
                ldx #(16-1)
-               lda initial_state,x
                sta state,x
                dex
                bpl -

                ; run 64 rounds
                ;
                lda #0
                sta round
                ;
-               lda round               ; bitops specific to round
                lsr a
                lsr a
                lsr a
                lsr a
                clc
                adc #3
                jsr jump_engine         ; A = sub index (3-6)
                ;
                jsr ops_common          ; operations common to all rounds
                ;
                inc round
                lda round
                cmp #64
                bne -

                ; add initial state
                ;
                ldx #0
--              clc
                ldy #4
                ;
-               lda initial_state,x
                adc state,x
                sta state,x
                inx
                dey
                bne -
                ;
                cpx #16
                bne --

                rts

initial_state   ; initial state of MD5 algorithm
                ; in little-endian order, i.e., 0x67452301 etc.
                hex 01234567 89abcdef fedcba98 76543210

bitops0_15      ; bitops for rounds 0-15
                ; tempdw = ((state2 ^ state3) & state1) ^ state3
                ldx #tempdw
                ldy #state2
                jsr mov_dword
                ldy #state3
                jsr eor_tempdw
                ldy #state1
                jsr and_tempdw
                ldy #state3
                jsr eor_tempdw
                rts

bitops16_31     ; bitops for rounds 16-31
                ; tempdw = ((state1 ^ state2) & state3) ^ state2
                ldx #tempdw
                ldy #state1
                jsr mov_dword
                ldy #state2
                jsr eor_tempdw
                ldy #state3
                jsr and_tempdw
                ldy #state2
                jsr eor_tempdw
                rts

bitops32_47     ; bitops for rounds 32-47
                ; tempdw = state1 ^ state2 ^ state3
                ldx #tempdw
                ldy #state1
                jsr mov_dword
                ldy #state2
                jsr eor_tempdw
                ldy #state3
                jsr eor_tempdw
                rts

bitops48_63     ; bitops for rounds 48-63
                ; tempdw = ((~state3) | state1) ^ state2
                ldx #tempdw
                ldy #state3
                jsr mov_dword
                jsr invert_tempdw
                ldy #state1
                jsr ora_tempdw
                ldy #state2
                jsr eor_tempdw
                rts

and_tempdw      ; AND tempdw with dword at $00,Y
                ldx #3
-               lda $03,y
                and tempdw,x
                sta tempdw,x
                dey
                dex
                bpl -
                rts

eor_tempdw      ; EOR tempdw with dword at $00,Y
                ldx #3
-               lda $03,y
                eor tempdw,x
                sta tempdw,x
                dey
                dex
                bpl -
                rts

ora_tempdw      ; ORA tempdw with dword at $00,Y
                ldx #3
-               lda $03,y
                ora tempdw,x
                sta tempdw,x
                dey
                dex
                bpl -
                rts

invert_tempdw   ; invert tempdw (EOR with 0xffffffff)
                ldx #3
-               lda #%11111111
                eor tempdw,x
                sta tempdw,x
                dex
                bpl -
                rts

ops_common      ; operations common to all rounds

                ; add state0, sines[round] and chunk[chunk_indexes[round]]
                ; to tempdw
                ldy #state0
                jsr add_to_tempdw
                jsr add_sine
                jsr add_chunk

                jsr rol_tempdw          ; rotate tempdw left
                ldy #state1             ; add state1 to tempdw
                jsr add_to_tempdw

                ; shuffle state and tempdw
                ldx #state0             ; state0 = state3
                ldy #state3
                jsr mov_dword
                ldx #state3             ; state3 = state2
                ldy #state2
                jsr mov_dword
                ldx #state2             ; state2 = state1
                ldy #state1
                jsr mov_dword
                ldx #state1             ; state1 = tempdw
                ldy #tempdw
                jsr mov_dword

                rts

mov_dword       ; copy dword from $00,Y to $00,X (note the order of X and Y)
                ; (loop counter in stack; still shorter than unrolled loop)
                ;
                lda #4
                pha
                ;
-               lda $00,y
                sta $00,x
                inx
                iny
                pla
                sec
                sbc #1
                pha
                bne -
                ;
                pla
                rts

add_to_tempdw   ; add dword at $00,Y to tempdw
                ;
                clc
                ldx #0                  ; target index
                ;
-               lda $00,y
                adc tempdw,x
                sta tempdw,x
                iny
                inx
                ;
                txa
                and #%00000100
                beq -
                ;
                rts

add_sine        ; add sine constant specified by "round" to tempdw
                ;
                lda round
                asl a
                asl a
                tay                     ; source index
                ;
                clc
                ldx #0                  ; target index
                ;
-               lda sines,y
                adc tempdw,x
                sta tempdw,x
                iny
                inx
                ;
                txa
                and #%00000100
                beq -
                ;
                rts

sines           ; math.floor(abs(math.sin(i)) * 0x100000000) for i in 1...64
                ; in little-endian order, i.e., 0xd76aa478 etc.
                hex 78a46ad7 56b7c7e8 db702024 eecebdc1
                hex af0f7cf5 2ac68747 134630a8 019546fd
                hex d8988069 aff7448b b15bffff bed75c89
                hex 2211906b 937198fd 8e4379a6 2108b449
                hex 62251ef6 40b340c0 515a5e26 aac7b6e9
                hex 5d102fd6 53144402 81e6a1d8 c8fbd3e7
                hex e6cde121 d60737c3 870dd5f4 ed145a45
                hex 05e9e3a9 f8a3effc d9026f67 8a4c2a8d
                hex 4239faff 81f67187 22619d6d 0c38e5fd
                hex 44eabea4 a9cfde4b 604bbbf6 70bcbfbe
                hex c67e9b28 fa27a1ea 8530efd4 051d8804
                hex 39d0d4d9 e599dbe6 f87ca21f 6556acc4
                hex 442229f4 97ff2a43 a72394ab 39a093fc
                hex c3595b65 92cc0c8f 7df4efff d15d8485
                hex 4f7ea86f e0e62cfe 144301a3 a111084e
                hex 827e53f7 35f23abd bbd2d72a 91d386eb

add_chunk       ; add chunk specified by "round" to tempdw
                ;
                ldx round
                lda chunk_indexes,x     ; 0-15
                asl a
                asl a
                tay                     ; offset to message/chunk
                ;
                jsr add_to_tempdw       ; add dword at $00,Y to tempdw
                rts

rol_tempdw      ; rotate tempdw left, amount specified by "round" (slow)

                ; index to rotate_counts: round=%aabbcc -> index=%aacc
                lda round
                and #%00000011
                sta temp
                lda round
                lsr a
                lsr a
                and #%00001100
                ora temp
                tax

                lda rotate_counts,x
                tax

-               lda tempdw+3
                asl a
                rol tempdw+0
                rol tempdw+1
                rol tempdw+2
                rol tempdw+3
                dex
                bne -

                rts

rotate_counts   db 7, 12, 17, 22  ; rounds  0- 3, ..., 12-15
                db 5,  9, 14, 20  ; rounds 16-19, ..., 28-31
                db 4, 11, 16, 23  ; rounds 32-35, ..., 44-47
                db 6, 10, 15, 21  ; rounds 48-51, ..., 60-63

; --- Main loop - mode 2 ------------------------------------------------------

main_mode2      ; update 2nd line of hash via PPU buffer
                ldy #>(ppu_nt0+24*32+6)  ; PPU address high
                lda #<(ppu_nt0+24*32+6)  ; PPU address low
                ldx #8                  ; source index
                jsr upd_hash_line

                lda #0                  ; back to mode 0
                sta mode

                rts

; --- Subs used in many places ------------------------------------------------

jump_engine     ; jump to sub specified by index A;
                ; RTS in those subs will act like RTS in this sub;
                ; see https://www.nesdev.org/wiki/Jump_table
                ; and https://www.nesdev.org/wiki/RTS_Trick

                ; push target address minus one, high byte first
                asl a
                tax
                lda jump_table+1,x
                pha
                lda jump_table+0,x
                pha

                ; pull address, low byte first; jump to address plus one
                rts

jump_table      dw main_mode0-1, main_mode1-1,  main_mode2-1
                dw bitops0_15-1, bitops16_31-1, bitops32_47-1, bitops48_63-1

upd_hash_line   ; update one line of hash (state) via PPU buffer
                ; Y/A = PPU address high/low, X = source index (0/8)

                sty ppu_buf_adr_hi
                sta ppu_buf_adr_lo

                ldy #0                  ; target index

-               jsr byte_to_ascii
                jsr byte_to_ascii
                lda #ASCII_SPACE
                sta ppu_buffer,y
                iny
                cpy #(4*5)
                bne -

                lda #(4*5)              ; length must be set last
                sta ppu_buf_length

                rts

byte_to_ascii   ; convert one hash byte into 2 digits in PPU buffer

                lda state,x
                lsr a
                lsr a
                lsr a
                lsr a
                jsr digit_to_ascii
                sta ppu_buffer,y
                iny

                lda state,x
                and #%00001111
                jsr digit_to_ascii
                sta ppu_buffer,y
                inx
                iny
                ;
                rts

digit_to_ascii  ; in: A = 0-15, out: A = ASCII for "0"-"9", "A"-"F";
                ; must not alter X, Y
                ;
                clc
                adc #ASCII_ZERO
                cmp #(ASCII_ZERO+10)
                bcc +
                clc
                adc #(ASCII_A-(ASCII_ZERO+10))  ; "A"-"F"
+               rts

                ; NESDev Compo requires $ffd0-$fff9 to be unused
                pad $ffd0, $ff

; --- Interrupt vectors -------------------------------------------------------

                pad $fffa, $ff
                dw nmi, reset, irq      ; IRQ unused

; --- CHR ROM -----------------------------------------------------------------

                base $0000
                incbin "chr.bin"        ; approximately ASCII 0x00-0x5f
                pad $2000, $ff
