; Qalle's MD5 Hasher (NES, ASM6)

; --- Constants ---------------------------------------------------------------

; Notes:
; - "dword" = 32-bit little endian integer
; - run_main_loop: $00-$7f = no, $80-$ff = yes
; - mode:
;     0 = editing
;     1 = computing and printing 1st line of hash
;     2 = printing 2nd line of hash

; RAM
msg_bytes       equ $00    ; message/chunk as bytes (64 bytes, 16 dwords)
msg_digits      equ $40    ; message as hexadecimal digits (16 bytes)
state           equ $50    ; MD5 state/hash (16 bytes, split in 4 dwords)
state0          equ $50
state1          equ $54
state2          equ $58
state3          equ $5c
ppu_buffer      equ $60    ; copied to PPU on next VBlank (24 bytes)
tdw             equ $78    ; temporary dword (4 bytes)
msg_len_digits  equ $f0    ; message length in digits (0-14)
msg_len_bytes   equ $f1    ; message length in bytes (0-7)
run_main_loop   equ $f2    ; is main loop allowed to run? (see above)
pad_status      equ $f3    ; joypad status
prev_pad_status equ $f4    ; previous joypad status
cursor_pos      equ $f5    ; cursor position (0-13)
mode            equ $f6    ; program mode (see above)
ppu_buf_adr_hi  equ $f7    ; PPU buffer - high byte of address
ppu_buf_adr_lo  equ $f8    ; PPU buffer - low  byte of address
ppu_buf_length  equ $f9    ; PPU buffer - length
round           equ $fa    ; MD5 round (0-63)
sprite_data     equ $0200  ; OAM page ($100 bytes)
shl4_table      equ $0300  ; LUT ($100 bytes)
shr4_table      equ $0400  ; LUT ($100 bytes)

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

; --- iNES header -------------------------------------------------------------

                ; see https://wiki.nesdev.org/w/index.php/INES
                base $0000
                db "NES", $1a            ; file id
                db 1, 1                  ; 16 KiB PRG ROM, 8 KiB CHR ROM
                db %00000000, %00000000  ; NROM mapper, horiz. NT mirroring
                pad $0010, $00           ; unused

; --- Initialization ----------------------------------------------------------

                base $c000              ; last 16 KiB of CPU address space
                pad $e000, $ff          ; only use last 8 KiB

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
                jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask
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
                lda #0
                sta msg_len_digits

                ; create LUTs
                ldx #0
-               txa
                asl a
                asl a
                asl a
                asl a
                sta shl4_table,x
                txa
                lsr a
                lsr a
                lsr a
                lsr a
                sta shr4_table,x
                inx
                bne -

                rts

init_spr_data   ; initial sprite data (Y, tile, attributes, X)
                ; #0: cursor
                db 15*8-1, "^", %00000000, 2*8  ; cursor

init_ppu_mem    ; initialize PPU memory

                ; set palette (while still in VBlank)

                ldy #$3f
                lda #$00
                jsr set_ppu_addr        ; Y*$100+A -> address

                ; fill palettes with alternating black and white
                ; (only $3f00, $3f01, $3f10, $3f11 are used)
                lda #$0f                ; black
                ldy #$30                ; white
                ldx #(2*8)
-               sta ppu_data
                sty ppu_data
                dex
                bne -

                ; clear NT0 & AT0 ($400 bytes)
                ;
                ldy #$20
                lda #$00
                jsr set_ppu_addr        ; Y*$100+A -> address
                ;
                ldy #4
                tax
-               sta ppu_data
                inx
                bne -
                dey
                bne -

                ; print strings
                ;
                ldx #$ff
--              inx
                lda strings,x           ; addr high / end of all strings
                bmi strs_end
                sta ppu_addr
                inx
                lda strings,x           ; addr low
                sta ppu_addr
                ;
-               inx
                lda strings,x
                bmi --                  ; end of string
                sta ppu_data
                jmp -

strs_end        rts

set_ppu_addr    sty ppu_addr            ; Y*$100+A -> address
                sta ppu_addr
                rts

macro str_start _y, _x
                dh $2000+_y*32+_x
                dl $2000+_y*32+_x
endm

strings         ; for each: PPU address high/low, bytes, terminator (negative)
                ; after all strings: terminator (negative)

                str_start 2, 6
                db "QALLE'S MD5 HASHER", $80

                str_start 5, 4
                db "  [ \\  MOVE CURSOR", $80
                str_start 6, 4
                db "  ] ^  ADJUST DIGIT", $80
                str_start 7, 4
                db "  B A  DECREASE/INCREASE", $80
                str_start 8, 11
                db "MESSAGE LENGTH", $80
                str_start 9, 4
                db "START  COMPUTE HASH", $80

                str_start 12, 2
                db "MESSAGE:", $80

                str_start 17, 2
                db "HASH:", $80
                str_start 19, 2
                db "---- ---- ---- ----", $80
                str_start 21, 2
                db "---- ---- ---- ----", $80

                db $80                  ; end all strings

; --- Main loop - common ------------------------------------------------------

main_loop       ; wait until NMI routine sets flag
                bit run_main_loop
                bpl main_loop

                ; store previous joypad status, read joypad
                lda pad_status
                sta prev_pad_status
                jsr read_joypad

                ; run sub for the mode we're in
                lda mode
                bne +
                jsr main_mode0
                jmp ++
+               cmp #1
                bne +
                jsr main_mode1
                jmp ++
+               jsr main_mode2

++              ; update cursor sprite tile
                ldx mode
                lda mode2crsr_tile,x
                sta sprite_data+0*4+1

                ; update cursor sprite X
                ldx cursor_pos
                lda crsr_pos_to_x,x
                sta sprite_data+0*4+3

                ; clear flag (must be done after writing PPU buffer to ensure
                ; it gets flushed before being written again)
                lsr run_main_loop

                jmp main_loop           ; restart main loop

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

mode2crsr_tile  ; mode to cursor sprite tile
                db "^@@"                ; up arrow, clock, clock

crsr_pos_to_x   ; hexadecimal digit index to cursor sprite X
                db  2*8,  3*8
                db  5*8,  6*8
                db  8*8,  9*8
                db 11*8, 12*8
                db 14*8, 15*8
                db 17*8, 18*8
                db 20*8, 21*8

; --- Main loop - mode 0 ------------------------------------------------------

main_mode0      jsr button_handler      ; handle buttons
                jsr update_msg          ; update message via PPU buffer
                rts

button_handler  ; exit if something was pressed on last frame
                lda prev_pad_status
                bne +

                lda pad_status
                bmi inc_msg_len         ; A
                asl a
                bmi dec_msg_len         ; B
                asl a
                asl a
                bmi start_comp          ; start
                asl a
                bmi inc_digit           ; up
                asl a
                bmi dec_digit           ; down
                asl a
                bmi cursor_left         ; left
                bne cursor_right        ; right
+               rts

inc_msg_len     lda msg_len_digits
                cmp #14
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

inc_digit       ldx cursor_pos
                lda msg_digits,x
                clc
                adc #1
                jmp +
                ;
dec_digit       ldx cursor_pos
                lda msg_digits,x
                sec
                sbc #1
+               and #%00001111
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

start_comp      inc mode                ; to mode 1
                rts

update_msg      ; update message via PPU buffer

                lda #$21
                sta ppu_buf_adr_hi
                lda #$c2
                sta ppu_buf_adr_lo

                ; write "-- -- "... to buffer
                lda #$2d                ; "-"
                ldy #$20                ; " "
                ldx #0                  ; target index
-               sta ppu_buffer,x
                inx
                sta ppu_buffer,x
                inx
                sty ppu_buffer,x
                inx
                cpx #(3*7)
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

+               lda #(3*7)              ; length must be set last
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
                lda #$62                ; PPU address low
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

+               ; append byte 0x80
                lda #$80
                ldx msg_len_bytes
                sta msg_bytes,x

                ; write original length of message in bits to index 56
                lda msg_len_digits
                asl a
                asl a
                sta msg_bytes+56

                rts

macro ldxy _1, _2                       ; used in hash_msg
                ldx _1
                ldy _2
endm

hash_msg        ; set initial state of algorithm
                ldx #0
-               lda init_state,x
                sta state,x
                inx
                cpx #16
                bne -

                ; round 0 (see Python program)

                lda #0
                sta round

                ldxy #tdw, #state2      ; t = state[2]
                jsr mov_dword
                ldxy #tdw, #state3      ; t ^= state[3]
                jsr eor_dword
                ldxy #tdw, #state1      ; t &= state[1]
                jsr and_dword
                ldxy #tdw, #state3      ; t ^= state[3]
                jsr eor_dword

                ldx #state0             ; t += state[0]
                jsr add_dword
                jsr add_sine            ; t += SINES[round]
                jsr add_chunk           ; t += chunk[CHUNK_INDEXES[round]]
                jsr rol_dword           ; t = ROL(t, SHIFTS[round])
                ldx #state1             ; t += state[1]
                jsr add_dword
                ldxy #state0, #state3   ; state0 = state3
                jsr mov_dword
                ldxy #state3, #state2   ; state3 = state2
                jsr mov_dword
                ldxy #state2, #state1   ; state2 = state1
                jsr mov_dword
                ldxy #state1, #tdw      ; state1 = t
                jsr mov_dword

                ; md5("", rounds=1) = 7777 7777 fdd2 ed94 8788 8888 7431 eda8

                ; TODO: do rounds 2-64

                ; add initial state
                ldx #0
-               clc
                lda init_state,x
                adc state,x
                sta state,x
                inx
                lda init_state,x
                adc state,x
                sta state,x
                inx
                lda init_state,x
                adc state,x
                sta state,x
                inx
                lda init_state,x
                adc state,x
                sta state,x
                inx
                cpx #16
                bne -

                rts

mov_dword       ; copy dword from $00,Y to $00,X (note the order of X and Y)
                lda $00,y
                sta $00,x
                lda $01,y
                sta $01,x
                lda $02,y
                sta $02,x
                lda $03,y
                sta $03,x
                rts

add_dword       ; ADC tdw with dword at $00,X
                clc
                lda $00,x
                adc tdw+0
                sta tdw+0
                lda $01,x
                adc tdw+1
                sta tdw+1
                lda $02,x
                adc tdw+2
                sta tdw+2
                lda $03,x
                adc tdw+3
                sta tdw+3
                rts

and_dword       ; AND dword at $00,X with dword at $00,Y
                lda $00,y
                and $00,x
                sta $00,x
                lda $01,y
                and $01,x
                sta $01,x
                lda $02,y
                and $02,x
                sta $02,x
                lda $03,y
                and $03,x
                sta $03,x
                rts

eor_dword       ; EOR dword at $00,X with dword at $00,Y
                lda $00,y
                eor $00,x
                sta $00,x
                lda $01,y
                eor $01,x
                sta $01,x
                lda $02,y
                eor $02,x
                sta $02,x
                lda $03,y
                eor $03,x
                sta $03,x
                rts

add_sine        ; add sine constant at round*4 to tdw
                ;
                lda round
                asl a
                asl a
                tax
                ;
                clc
                lda sines+0,x
                adc tdw+0
                sta tdw+0
                lda sines+1,x
                adc tdw+1
                sta tdw+1
                lda sines+2,x
                adc tdw+2
                sta tdw+2
                lda sines+3,x
                adc tdw+3
                sta tdw+3
                rts

add_chunk       ; add a chunk specified by "round" to tdw
                ;
                ldx round
                lda chunk_indexes,x     ; 0-15
                asl a
                asl a
                tax                     ; offset to message/chunk
                ;
                clc
                lda msg_bytes+0,x
                adc tdw+0
                sta tdw+0
                lda msg_bytes+1,x
                adc tdw+1
                sta tdw+1
                lda msg_bytes+2,x
                adc tdw+2
                sta tdw+2
                lda msg_bytes+3,x
                adc tdw+3
                sta tdw+3
                rts

rol_dword       ; rotate tdw left specified by "round"; TODO: optimize
                ldx round
                lda shift_counts,x
                tax
-               jsr rol1_tdw
                dex
                bne -
                rts

rol1_tdw        ; ROL tdw
                lda tdw+3
                asl a
                rol tdw+0
                rol tdw+1
                rol tdw+2
                rol tdw+3
                rts

ror1_tdw        ; ROR tdw
                lda tdw+0
                lsr a
                ror tdw+3
                ror tdw+2
                ror tdw+1
                ror tdw+0
                rts

rol8_tdw        ; ROL*8 tdw
                lda tdw+3
                pha
                lda tdw+2
                sta tdw+3
                lda tdw+1
                sta tdw+2
                lda tdw+0
                sta tdw+1
                pla
                sta tdw+0
                rts

ror8_tdw        ; ROR*8 tdw
                lda tdw+0
                pha
                lda tdw+1
                sta tdw+0
                lda tdw+2
                sta tdw+1
                lda tdw+3
                sta tdw+2
                pla
                sta tdw+3
                rts

init_state      ; initial state of MD5 algorithm
                ; in little-endian order, i.e., 0x67452301 etc.
                ;
                hex 01234567 89abcdef fedcba98 76543210

sines           ; math.floor(abs(math.sin(i)) * 0x100000000) for i in 1...64
                ; in little-endian order, i.e., 0xd76aa478 etc.
                ;
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

chunk_indexes   ; chunk index for each round
                db 0, 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
                db 1, 6, 11,  0,  5, 10, 15,  4,  9, 14,  3,  8, 13,  2,  7, 12
                db 5, 8, 11, 14,  1,  4,  7, 10, 13,  0,  3,  6,  9, 12, 15,  2
                db 0, 7, 14,  5, 12,  3, 10,  1,  8, 15,  6, 13,  4, 11,  2,  9

shift_counts    ; shift count for each round
                db 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22
                db 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20
                db 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23
                db 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21

; --- Main loop - mode 2 ------------------------------------------------------

main_mode2      ; update 2nd line of hash via PPU buffer
                lda #$a2                ; PPU address low
                ldx #8                  ; source index
                jsr upd_hash_line

                lda #0                  ; back to mode 0
                sta mode

                rts

; --- Subs used in many places ------------------------------------------------

upd_hash_line   ; update one line of hash via PPU buffer
                ; A = PPU address low, X = source index

                ldy #$22
                sty ppu_buf_adr_hi
                sta ppu_buf_adr_lo

                ldy #0                  ; target index

-               jsr byte_to_ascii
                jsr byte_to_ascii
                lda #$20                ; " "
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
                ora #$30                ; "0" = 0x30
                cmp #$3a
                bcc +
                clc
                adc #7                  ; "A" = 0x41
+               rts

; --- Interrupt routines ------------------------------------------------------

                align $100, $ff         ; for speed

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

                jsr flush_ppu_buf

                sec                     ; set flag to let main loop run once
                ror run_main_loop

                jsr set_ppu_regs        ; set ppu_scroll/ppu_ctrl/ppu_mask

                pla                     ; pull Y, X, A
                tay
                pla
                tax
                pla

irq             rti                     ; IRQ unused

flush_ppu_buf   ; flush PPU buffer

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

; --- Interrupt vectors -------------------------------------------------------

                pad $fffa, $ff
                dw nmi, reset, irq      ; IRQ unused

; --- CHR ROM -----------------------------------------------------------------

                base $0000
                pad $0200
                incbin "chr.bin"        ; ASCII 0x20-0x7f
                pad $2000, $ff
