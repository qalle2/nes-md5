; Qalle's MD5 Hasher (NES, ASM6)

; Simplified MD5 algorithm (only for short messages):
; 1. Pad message:
;    1. Take 64 * byte 0x00.
;    2. Replace beginning with original message.
;    3. Replace following byte with 0x80.
;    4. Replace byte at index 56 with original length in *bits*.
; 2. Treat message as 16 * 32-bit ints.
; 3. Hash message to 4 * 32-bit ints (16 bytes).
; 4. Add each 32-bit int of the initial (constant) 16-byte state to the
;    corresponding 32-bit int in the hash.

; md5("") = d41d 8cd9 8f00 b204 e980 0998 ecf8 427e

; --- Constants ---------------------------------------------------------------

; Notes:
; - dword0, dword1, dword2, dword3: 32-bit unsigned integers
; - run_main_loop: $00-$7f = no, $80-$ff = yes
; - mode:
;     0 = editing
;     1 = computing and printing 1st line of hash
;     2 = printing 2nd line of hash

; RAM
msg_bytes       equ $00    ; message as bytes (64 bytes)
msg_digits      equ $40    ; message as hexadecimal digits (16 bytes)
hash            equ $50    ; MD5 hash (16 bytes)
dword0          equ $60    ; see above (4 bytes)
dword1          equ $64    ; see above (4 bytes)
dword2          equ $68    ; see above (4 bytes)
dword3          equ $6c    ; see above (4 bytes)
ppu_buffer      equ $70    ; copied to PPU on next VBlank (24 bytes)
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

main_mode1      jsr prepare_msg

                ; TODO: treating the message as 16 * 32-bit ints,
                ; hash it to 4 * 32-bit ints (16 bytes).

                ; TODO: finally, add initial state (constant) to hash
                ; 0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476

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

                lda hash,x
                lsr a
                lsr a
                lsr a
                lsr a
                jsr digit_to_ascii
                sta ppu_buffer,y
                iny

                lda hash,x
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
