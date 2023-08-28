; makroja, jotka liittyvät 32-bittisiin little-endian-kaksoissanoihin (vähiten
; merkitsevä tavu ensin);
; apumakrojen (kutsutaan vain tästä tiedostosta) nimet alkavat "_":lla;
; muiden makrojen nimet alkavat "dw_":lla

; -----------------------------------------------------------------------------

; 32-bittinen kierto vasemmalle ilman muistibittiä aputaulukoita käyttäen
; (kierto 4 + 8n bittiä, aina 80 kellojaksoa)

.macro _nybble_combine
    ; (_1 << 4) | (_2 >> 4) -> _3
    ldx _1
    lda shl4_table,x
    ldx _2
    ora shr4_table,x
    sta _3
.macend

.macro dw_rol4
    ;     tavu3    tavu2    tavu1    tavu0
    ;    abcdefgh ijklmnop qrstuvwx yzABCDEF
    ; -> efghijkl mnopqrst uvwxyzAB CDEFabcd

    `_nybble_combine _1 + 3, _1 + 2, tempdw2 + 3
    `_nybble_combine _1 + 1, _1 + 0, tempdw2 + 1
    `_nybble_combine _1 + 2, _1 + 1, _1 + 2
    `_nybble_combine _1 + 0, _1 + 3, _1 + 0

    lda tempdw2 + 1
    sta _1 + 1
    lda tempdw2 + 3
    sta _1 + 3
.macend

.macro dw_rol12
    ;     tavu3    tavu2    tavu1    tavu0
    ;    abcdefgh ijklmnop qrstuvwx yzABCDEF
    ; -> mnopqrst uvwxyzAB CDEFabcd efghijkl

    `_nybble_combine _1 + 2, _1 + 1, tempdw2 + 3
    `_nybble_combine _1 + 1, _1 + 0, tempdw2 + 2
    `_nybble_combine _1 + 0, _1 + 3, _1 + 1
    `_nybble_combine _1 + 3, _1 + 2, _1 + 0

    lda tempdw2 + 2
    sta _1 + 2
    lda tempdw2 + 3
    sta _1 + 3
.macend

.macro dw_rol20
    ;     tavu3    tavu2    tavu1    tavu0
    ;    abcdefgh ijklmnop qrstuvwx yzABCDEF
    ; -> uvwxyzAB CDEFabcd efghijkl mnopqrst

    `_nybble_combine _1 + 2, _1 + 1, tempdw2 + 0
    `_nybble_combine _1 + 1, _1 + 0, tempdw2 + 3
    `_nybble_combine _1 + 3, _1 + 2, _1 + 1
    `_nybble_combine _1 + 0, _1 + 3, _1 + 2

    lda tempdw2 + 0
    sta _1 + 0
    lda tempdw2 + 3
    sta _1 + 3
.macend

; -----------------------------------------------------------------------------

; 32-bittinen kierto vasemmalle ilman muistibittiä ilman aputaulukoita

.macro _dw_rol_bit
    ; kierrä 1 bitti vasemmalle olettaen, että ENITEN merkitsevä tavu on A:ssa
    ; 19 kellojaksoa
    cmp #%10000000
    rol _1 + 0
    rol _1 + 1
    rol _1 + 2
    rol
.macend

.macro _dw_ror_bit
    ; kierrä 1 bitti oikealle olettaen, että VÄHITEN merkitsevä tavu on SEKÄ
    ; A:SSA ETTÄ MUISTISSA
    ; 22 kellojaksoa
    lsr
    ror _1 + 3
    ror _1 + 2
    ror _1 + 1
    ror _1 + 0
.macend

.macro _dw_rol_byte_lsb
    ; kierrä 1 tavu vasemmalle jättäen uuden VÄHITEN merkitsevän tavun VAIN
    ; A:HAN
    ; _1+0 -> _1+1 -> _1+2 -> _1+3 -> A
    ; 21 kellojaksoa
    lda _1 + 3
    ldx _1 + 2
    stx _1 + 3
    ldx _1 + 1
    stx _1 + 2
    ldx _1 + 0
    stx _1 + 1
.macend

.macro _dw_rol_byte_msb
    ; kierrä 1 tavu vasemmalle jättäen uuden ENITEN merkitsevän tavun
    ; VAIN A:HAN
    ; _1+3 -> _1+0 -> _1+1 -> _1+2 -> A
    ; 21 kellojaksoa
    lda _1 + 2
    ldx _1 + 1
    stx _1 + 2
    ldx _1 + 0
    stx _1 + 1
    ldx _1 + 3
    stx _1 + 0
.macend

.macro _dw_ror_byte
    ; kierrä 1 tavu oikealle jättäen uuden VÄHITEN merkitsevän tavun VAIN A:HAN
    ; _1+0 -> _1+3 -> _1+2 -> _1+1 -> A
    ; 21 kellojaksoa
    lda _1 + 1
    ldx _1 + 2
    stx _1 + 1
    ldx _1 + 3
    stx _1 + 2
    ldx _1 + 0
    stx _1 + 3
.macend

.macro _dw_swap_words_lsb
    ; vaihda sanat keskenään jättäen uuden VÄHITEN merkitsevän tavun VAIN A:HAN
    ; 21 kellojaksoa
    lda _1 + 3
    ldx _1 + 1
    stx _1 + 3
    sta _1 + 1
    lda _1 + 2
    ldx _1 + 0
    stx _1 + 2
.macend

.macro _dw_swap_words_msb
    ; vaihda sanat keskenään jättäen uuden ENITEN merkitsevän tavun VAIN A:HAN
    ; 21 kellojaksoa
    lda _1 + 0
    ldx _1 + 2
    stx _1 + 0
    sta _1 + 2
    lda _1 + 1
    ldx _1 + 3
    stx _1 + 1
.macend

.macro dw_rol5  ; 90 kellojaksoa
    `_dw_rol_byte_lsb _1
    sta _1 + 0
    `_dw_ror_bit _1
    `_dw_ror_bit _1
    `_dw_ror_bit _1
.macend

.macro dw_rol6  ; 68 kellojaksoa
    `_dw_rol_byte_lsb _1
    sta _1 + 0
    `_dw_ror_bit _1
    `_dw_ror_bit _1
.macend

.macro dw_rol7  ; 46 kellojaksoa
    `_dw_rol_byte_lsb _1
    sta _1 + 0
    `_dw_ror_bit _1
.macend

.macro dw_rol9  ; 43 kellojaksoa
    `_dw_rol_byte_msb _1
    `_dw_rol_bit _1
    sta _1 + 3
.macend

.macro dw_rol10  ; 62 kellojaksoa
    `_dw_rol_byte_msb _1
    `_dw_rol_bit _1
    `_dw_rol_bit _1
    sta _1 + 3
.macend

.macro dw_rol11  ; 81 kellojaksoa
    `_dw_rol_byte_msb _1
    `_dw_rol_bit _1
    `_dw_rol_bit _1
    `_dw_rol_bit _1
    sta _1 + 3
.macend

.macro dw_rol14  ; 68 kellojaksoa
    `_dw_swap_words_lsb _1
    sta _1 + 0
    `_dw_ror_bit _1
    `_dw_ror_bit _1
.macend

.macro dw_rol15  ; 46 kellojaksoa
    `_dw_swap_words_lsb _1
    sta _1 + 0
    `_dw_ror_bit _1
.macend

.macro dw_rol16  ; 24 kellojaksoa
    `_dw_swap_words_lsb _1
    sta _1 + 0
.macend

.macro dw_rol17  ; 43 kellojaksoa
    `_dw_swap_words_msb _1
    `_dw_rol_bit _1
    sta _1 + 3
.macend

.macro dw_rol21  ; 90 kellojaksoa
    `_dw_ror_byte _1
    sta _1 + 0
    `_dw_ror_bit _1
    `_dw_ror_bit _1
    `_dw_ror_bit _1
.macend

.macro dw_rol22  ; 68 kellojaksoa
    `_dw_ror_byte _1
    sta _1 + 0
    `_dw_ror_bit _1
    `_dw_ror_bit _1
.macend

.macro dw_rol23  ; 46 kellojaksoa
    `_dw_ror_byte _1
    sta _1 + 0
    `_dw_ror_bit _1
.macend

; -----------------------------------------------------------------------------

; argumentit: lähde vakio, kohde muisti

.macro _copy_const
    lda #_1
    sta _2
.macend

.macro dw_copy_const
    `_copy_const _1 & $ff, _2 + 0
    `_copy_const [_1 / $100] & $ff, _2 + 1
    `_copy_const [_1 / $10000] & $ff, _2 + 2
    `_copy_const [_1 / $1000000] & $ff, _2 + 3
.macend

; -----------------------------------------------------------------------------

; argumentit: lähde vakio, lähde/kohde muisti

.macro _add_const
    lda #_1
    adc _2
    sta _2
.macend

.macro dw_add_const
    clc
    `_add_const _1 & $ff, _2 + 0
    `_add_const [_1 / $100] & $ff, _2 + 1
    `_add_const [_1 / $10000] & $ff, _2 + 2
    `_add_const [_1 / $1000000] & $ff, _2 + 3
.macend

; -----------------------------------------------------------------------------

; argumentit: lähde muisti, lähde/kohde muisti

.macro _add_mem
    lda _1
    adc _2
    sta _2
.macend

.macro dw_add_mem
    clc
    `_add_mem _1 + 0, _2 + 0
    `_add_mem _1 + 1, _2 + 1
    `_add_mem _1 + 2, _2 + 2
    `_add_mem _1 + 3, _2 + 3
.macend

; -----------------------------------------------------------------------------

; argumentit: lähde muisti, lähde muisti, kohde muisti

.macro _add_mem_to_mem
    lda _1
    adc _2
    sta _3
.macend

.macro dw_add_mem_to_mem
    clc
    `_add_mem_to_mem _1 + 0, _2 + 0, _3 + 0
    `_add_mem_to_mem _1 + 1, _2 + 1, _3 + 1
    `_add_mem_to_mem _1 + 2, _2 + 2, _3 + 2
    `_add_mem_to_mem _1 + 3, _2 + 3, _3 + 3
.macend

; -----------------------------------------------------------------------------

.macro dw_add_membyte
    ; argumentit: lähde muisti tavu, lähde/kohde muisti kaksoissana
    clc
    `_add_mem _1, _2 + 0
    `_add_const 0, _2 + 1
    `_add_const 0, _2 + 2
    `_add_const 0, _2 + 3
.macend

; -----------------------------------------------------------------------------

; ((_1 XOR _2) AND _3) XOR _1 -> _4

.macro _xor_and_xor
    lda _1
    eor _2
    and _3
    eor _1
    sta _4
.macend

.macro dw_xor_and_xor
    `_xor_and_xor _1 + 0, _2 + 0, _3 + 0, _4 + 0
    `_xor_and_xor _1 + 1, _2 + 1, _3 + 1, _4 + 1
    `_xor_and_xor _1 + 2, _2 + 2, _3 + 2, _4 + 2
    `_xor_and_xor _1 + 3, _2 + 3, _3 + 3, _4 + 3
.macend

; -----------------------------------------------------------------------------

; _1 XOR _2 XOR _3 -> _4

.macro _xor_xor
    lda _1
    eor _2
    eor _3
    sta _4
.macend

.macro dw_xor_xor
    `_xor_xor _1 + 0, _2 + 0, _3 + 0, _4 + 0
    `_xor_xor _1 + 1, _2 + 1, _3 + 1, _4 + 1
    `_xor_xor _1 + 2, _2 + 2, _3 + 2, _4 + 2
    `_xor_xor _1 + 3, _2 + 3, _3 + 3, _4 + 3
.macend

; -----------------------------------------------------------------------------

; ((NOT _1) OR _2) XOR _3 -> _4

.macro _not_or_xor
    lda _1
    eor #%11111111
    ora _2
    eor _3
    sta _4
.macend

.macro dw_not_or_xor
    `_not_or_xor _1 + 0, _2 + 0, _3 + 0, _4 + 0
    `_not_or_xor _1 + 1, _2 + 1, _3 + 1, _4 + 1
    `_not_or_xor _1 + 2, _2 + 2, _3 + 2, _4 + 2
    `_not_or_xor _1 + 3, _2 + 3, _3 + 3, _4 + 3
.macend
