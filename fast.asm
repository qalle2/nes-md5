    .require "md5-macros.asm"  ; lue makrotiedosto

md5_algo:
    ; laskee tavujonon MD5-tiivisteen;
    ; portattu koodista osoitteessa http://en.wikipedia.org/wiki/MD5#Pseudocode
    ;
    ; vaatii ennalta lasketut aputaulukot:
    ;     - shl4_table (256 tavua): arvo indeksissä = (indeksi << 4) & 0xff
    ;     - shr4_table (256 tavua): arvo indeksissä = indeksi >> 4
    ;
    ; syöte:
    ;     - little-endian-kaksoissana m0: viestin tavut 0-3
    ;     - little-endian-kaksoissana m1: viestin tavut 4-7
    ;       (itse viesti voi olla 0-7 tavua pitkä; välittömästi sen jälkeen on
    ;       oltava yksi 0x80-tavu ja tarvittaessa 0x00-tavuja)
    ;     - tavu message_length_in_bits: itse viestin pituus bitteinä (ei
    ;       0x80-tavua eikä myöhempiä); 0-56; jaollinen 8:lla
    ;
    ; palauttaa:
    ;     - little-endian-kaksoissanat s0, s1, s2, s3: viestin tiiviste
    ;
    ; käyttää tilapäismuuttujia:
    ;     - little-endian-kaksoissanat tempdw, tempdw2

    ; alusta algoritmin tila
    `dw_copy_const $67452301, s0
    `dw_copy_const $efcdab89, s1
    `dw_copy_const $98badcfe, s2
    `dw_copy_const $10325476, s3

    ; kierros 0
    `dw_xor_and_xor s3, s2, s1, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_mem m0, tempdw
    `dw_add_const $d76aa478, tempdw
    `dw_rol7 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 1
    `dw_xor_and_xor s2, s1, s0, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_mem m1, tempdw
    `dw_add_const $e8c7b756, tempdw
    `dw_rol12 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 2
    `dw_xor_and_xor s1, s0, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $242070db, tempdw
    `dw_rol17 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 3
    `dw_xor_and_xor s0, s3, s2, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $c1bdceee, tempdw
    `dw_rol22 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 4
    `dw_xor_and_xor s3, s2, s1, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $f57c0faf, tempdw
    `dw_rol7 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 5
    `dw_xor_and_xor s2, s1, s0, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $4787c62a, tempdw
    `dw_rol12 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 6
    `dw_xor_and_xor s1, s0, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $a8304613, tempdw
    `dw_rol17 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 7
    `dw_xor_and_xor s0, s3, s2, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $fd469501, tempdw
    `dw_rol22 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 8
    `dw_xor_and_xor s3, s2, s1, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $698098d8, tempdw
    `dw_rol7 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 9
    `dw_xor_and_xor s2, s1, s0, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $8b44f7af, tempdw
    `dw_rol12 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 10
    `dw_xor_and_xor s1, s0, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $ffff5bb1, tempdw
    `dw_rol17 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 11
    `dw_xor_and_xor s0, s3, s2, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $895cd7be, tempdw
    `dw_rol22 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 12
    `dw_xor_and_xor s3, s2, s1, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $6b901122, tempdw
    `dw_rol7 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 13
    `dw_xor_and_xor s2, s1, s0, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $fd987193, tempdw
    `dw_rol12 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 14
    `dw_xor_and_xor s1, s0, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $a679438e, tempdw
    `dw_add_membyte message_length_in_bits, tempdw
    `dw_rol17 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 15
    `dw_xor_and_xor s0, s3, s2, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $49b40821, tempdw
    `dw_rol22 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 16
    `dw_xor_and_xor s2, s1, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_mem m1, tempdw
    `dw_add_const $f61e2562, tempdw
    `dw_rol5 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 17
    `dw_xor_and_xor s1, s0, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $c040b340, tempdw
    `dw_rol9 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 18
    `dw_xor_and_xor s0, s3, s1, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $265e5a51, tempdw
    `dw_rol14 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 19
    `dw_xor_and_xor s3, s2, s0, tempdw
    `dw_add_mem m0, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $e9b6c7aa, tempdw
    `dw_rol20 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 20
    `dw_xor_and_xor s2, s1, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $d62f105d, tempdw
    `dw_rol5 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 21
    `dw_xor_and_xor s1, s0, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $02441453, tempdw
    `dw_rol9 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 22
    `dw_xor_and_xor s0, s3, s1, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $d8a1e681, tempdw
    `dw_rol14 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 23
    `dw_xor_and_xor s3, s2, s0, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $e7d3fbc8, tempdw
    `dw_rol20 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 24
    `dw_xor_and_xor s2, s1, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $21e1cde6, tempdw
    `dw_rol5 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 25
    `dw_xor_and_xor s1, s0, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $c33707d6, tempdw
    `dw_add_membyte message_length_in_bits, tempdw
    `dw_rol9 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 26
    `dw_xor_and_xor s0, s3, s1, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $f4d50d87, tempdw
    `dw_rol14 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 27
    `dw_xor_and_xor s3, s2, s0, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $455a14ed, tempdw
    `dw_rol20 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 28
    `dw_xor_and_xor s2, s1, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $a9e3e905, tempdw
    `dw_rol5 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 29
    `dw_xor_and_xor s1, s0, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $fcefa3f8, tempdw
    `dw_rol9 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 30
    `dw_xor_and_xor s0, s3, s1, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $676f02d9, tempdw
    `dw_rol14 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 31
    `dw_xor_and_xor s3, s2, s0, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $8d2a4c8a, tempdw
    `dw_rol20 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 32
    `dw_xor_xor s1, s2, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $fffa3942, tempdw
    `dw_rol4 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 33
    `dw_xor_xor s0, s1, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $8771f681, tempdw
    `dw_rol11 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 34
    `dw_xor_xor s0, s1, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $6d9d6122, tempdw
    `dw_rol16 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 35
    `dw_xor_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $fde5380c, tempdw
    `dw_add_membyte message_length_in_bits, tempdw
    `dw_rol23 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 36
    `dw_xor_xor s1, s2, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_mem m1, tempdw
    `dw_add_const $a4beea44, tempdw
    `dw_rol4 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 37
    `dw_xor_xor s0, s1, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $4bdecfa9, tempdw
    `dw_rol11 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 38
    `dw_xor_xor s0, s1, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $f6bb4b60, tempdw
    `dw_rol16 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 39
    `dw_xor_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $bebfbc70, tempdw
    `dw_rol23 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 40
    `dw_xor_xor s1, s2, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $289b7ec6, tempdw
    `dw_rol4 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 41
    `dw_xor_xor s0, s1, s2, tempdw
    `dw_add_mem m0, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $eaa127fa, tempdw
    `dw_rol11 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 42
    `dw_xor_xor s0, s1, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $d4ef3085, tempdw
    `dw_rol16 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 43
    `dw_xor_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $04881d05, tempdw
    `dw_rol23 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 44
    `dw_xor_xor s1, s2, s3, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $d9d4d039, tempdw
    `dw_rol4 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 45
    `dw_xor_xor s0, s1, s2, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $e6db99e5, tempdw
    `dw_rol11 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 46
    `dw_xor_xor s0, s1, s3, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $1fa27cf8, tempdw
    `dw_rol16 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 47
    `dw_xor_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $c4ac5665, tempdw
    `dw_rol23 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 48
    `dw_not_or_xor s3, s1, s2, tempdw
    `dw_add_mem m0, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $f4292244, tempdw
    `dw_rol6 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 49
    `dw_not_or_xor s2, s0, s1, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $432aff97, tempdw
    `dw_rol10 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 50
    `dw_not_or_xor s1, s3, s0, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $ab9423a7, tempdw
    `dw_add_membyte message_length_in_bits, tempdw
    `dw_rol15 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 51
    `dw_not_or_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $fc93a039, tempdw
    `dw_rol21 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 52
    `dw_not_or_xor s3, s1, s2, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $655b59c3, tempdw
    `dw_rol6 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 53
    `dw_not_or_xor s2, s0, s1, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $8f0ccc92, tempdw
    `dw_rol10 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 54
    `dw_not_or_xor s1, s3, s0, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $ffeff47d, tempdw
    `dw_rol15 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 55
    `dw_not_or_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_mem m1, tempdw
    `dw_add_const $85845dd1, tempdw
    `dw_rol21 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 56
    `dw_not_or_xor s3, s1, s2, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $6fa87e4f, tempdw
    `dw_rol6 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 57
    `dw_not_or_xor s2, s0, s1, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $fe2ce6e0, tempdw
    `dw_rol10 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 58
    `dw_not_or_xor s1, s3, s0, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $a3014314, tempdw
    `dw_rol15 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 59
    `dw_not_or_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $4e0811a1, tempdw
    `dw_rol21 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; -------------------------------------------------------------------------

    ; kierros 60
    `dw_not_or_xor s3, s1, s2, tempdw
    `dw_add_mem s0, tempdw
    `dw_add_const $f7537e82, tempdw
    `dw_rol6 tempdw
    `dw_add_mem_to_mem tempdw, s1, s0

    ; kierros 61
    `dw_not_or_xor s2, s0, s1, tempdw
    `dw_add_mem s3, tempdw
    `dw_add_const $bd3af235, tempdw
    `dw_rol10 tempdw
    `dw_add_mem_to_mem tempdw, s0, s3

    ; kierros 62
    `dw_not_or_xor s1, s3, s0, tempdw
    `dw_add_mem s2, tempdw
    `dw_add_const $2ad7d2bb, tempdw
    `dw_rol15 tempdw
    `dw_add_mem_to_mem tempdw, s3, s2

    ; kierros 63
    `dw_not_or_xor s0, s2, s3, tempdw
    `dw_add_mem s1, tempdw
    `dw_add_const $eb86d391, tempdw
    `dw_rol21 tempdw
    `dw_add_mem_to_mem tempdw, s2, s1

    ; lisää tiivisteeseen samat vakiot kuin alussa
    `dw_add_const $67452301, s0
    `dw_add_const $efcdab89, s1
    `dw_add_const $98badcfe, s2
    `dw_add_const $10325476, s3

    rts
