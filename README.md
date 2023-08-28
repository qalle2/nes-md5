# nes-md5
Compute an MD5 hash on the NES. The message can be 0&ndash;7 bytes long.

## List of files

### Text
* `assemble.sh`: a Linux script that assembles `md5.asm` (don't run it before reading it)
* `fast.asm`: an old fast but incomprehensible algorithm
* `fast-macros.asm`: an old fast but incomprehensible algorithm
* `fast-raw.asm`: an old fast but incomprehensible algorithm
* `md5.asm`: NES source code, assembles with ASM6
* `md5-simple.py`: a simple MD5 implementation in Python

### Other
* `chr.bin.gz`: CHR data in NES CHR format
* `chr.png`: CHR data as an image
* `md5.nes.gz`: assembled NES program
