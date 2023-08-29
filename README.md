# nes-md5
Compute an MD5 hash on the NES. The message can be 0&ndash;8 bytes long.

![screenshot](snap.png)

## List of files

### Text
* `assemble.sh`: a Linux script that assembles `md5.asm` (don't run it before reading it)
* `md5.asm`: NES source code, assembles with ASM6
* `md5-simple.py`: a simple MD5 implementation in Python

### Other
* `chr.bin.gz`: CHR data in NES CHR format
* `chr.png`: CHR data as an image
* `md5.nes.gz`: assembled NES program
* `snap.png`: screenshot

## Technical information
* mapper: NROM
* PRG ROM: 16 KiB (only uses the last 2 KiB)
* CHR ROM: 8 KiB (only uses the first 2 KiB)
* name table mirroring: does not matter
