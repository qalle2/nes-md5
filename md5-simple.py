# a simplified MD5 implementation, based on https://github.com/qalle2/md5-algo
# does not support messages longer than 7 bytes

import math, struct, sys

# initial state of algorithm
INIT_STATE = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)

# math.floor(abs(math.sin(i)) * 0x100000000) for i in 1...64
SINES = (
    0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
    0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
    0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
    0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
    0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
    0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
    0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
    0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
    0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
    0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
    0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
    0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
    0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
    0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
    0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
    0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
)

CHUNK_INDEXES = (
    0, 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    1, 6, 11,  0,  5, 10, 15,  4,  9, 14,  3,  8, 13,  2,  7, 12,
    5, 8, 11, 14,  1,  4,  7, 10, 13,  0,  3,  6,  9, 12, 15,  2,
    0, 7, 14,  5, 12,  3, 10,  1,  8, 15,  6, 13,  4, 11,  2,  9,
)

SHIFTS = (
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
)

def pad_message(message):
    assert len(message) <= 7
    return (
        message
        + b"\x80"
        + (55 - len(message)) * b"\x00"
        + bytes((len(message) * 8,)) + 7 * b"\x00"
    )

def add(a, b):
    # add 32-bit integers
    return (a + b) & 0xffffffff

def rol(n):
    # rotate a 32-bit integer left
    return (n << 1) & 0xffffffff | (n >> 31)

def hash_chunk(state, chunk):
    # hash one chunk
    # state:   4 * 32 = 128 bits
    # chunk:  16 * 32 = 512 bits
    # return:  4 * 32 = 128 bits

    (a, b, c, d) = state

    for r in range(64):
        if r < 16:
            t = d ^ (b & (c ^ d))
        elif r < 32:
            t = c ^ (d & (b ^ c))
        elif r < 48:
            t = b ^ c ^ d
        else:
            t = c ^ (b | ~d)

        t = add(t, a)
        t = add(t, SINES[r])
        t = add(t, chunk[CHUNK_INDEXES[r]])

        for i in range(SHIFTS[r]):
            t = rol(t)

        a = d
        d = c
        c = b
        b = (b + t) & 0xffffffff

    return (a, b, c, d)

def md5(message, debug=False):
    # hash a bytestring; return the hash as 16 bytes

    assert len(message) <= 7

    if debug:
        print("Message:", message.hex())

    # pad to 512 bits (64 bytes)
    message = pad_message(message)
    if debug:
        print("Padded message:", message.hex())

    # only 1 chunk; treat as 16 * 32 bits
    chunk = struct.unpack("<16I", message)
    if debug:
        print("Chunk:", " ".join(f"0x{i:08x}" for i in chunk))

    # get 128-bit (4*32-bit, 16-byte) hash
    state = hash_chunk(INIT_STATE, chunk)
    if debug:
        print(
            "State after hashing chunk:       ",
            " ".join(f"0x{i:08x}" for i in state)
        )

    # add initial state
    state = [
        (s + i) & 0xffff_ffff for (s, i) in zip(state, INIT_STATE)
    ]
    if debug:
        print(
            "State after adding initial state:",
            " ".join(f"0x{i:08x}" for i in state)
        )

    # final state = hash
    return struct.pack("<4I", *state)

def main():
    if len(sys.argv) != 2:
        sys.exit(
            "Compute the MD5 hash of a bytestring (0-7 bytes). Argument: "
            "bytestring in hexadecimal (\"\"=empty)."
        )

    try:
        message = bytes.fromhex(sys.argv[1])
    except ValueError:
        sys.exit("The argument is not a valid hexadecimal bytestring.")
    if len(message) > 7:
        sys.exit("The bytestring must be 7 bytes or less.")

    print(md5(message, True).hex())

# verified with hashlib
assert md5(b"").hex()        == "d41d8cd98f00b204e9800998ecf8427e"
assert md5(b"\x00").hex()    == "93b885adfe0da089cdf634904fd59f71"
assert md5(b"\xff").hex()    == "00594fd4f42ba43fc1ca0427a0576295"
assert md5(b"AB").hex()      == "b86fc6b051f63d73de262d4c34e3a0a9"
assert md5(b"ximaz").hex()   == "61529519452809720693702583126814"
assert md5(b"passwd").hex()  == "76a2173be6393254e72ffa4d6df1030a"
assert md5(b"ABCDEFG").hex() == "bb747b3df3130fe1ca4afa93fb7d97c9"

# if no chunks are hashed:
# state: 0xce8a4602 0xdf9b5712 0x3175b9fc 0x2064a8ec
# hexadecimal hash: 02468ace 12579bdf fcb97531 eca86420

main()
