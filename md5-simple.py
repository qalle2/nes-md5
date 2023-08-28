"""
a simplified MD5 implementation, based on https://github.com/qalle2/md5-algo
does not support messages longer than 7 bytes

hash of an empty string if only n rounds are run:
    n= 0: 02468ace 12579bdf fcb97531 eca86420
    n= 1: 77777777 fdd2ed94 87888888 7431eda8
    n= 2: ffffffff 663e63e5 7204db3d ffffffff
    n= 3: 8ace1257 ac16bed7 db6f508e ea7b52b5
    n= 4: 754a650c 8cdde30a 2148ab80 53e7c705
    n= 8: 45564899 72e88e8b d9356c8e 0a29683f
    n=16: f29fb15c f3474fc6 731c60f3 1d987b47
    n=24: 6d6163a4 a3feefd3 3c41fc83 5908ea78
    n=32: 3793b57a a80aee91 82a0ecb4 5168aabe
    n=40: 2fbebfc6 9e37aab5 b529852a 8ca2f5bb
    n=48: fe109ba6 46d94aba a02b9301 ca73d532
    n=56: 15c46abd eaa5450e ceade5d1 a1488344
    n=64: d41d8cd9 8f00b204 e9800998 ecf8427e
"""

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

    state = list(state)

    for r in range(64):
        if r < 16:
            t = state[2]
            t ^= state[3]
            t &= state[1]
            t ^= state[3]
        elif r < 32:
            t = state[1]
            t ^= state[2]
            t &= state[3]
            t ^= state[2]
        elif r < 48:
            t = state[1]
            t ^= state[2]
            t ^= state[3]
        else:
            t = state[3]
            t ^= 0xffffffff
            t |= state[1]
            t ^= state[2]

        t = add(t, state[0])
        t = add(t, SINES[r])
        t = add(t, chunk[CHUNK_INDEXES[r]])

        for i in range(SHIFTS[r]):
            t = rol(t)

        t = add(t, state[1])

        state[0] = state[3]
        state[3] = state[2]
        state[2] = state[1]
        state[1] = t

    return tuple(state)

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

main()
