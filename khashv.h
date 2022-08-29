/*
MIT License
Copyright (c) 2022 Keith-Cancel
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef K_HASH_V_H
#define K_HASH_V_H
#ifdef __cplusplus
extern "C" {
#define restrict
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// For MSVC compiler, no __SSE3__ macro
#if !defined(__SSE3__) && (defined(__AVX__) || defined(__AVX2__))
    #define __SSE3__
#endif
// Same deal
#if !defined(__SSE4_1__) && (defined(__AVX__) || defined(__AVX2__))
    #define __SSE4_1__
#endif

#if defined(__SSE3__)
    #include <immintrin.h>
    #if defined(__MINGW32__) || defined(_WIN32)
        #include <emmintrin.h>
    #endif
#endif

#if defined(__GNUC__) && !defined(__clang__)
#define KHASH_GCC_LEAST__(maj, min)     (__GNUC__ > maj || __GNUC__ == maj && __GNUC_MINOR__ >= min)
#else
#define KHASH_GCC_LEAST__(maj, min) 0
#endif

#if defined(__BYTE_ORDER__) && !defined(__BYTE_ORDER)
#define __BYTE_ORDER __BYTE_ORDER__
#endif

#if defined(__ORDER_LITTLE_ENDIAN__) && !defined(__LITTLE_ENDIAN)
#define __LITTLE_ENDIAN __ORDER_LITTLE_ENDIAN__
#endif

#if defined(__ORDER_BIG_ENDIAN__) && !defined(__BIG_ENDIAN)
#define __BIG_ENDIAN __ORDER_BIG_ENDIAN__
#endif


#if defined(__clang__) && defined(__has_attribute)
#define KHASH_CHK_ATTRIBUTE__(attr) __has_attribute(attr)
#elif defined(__has_attribute) && KHASH_GCC_LEAST__(5, 0)
#define KHASH_CHK_ATTRIBUTE__(attr) __has_attribute(attr)
#else
#define KHASH_CHK_ATTRIBUTE__(attr) 0
#endif

#if defined(__clang__) && defined(__has_builtin)
#define KHASH_CHK_BUILTIN__(built) __has_builtin(built)
#elif defined(__has_attribute) && KHASH_GCC_LEAST__(10, 1)
#define KHASH_CHK_BUILTIN__(built) __has_builtin(built)
#else
#define KHASH_CHK_BUILTIN__(built) 0
#endif

#if defined(_MSC_VER) && !defined(__clang__)
#define KHASH_FINLINE __forceinline
#define KHASH_BSWAP32(val) _byteswap_ulong(val)
#endif

#if !defined(KHASH_FINLINE) && (KHASH_CHK_ATTRIBUTE__(always_inline) || KHASH_GCC_LEAST__(3, 1))
#define KHASH_FINLINE __attribute__((always_inline)) inline
#endif

#if !defined(KHASH_BSWAP32) && (KHASH_CHK_BUILTIN__(__builtin_bswap32) || KHASH_GCC_LEAST__(4, 5))
#define KHASH_BSWAP32(val) __builtin_bswap32(val)
#endif

#if !defined(KHASH_OPT_SZ) && (KHASH_CHK_ATTRIBUTE__(optimize) || KHASH_GCC_LEAST__(4, 8))
    #define KHASH_OPT_SZ __attribute__((optimize("Os")))
#endif

#if !defined(KHASH_FINLINE)
#define KHASH_FINLINE inline
#endif

#if !defined(KHASH_OPT_SZ)
#define KHASH_OPT_SZ
#endif

#if !defined(KHASH_BSWAP32)
    #define KHASH_BSWAP32(val) ((val >> 24) | ((val >> 8) & 0xff00) | ((val << 8) & 0xff0000) |  (val << 24))
#endif

static KHASH_FINLINE int khashv_is_little_endian() {
#if defined(__BYTE_ORDER) && __BYTE_ORDER == __LITTLE_ENDIAN
    return 1;
#elif defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN
    return 0;
#elif defined(__BYTE_ORDER)
    #error "Mixed/Middle endian machine, you will need to write a custom byteswap routine"
#else
    // Otherwise hope the compiler's optimizer figures this is constant.
    // Also since the byte order macro does not exist there are
    // Middle-endian/Mixed endian machines out there but they are quite
    // rare/old. So I am not gonna worry about it since there are 24 or
    // 4! (four factorial) total endianess-es. So if the compiler does
    // not define __BYTE_ORDER, the hash output will be different, on
    // such machines, but the hash should still work fine.
    unsigned int x = 1;
    return *((char*)(&x)) == 1;
#endif
}

#define KHASH_ROTR32(x, n) (((x) >> (n)) | ((x) << (32 - (n))))

struct khashv_block_s {
    union {
        uint8_t  bytes[16];
        uint32_t words[4];
        #if defined(__SSE3__)
        __m128i  vec;
        #endif
    };
};

typedef struct khashv_block_s khashvBlock;
typedef struct khashv_block_s khashvSeed;

static const khashvBlock khash_v_init = {
    .words = {
        // Really this could basically be almost anything
        // So just using some bytes of the SHA-256 hashes
        // of 1, 2, 3, and 4
        0x7785459a,  // SHA256 of the byte 0x01, using the last 4 bytes
        0x6457d986,  // SHA256 of the byte 0x02, using the last 4 bytes
        0xadff29c5,  // SHA256 of the byte 0x03, using the last 4 bytes
        0x81c89e71,  // SHA256 of the byte 0x04, using the last 4 bytes
    }};

static const uint8_t khashv_s1[16] = {
    0x1c, 0x5d, 0xf8, 0xe3, 0xc1, 0x9c, 0xda, 0xb7,
    0x63, 0x91, 0x59, 0xb3, 0x2b, 0xa5, 0xee, 0x12,
};

static const uint8_t khashv_s2[16] = {
    0xef, 0xce, 0x66, 0xf3, 0xf6, 0x21, 0x42, 0xa5,
    0x11, 0xad, 0x5b, 0xc6, 0x72, 0x38, 0x95, 0x7a,
};

static const uint8_t khashv_xored[256] = {
    0xf3, 0xb2, 0x17, 0x0c, 0x2e, 0x73, 0x35, 0x58,
    0x8c, 0x7e, 0xb6, 0x5c, 0xc4, 0x4a, 0x01, 0xfd,
    0xd2, 0x93, 0x36, 0x2d, 0x0f, 0x52, 0x14, 0x79,
    0xad, 0x5f, 0x97, 0x7d, 0xe5, 0x6b, 0x20, 0xdc,
    0x7a, 0x3b, 0x9e, 0x85, 0xa7, 0xfa, 0xbc, 0xd1,
    0x05, 0xf7, 0x3f, 0xd5, 0x4d, 0xc3, 0x88, 0x74,
    0xef, 0xae, 0x0b, 0x10, 0x32, 0x6f, 0x29, 0x44,
    0x90, 0x62, 0xaa, 0x40, 0xd8, 0x56, 0x1d, 0xe1,
    0xea, 0xab, 0x0e, 0x15, 0x37, 0x6a, 0x2c, 0x41,
    0x95, 0x67, 0xaf, 0x45, 0xdd, 0x53, 0x18, 0xe4,
    0x3d, 0x7c, 0xd9, 0xc2, 0xe0, 0xbd, 0xfb, 0x96,
    0x42, 0xb0, 0x78, 0x92, 0x0a, 0x84, 0xcf, 0x33,
    0x5e, 0x1f, 0xba, 0xa1, 0x83, 0xde, 0x98, 0xf5,
    0x21, 0xd3, 0x1b, 0xf1, 0x69, 0xe7, 0xac, 0x50,
    0xb9, 0xf8, 0x5d, 0x46, 0x64, 0x39, 0x7f, 0x12,
    0xc6, 0x34, 0xfc, 0x16, 0x8e, 0x00, 0x4b, 0xb7,
    0x0d, 0x4c, 0xe9, 0xf2, 0xd0, 0x8d, 0xcb, 0xa6,
    0x72, 0x80, 0x48, 0xa2, 0x3a, 0xb4, 0xff, 0x03,
    0xb1, 0xf0, 0x55, 0x4e, 0x6c, 0x31, 0x77, 0x1a,
    0xce, 0x3c, 0xf4, 0x1e, 0x86, 0x08, 0x43, 0xbf,
    0x47, 0x06, 0xa3, 0xb8, 0x9a, 0xc7, 0x81, 0xec,
    0x38, 0xca, 0x02, 0xe8, 0x70, 0xfe, 0xb5, 0x49,
    0xda, 0x9b, 0x3e, 0x25, 0x07, 0x5a, 0x1c, 0x71,
    0xa5, 0x57, 0x9f, 0x75, 0xed, 0x63, 0x28, 0xd4,
    0x6e, 0x2f, 0x8a, 0x91, 0xb3, 0xee, 0xa8, 0xc5,
    0x11, 0xe3, 0x2b, 0xc1, 0x59, 0xd7, 0x9c, 0x60,
    0x24, 0x65, 0xc0, 0xdb, 0xf9, 0xa4, 0xe2, 0x8f,
    0x5b, 0xa9, 0x61, 0x8b, 0x13, 0x9d, 0xd6, 0x2a,
    0x89, 0xc8, 0x6d, 0x76, 0x54, 0x09, 0x4f, 0x22,
    0xf6, 0x04, 0xcc, 0x26, 0xbe, 0x30, 0x7b, 0x87,
    0x66, 0x27, 0x82, 0x99, 0xbb, 0xe6, 0xa0, 0xcd,
    0x19, 0xeb, 0x23, 0xc9, 0x51, 0xdf, 0x94, 0x68,
};

/* Scalar Code */

static KHASH_FINLINE void khashv_bswap_be_block_scalar(khashvBlock* in) {
    // Byte swapping is only needed if we are not on on a little endian system
    if (khashv_is_little_endian()) {
        return;
    }
    for(int i = 0; i < 4; i++) {
        in->words[i] =  KHASH_BSWAP32(in->words[i]);
    }
}

static KHASH_FINLINE void khashv_rotr_5_bytes_scalar(khashvBlock* in) {
    khashv_bswap_be_block_scalar(in);
    khashvBlock tmp1;
    khashvBlock tmp2;
    // Avoid aliasing issues by using memcpy between these union values.
    memcpy(tmp1.bytes, in->words, 16);
    for(int i = 0; i < 16; i++) {
        tmp2.bytes[i] = tmp1.bytes[(i + 5) & 0xf];
    }
    memcpy(in->words, tmp2.bytes, 16);
    khashv_bswap_be_block_scalar(in);
}

static KHASH_FINLINE void khashv_rotr_9_bytes_scalar(khashvBlock* in) {
    khashv_bswap_be_block_scalar(in);
    khashvBlock tmp1;
    khashvBlock tmp2;
    // Avoid aliasing issues by using memcpy between these union values.
    memcpy(tmp1.bytes, in->words, 16);
    for(int i = 0; i < 16; i++) {
        tmp2.bytes[i] = tmp1.bytes[(i + 9) & 0xf];
    }
    memcpy(in->words, tmp2.bytes, 16);
    khashv_bswap_be_block_scalar(in);
}

static KHASH_FINLINE void khashv_shl_13_block_scalar(khashvBlock* in) {
    for(int i = 0; i < 4; i++) {
        in->words[i] <<= 13;
    }
}

static KHASH_FINLINE void khashv_shr_3_block_scalar(khashvBlock* in) {
    for(int i = 0; i < 4; i++) {
        in->words[i] >>= 3;
    }
}

static KHASH_FINLINE void khashv_add_block_scalar(khashvBlock* restrict a, const khashvBlock* restrict b) {
    for(int i = 0; i < 4; i++) {
        a->words[i] += b->words[i];
    }
}

static KHASH_FINLINE void khashv_xor_block_scalar(khashvBlock* restrict a, const khashvBlock* restrict b) {
    for(int i = 0; i < 4; i++) {
        a->words[i] ^= b->words[i];
    }
}

// GCC and Clang with -O3 were vectorizing this quite poorly with -O3
// They could not detect that only a PSHUFB was needed and instead
// where generating tons of inserts and extracts from the vector
// registers. Thusly it was running slower than code that was not being
// vectorized on my machine. So I specify the optimization level directly.
// Tried a few other things to get GCC and Clang to generate more sane
// code or code using PSHUFB, but this seemed the cleanest.
// Example of what I mean: https://godbolt.org/z/PMnzsThPc
// Compared to this: https://godbolt.org/z/dWfjr7GWP
/*static KHASH_OPT_SZ void khashv_sub16(khashvBlock* tmp, const uint8_t sub[16]) {
    #if defined(__clang__)
        // Stop clang from being annoying!!!
        // The auto-vectorized code was worse at the time of writing this
        #pragma nounroll
        #pragma clang loop vectorize(disable)
        #pragma clang loop interleave(disable)
    #endif
    for (int i = 0; i < 16; i++) {
        tmp->bytes[i] = sub[tmp->bytes[i]];
    }
}

static KHASH_FINLINE void khashv_replace_scalar(khashvBlock* replace) {
    khashvBlock tmp;
    for (int i = 0; i < 16; i++) {
        tmp.bytes[i] = (replace->bytes[i] >> 4);
        replace->bytes[i] &= 0x0f;
    }
    khashv_sub16(replace, khashv_s1);
    khashv_sub16(&tmp, khashv_s2);
    for (int i = 0; i < 16; i++) {
        replace->bytes[i] ^= tmp.bytes[i];
    }
}*/
// Similar issue as the commented out code so stop the optimizers
// from getting crazy
static KHASH_OPT_SZ void khashv_replace_scalar(khashvBlock* replace) {
    khashvBlock tmp;
    memcpy(tmp.bytes, replace->words, 16);
    #if defined(__clang__)
        // Stop clang from being annoying!!!
        // The auto-vectorized code was worse at the time of writing this
        #pragma nounroll
        #pragma clang loop vectorize(disable)
        #pragma clang loop interleave(disable)
    #endif
    for(int i = 0; i < 16; i++) {
        tmp.bytes[i] = khashv_xored[tmp.bytes[i]];
    }
    memcpy(replace->words, tmp.bytes, 16);
}

static KHASH_FINLINE void khashv_mix_words_scalar(khashvBlock* in) {
    unsigned rots[4] = { 5, 7, 11, 17 };
    khashvBlock tmp  = { 0 };
    for (int i = 0; i < 4; i++) {
        unsigned rot = rots[i];
        tmp = *in;
        khashv_rotr_5_bytes_scalar(&tmp);
        khashv_add_block_scalar(&tmp, in);
        for (int j = 0; j < 4; j++) {
            tmp.words[j] = KHASH_ROTR32(tmp.words[j], rot);
        }
        khashv_xor_block_scalar(in, &tmp);
    }
}

static void khashv_hash_scalar(khashvBlock* hash, const uint8_t* data, size_t data_len) {
    hash->words[0] ^= data_len;
    // size_t is bigger than 32 bits
    #if defined(SIZE_MAX) && SIZE_MAX > 4294967295
        hash->words[1] ^= data_len >> 32;
    #endif

    khashvBlock tmp_1;
    khashvBlock tmp_2;
    khashvBlock tmp_h = *hash;

    const uint8_t* end = data + (data_len & ~((size_t)15));

    while (data < end) {
        memcpy(&tmp_2, data, 16);
        khashv_replace_scalar(&tmp_2);
        memcpy(&tmp_1.words, tmp_2.bytes, 16);

        khashv_bswap_be_block_scalar(&tmp_1);

        tmp_2 = tmp_1;
        //khashv_shl_13_block_scalar(&tmp_2);
        //khashv_add_block_scalar(&tmp_2, &tmp_1);
        for(int i = 0; i < 4; i++) {
            tmp_2.words[i] *= 8193;
        }
        khashv_xor_block_scalar(&tmp_h, &tmp_2);
        khashv_rotr_5_bytes_scalar(&tmp_h);
        khashv_add_block_scalar(&tmp_h, &tmp_1);

        tmp_2 = tmp_h;
        khashv_shr_3_block_scalar(&tmp_2);
        khashv_rotr_9_bytes_scalar(&tmp_h);
        khashv_add_block_scalar(&tmp_h, &tmp_2);

        data += 16;
    }

    unsigned trailing = data_len & 0xf;
    if(trailing) {
        memset(&tmp_2, 0, 16);

        memcpy(&tmp_2.bytes, data, trailing);
        khashv_replace_scalar(&tmp_2);
        memcpy(&tmp_1.words, tmp_2.bytes, 16);

        khashv_bswap_be_block_scalar(&tmp_1);

        tmp_2 = tmp_1;
        //khashv_shl_13_block_scalar(&tmp_2);
        //khashv_add_block_scalar(&tmp_2, &tmp_1);
        for(int i = 0; i < 4; i++) {
            tmp_2.words[i] *= 8193;
        }
        khashv_xor_block_scalar(&tmp_h, &tmp_2);
        khashv_rotr_5_bytes_scalar(&tmp_h);
        khashv_add_block_scalar(&tmp_h, &tmp_1);

        tmp_2 = tmp_h;
        khashv_shr_3_block_scalar(&tmp_2);
        khashv_rotr_9_bytes_scalar(&tmp_h);
        khashv_add_block_scalar(&tmp_h, &tmp_2);

    }
    khashv_mix_words_scalar(&tmp_h);
    *hash = tmp_h;
}

static void khashv_prep_seed32_scalar(khashvSeed* seed_prepped, uint32_t seed) {
    *seed_prepped = khash_v_init;
    seed_prepped->words[0] ^= seed;
    khashv_mix_words_scalar(seed_prepped);
}

static void khashv_prep_seed64_scalar(khashvSeed* seed_prepped, uint64_t seed) {
    *seed_prepped = khash_v_init;
    seed_prepped->words[0] ^= seed;
    khashv_mix_words_scalar(seed_prepped);
    // Do it again with the other part to make it different than the 32 bit seed.
    seed_prepped->words[1] ^= seed >> 32;
    khashv_mix_words_scalar(seed_prepped);
}

static void khashv_prep_seed128_scalar(khashvSeed* seed_prepped, const uint32_t seed[4]) {
    for(int i = 0; i < 4; i++) {
        seed_prepped->words[i] = seed[i];
    }
}

static uint32_t khashv32_scalar(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    khashvBlock h = *seed;
    khashv_hash_scalar(&h, data, data_len);
    return h.words[3];
}

static uint64_t khashv64_scalar(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    khashvBlock h = *seed;
    khashv_hash_scalar(&h, data, data_len);
    uint64_t r = h.words[1];
    r <<= 32;
    r  |= h.words[0];
    return r;
}

/* Vectorization for and Intel/AMD */

#if defined(__SSE3__)

#define KHASH_VECTOR 1

#if !defined(_MSC_VER) && !defined(__clang__) && !(KHASH_GCC_LEAST__(11, 0))
    static KHASH_FINLINE __m128i _mm_loadu_si32(const void* data) {
        uint32_t val;
        memcpy(&val, data, sizeof(uint32_t));
        return _mm_cvtsi32_si128(val);
    }
    static KHASH_FINLINE __m128i _mm_loadu_si16(const void* data) {
        uint32_t val = 0;
        memcpy(&val, data, sizeof(uint16_t));
        return _mm_cvtsi32_si128(val);
    }
#endif

#if !defined(_MSC_VER) && !defined(__clang__) && !(KHASH_GCC_LEAST__(9, 1))
    static KHASH_FINLINE __m128i _mm_loadu_si64(const void* data) {
        uint64_t val = 0;
        memcpy(&val, data, sizeof(uint64_t));
        return _mm_cvtsi64_si128(val);
    }
#endif

static KHASH_FINLINE __m128i khashv_mix_words_vector(__m128i val) {
    __m128i tmp1;
    __m128i tmp2;

    tmp1 = _mm_alignr_epi8(val, val, 5);
    tmp1 = _mm_add_epi32(val, tmp1);
    #if defined(__AVX512VL__)
        tmp1 = _mm_ror_epi32(tmp1, 5);
        val  = _mm_xor_si128(val, tmp1);
    #else
        tmp2 = _mm_srli_epi32(tmp1, 5);
        tmp1 = _mm_slli_epi32(tmp1, 27);
        val  = _mm_xor_si128(val, tmp2);
        val  = _mm_xor_si128(val, tmp1);
    #endif

    tmp1 = _mm_alignr_epi8(val, val, 5);
    tmp1 = _mm_add_epi32(val, tmp1);
    #if defined(__AVX512VL__)
        tmp1 = _mm_ror_epi32(tmp1, 7);
        val  = _mm_xor_si128(val, tmp1);
    #else
        tmp2 = _mm_srli_epi32(tmp1, 7);
        tmp1 = _mm_slli_epi32(tmp1, 25);
        val  = _mm_xor_si128(val, tmp2);
        val  = _mm_xor_si128(val, tmp1);
    #endif

    tmp1 = _mm_alignr_epi8(val, val, 5);
    tmp1 = _mm_add_epi32(tmp1, val);
    #if defined(__AVX512VL__)
        tmp1 = _mm_ror_epi32(tmp1, 11);
        val  = _mm_xor_si128(val, tmp1);
    #else
        tmp2 = _mm_srli_epi32(tmp1, 11);
        tmp1 = _mm_slli_epi32(tmp1, 21);
        val  = _mm_xor_si128(val, tmp2);
        val  = _mm_xor_si128(val, tmp1);
    #endif

    tmp1 = _mm_alignr_epi8(val, val, 5);
    tmp1 = _mm_add_epi32(tmp1, val);
    #if defined(__AVX512VL__)
        tmp1 = _mm_ror_epi32(tmp1, 17);
        val  = _mm_xor_si128(val, tmp1);
    #else
        tmp2 = _mm_srli_epi32(tmp1, 17);
        tmp1 = _mm_slli_epi32(tmp1, 15);
        val  = _mm_xor_si128(val, tmp2);
        val  = _mm_xor_si128(val, tmp1);
    #endif

    return val;
}

static KHASH_FINLINE __m128i khashv_part_load_vector(const uint8_t* data, size_t len) {
    __m128i tmp  = { 0 };
    __m128i tmp2 = { 0 };
    switch(len) {
        case 1:
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[0], 0);
            #else
                tmp = _mm_cvtsi32_si128(data[0]);
            #endif
            break;
        case 2:
            tmp = _mm_loadu_si16(data);
            break;
        case 3:
            tmp = _mm_loadu_si16(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[2], 2);
            #else
                tmp = _mm_insert_epi16(tmp, data[2], 1);
            #endif
            break;
        case 4:
            tmp = _mm_loadu_si32(data);
            break;
        case 5:
            tmp = _mm_loadu_si32(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[4], 4);
            #else
                tmp = _mm_insert_epi16(tmp, data[4], 2);
            #endif
            break;
        case 6:
            tmp = _mm_loadu_si32(data);
            tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 4), 2);
            break;
        case 7:
            tmp = _mm_loadu_si32(data);
            tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 4), 2);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[6], 6);
            #else
                tmp = _mm_insert_epi16(tmp, data[6], 3);
            #endif
            break;
        case 8:
            tmp = _mm_loadu_si64(data);
            break;
        case 9:
            tmp = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[8], 8);
            #else
                tmp = _mm_insert_epi16(tmp, data[8], 4);
            #endif
            break;
        case 10:
            tmp = _mm_loadu_si64(data);
            tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 8), 4);
            break;
        case 11:
            tmp = _mm_loadu_si64(data);
            tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 8), 4);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi8(tmp, data[10], 10);
            #else
                tmp = _mm_insert_epi16(tmp, data[10], 5);
            #endif
            break;
        case 12:
            tmp = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi32(tmp, *(uint32_t*)(data + 8), 2);
            #else
                tmp2 = _mm_loadu_si32(data + 8);
                tmp2 = _mm_shuffle_epi32(tmp2, 0x4f);
                tmp  = _mm_or_si128(tmp, tmp2);
            #endif
            break;
        case 13:
            tmp = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi32(tmp, *(uint32_t*)(data + 8), 2);
                tmp = _mm_insert_epi8(tmp, data[12], 12);
            #else
                tmp2 = _mm_loadu_si32(data + 8);
                tmp2 = _mm_insert_epi16(tmp2, data[12], 2);
                tmp2 = _mm_shuffle_epi32(tmp2, 0x4f);
                tmp  = _mm_or_si128(tmp, tmp2);
            #endif
            break;
        case 14:
            tmp = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi32(tmp, *(uint32_t*)(data + 8), 2);
                tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 12), 6);
            #else
                tmp2 = _mm_loadu_si32(data + 8);
                tmp2 = _mm_insert_epi16(tmp2, *(uint16_t*)(data + 12), 6);
                tmp2 = _mm_shuffle_epi32(tmp2, 0x4f);
                tmp  = _mm_or_si128(tmp, tmp2);
            #endif
            break;
        case 15:
            tmp = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi32(tmp, *(uint32_t*)(data + 8), 2);
                tmp = _mm_insert_epi16(tmp, *(uint16_t*)(data + 12), 6);
                tmp = _mm_insert_epi8(tmp, data[14], 14);
            #else
                tmp2 = _mm_loadu_si32(data + 8);
                tmp2 = _mm_insert_epi16(tmp2, *(uint16_t*)(data + 12), 6);
                tmp2 = _mm_insert_epi16(tmp2, data[14], 7);
                tmp2 = _mm_shuffle_epi32(tmp2, 0x4f);
                tmp  = _mm_or_si128(tmp, tmp2);
            #endif
            break;
        case 16:
            tmp  = _mm_loadu_si64(data);
            #if defined(__SSE4_1__)
                tmp = _mm_insert_epi64(tmp, *(uint64_t*)(data + 8), 1);
            #else
                tmp2 = _mm_loadu_si64(data + 8);
                tmp  = _mm_unpacklo_epi64(tmp, tmp2);
            #endif
            break;
    }
    return tmp;
}

static const uint8_t khashv_shuff[16] = {
    0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x00,
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
};

static __m128i khashv_hash_vector(__m128i hash, const uint8_t* data, size_t data_len) {
    const __m128i s1     = _mm_loadu_si128((const __m128i*)khashv_s1);
    const __m128i s2     = _mm_loadu_si128((const __m128i*)khashv_s2);
    const __m128i shuff  = _mm_loadu_si128((const __m128i*)khashv_shuff);
    const __m128i mask   = _mm_set1_epi32(0x0f0f0f0f);

    __m128i       tmp_1;
    __m128i       tmp_2;

    #if defined(SIZE_MAX) && SIZE_MAX > 4294967295
        tmp_1 = _mm_cvtsi64_si128(data_len);
    #else
        tmp_1 = _mm_cvtsi32_si128(data_len);
    #endif
    hash = _mm_xor_si128(tmp_1, hash);

    const uint8_t* end  = data + (data_len & ~((size_t)15));
    const uint8_t* end2 = data + data_len;
    while(data_len > 16 && data < end) {
        tmp_1 = _mm_lddqu_si128((const __m128i*)data);
        tmp_2 = _mm_srli_epi32  (tmp_1, 4);

        tmp_1 = _mm_and_si128   (tmp_1, mask);
        tmp_2 = _mm_and_si128   (tmp_2, mask);
        tmp_1 = _mm_shuffle_epi8(s1,    tmp_1);
        tmp_2 = _mm_shuffle_epi8(s2,    tmp_2);
        tmp_1 = _mm_xor_si128   (tmp_1, tmp_2);

        tmp_2 = _mm_slli_epi32 (tmp_1, 13);
        tmp_2 = _mm_add_epi32  (tmp_1, tmp_2);
        tmp_2 = _mm_xor_si128  (hash,  tmp_2);
        tmp_2 = _mm_alignr_epi8(tmp_2, tmp_2, 5);
        hash  = _mm_add_epi32  (tmp_2, tmp_1);

        tmp_2 = _mm_srli_epi32(hash, 3);
        tmp_1 = _mm_shuffle_epi8(hash, shuff);
        hash  = _mm_add_epi32 (tmp_2, tmp_1);

        data += 16;
    }
    uintptr_t trailing = end2 - data;
    if(trailing) {
        tmp_1 = khashv_part_load_vector(data, trailing);
        tmp_2 = _mm_srli_epi32  (tmp_1, 4);

        tmp_1 = _mm_and_si128   (tmp_1, mask);
        tmp_2 = _mm_and_si128   (tmp_2, mask);
        tmp_1 = _mm_shuffle_epi8(s1,    tmp_1);
        tmp_2 = _mm_shuffle_epi8(s2,    tmp_2);
        tmp_1 = _mm_xor_si128   (tmp_1, tmp_2);

        tmp_2 = _mm_slli_epi32 (tmp_1, 13);
        tmp_2 = _mm_add_epi32  (tmp_1, tmp_2);
        tmp_2 = _mm_xor_si128  (hash,  tmp_2);
        tmp_2 = _mm_alignr_epi8(tmp_2, tmp_2, 5);
        hash  = _mm_add_epi32  (tmp_2, tmp_1);

        tmp_2 = _mm_srli_epi32(hash, 3);
        tmp_1 = _mm_shuffle_epi8(hash, shuff);
        hash  = _mm_add_epi32 (tmp_2, tmp_1);
    }
    hash = khashv_mix_words_vector(hash);
    return hash;
}

static void khashv_prep_seed32_vector(khashvSeed* seed_prepped, uint32_t seed) {
    __m128i s = _mm_loadu_si128((const __m128i*)&khash_v_init);
    s = _mm_xor_si128(s, _mm_cvtsi32_si128(seed));
    seed_prepped->vec = khashv_mix_words_vector(s);
}

static void khashv_prep_seed64_vector(khashvSeed* seed_prepped, uint64_t seed) {
    __m128i s = _mm_loadu_si128((const __m128i*)&khash_v_init);
    __m128i t = _mm_cvtsi32_si128(seed >> 32);
    s = _mm_xor_si128(s, _mm_cvtsi32_si128(seed));
    s = khashv_mix_words_vector(s);
    s = _mm_xor_si128(s, _mm_shuffle_epi32(t, 0xf3));
    seed_prepped->vec = khashv_mix_words_vector(s);
}

static void khashv_prep_seed128_vector(khashvSeed* seed_prepped, const uint32_t seed[4]) {
    seed_prepped->vec = _mm_loadu_si128((const __m128i*)seed);
}

static uint32_t khashv32_vector(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    __m128i h = khashv_hash_vector(seed->vec, data, data_len);
    // using word[3] to avoid any overlap with with the
    // 64 bit hash which uses words [0] and [1], this ensures
    // the 2 bit outputs should behave differently when used.
    #if defined(__SSE4_1__)
        return _mm_extract_epi32(h, 3);
    #else
        h = _mm_shuffle_epi32(h, 0xff);
        return _mm_cvtsi128_si32(h);
    #endif
}

static uint64_t khashv64_vector(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    __m128i h = khashv_hash_vector(seed->vec, data, data_len);
    return _mm_cvtsi128_si64(h);
}

#endif

/* Vectorization via GCCs Vectorization builtins */
// Handy since it allows vectorization without explicit intrinsics
// for a particular CPU.

#if !defined(KHASH_VECTOR) && KHASH_GCC_LEAST__(6, 1)

#define KHASH_VECTOR 1

typedef uint8_t  kv16ui __attribute__((vector_size(16)));
typedef uint32_t kv4ui  __attribute__((vector_size(16)));

static KHASH_FINLINE kv16ui khashv_sub_s1_gcc(kv16ui in) {
    const kv16ui mask = {
        0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf,
        0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf, 0xf
    };
    const kv16ui sub = {
        0x1c, 0x5d, 0xf8, 0xe3, 0xc1, 0x9c, 0xda, 0xb7,
        0x63, 0x91, 0x59, 0xb3, 0x2b, 0xa5, 0xee, 0x12,
    };
    in &= mask;
    return __builtin_shuffle(sub, in);
}

static KHASH_FINLINE kv16ui khashv_sub_s2_gcc(kv16ui in) {
    const kv16ui sub = {
        0xef, 0xce, 0x66, 0xf3, 0xf6, 0x21, 0x42, 0xa5,
        0x11, 0xad, 0x5b, 0xc6, 0x72, 0x38, 0x95, 0x7a,
    };
    in >>= 4;
    return __builtin_shuffle(sub, in);
}

static KHASH_FINLINE kv4ui khashv_rotr_5_bytes_gcc(kv4ui input) {
    const kv16ui rotrLE = {
        0x5, 0x6, 0x7, 0x8, 0x9, 0xa, 0xb, 0xc,
        0xd, 0xe, 0xf, 0x0, 0x1, 0x2, 0x3, 0x4
    };
    const kv16ui rotrBE = {
        0xb, 0x4, 0x5, 0x6, 0xf, 0x8, 0x9, 0xa,
        0x3, 0xc, 0xd, 0xe, 0x7, 0x0, 0x1, 0x2
    };
    kv16ui tmp;
    memcpy(&tmp, &input, 16);
    if (khashv_is_little_endian()) {
        tmp = __builtin_shuffle(tmp, rotrLE);
    } else {
        tmp = __builtin_shuffle(tmp, rotrBE);
    }
    memcpy(&input, &tmp, 16);
    return input;
}

static KHASH_FINLINE kv4ui khashv_rotr_9_bytes_gcc(kv4ui input) {
    const kv16ui rotrLE = {
        0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0x0,
        0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8,
    };
    const kv16ui rotrBE = {
        0xf, 0x8, 0x9, 0xa, 0x3, 0xc, 0xd, 0xe,
        0x7, 0x0, 0x1, 0x2, 0xb, 0x4, 0x5, 0x6,
    };
    kv16ui tmp;
    memcpy(&tmp, &input, 16);
    if (khashv_is_little_endian()) {
        tmp = __builtin_shuffle(tmp, rotrLE);
    } else {
        tmp = __builtin_shuffle(tmp, rotrBE);
    }
    memcpy(&input, &tmp, 16);
    return input;
}

static KHASH_FINLINE kv4ui khash_byteswap_vec32_gcc( kv4ui input ) {
    const kv16ui bswap32 = {
        0x3, 0x2, 0x1, 0x0, 0x7, 0x6, 0x5, 0x4,
        0xb, 0xa, 0x9, 0x8, 0xf, 0xe, 0xd, 0xc,
    };
    kv16ui b;

    memcpy(&b, &input, 16);
    b = __builtin_shuffle(b, bswap32);
    memcpy(&input, &b, 16);
    return input;
}

static KHASH_FINLINE kv4ui khashv_replace_gcc(kv4ui input) {
    kv16ui s1;
    kv16ui s2;
    memcpy(&s1, &input, 16);
    s2 = khashv_sub_s2_gcc(s1);
    s1 = khashv_sub_s1_gcc(s1);
    s1 ^= s2;
    memcpy(&input, &s1, 16);
    return input;
}

static KHASH_FINLINE kv4ui khashv_mix_words_gcc(kv4ui val) {
    const unsigned rots[4] = { 5, 7, 11, 17 };
    for (int i = 0; i < 4; i++) {
        unsigned rot = rots[i];
        kv4ui tmp = val;
        tmp  = khashv_rotr_5_bytes_gcc(tmp);
        tmp += val;
        tmp  = (tmp >> rot) | (tmp << (32 - rot));
        val ^= tmp;
    }
    return val;
}

static KHASH_FINLINE kv4ui khashv_hash_block_gcc(kv4ui hash, kv4ui input) {
    kv4ui tmp_1 = khashv_replace_gcc(input);
    if (!khashv_is_little_endian()) {
        tmp_1 = khash_byteswap_vec32_gcc(tmp_1);
    }
    kv4ui tmp_2 = tmp_1 * 8193;
    tmp_2 ^= hash;
    tmp_2  = khashv_rotr_5_bytes_gcc(tmp_2);
    hash   = tmp_1 + tmp_2;

    tmp_2  = hash >> 3;
    tmp_1  = khashv_rotr_9_bytes_gcc(hash);
    hash   = tmp_1 + tmp_2;
    return hash;
}

static KHASH_FINLINE kv4ui khashv_hash_gcc(kv4ui hash, const uint8_t* data, size_t data_len) {
    hash[0] ^= data_len;
    #if defined(SIZE_MAX) && SIZE_MAX > 4294967295
        hash[1] ^= data_len >> 32;
    #endif

    kv4ui data_v;
    const uint8_t* end = data + (data_len & ~((size_t)15));
    while (data < end) {
        memcpy(&data_v, data, 16);
        hash = khashv_hash_block_gcc(hash, data_v);
        data += 16;
    }

    unsigned trailing = data_len & 0xf;
    if(trailing) {
        memset(&data_v, 0, 16);
        memcpy(&data_v, data, trailing);
        hash = khashv_hash_block_gcc(hash, data_v);
    }
    return khashv_mix_words_gcc(hash);
}


static void khashv_prep_seed32_vector(khashvSeed* seed_prepped, uint32_t seed) {
    kv4ui s;
    memcpy(&s, &khash_v_init, 16);
    s[0] ^= seed;
    s = khashv_mix_words_gcc(s);
    memcpy(seed_prepped, &s, 16);
}

static void khashv_prep_seed64_vector(khashvSeed* seed_prepped, uint64_t seed) {
    kv4ui s;
    memcpy(&s, &khash_v_init, 16);
    s[0] ^= seed;
    s = khashv_mix_words_gcc(s);
    s[1] ^= seed >> 32;
    s = khashv_mix_words_gcc(s);
    memcpy(seed_prepped, &s, 16);
}

static void khashv_prep_seed128_vector(khashvSeed* seed_prepped, const uint32_t seed[4]) {
    memcpy(seed_prepped->words, seed, 16);
}

static uint32_t khashv32_vector(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    kv4ui h;
    memcpy(&h, seed, 16);
    h = khashv_hash_gcc(h, data, data_len);
    return h[3];
}

static uint64_t khashv64_vector(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
    kv4ui h;
    memcpy(&h, seed, 16);
    h = khashv_hash_gcc(h, data, data_len);
    uint64_t ret;
    if (khashv_is_little_endian()) {
        memcpy(&ret, &h, 8);
    } else {
        ret = h[1];
        ret = (ret << 32) | h[0];
    }
    return ret;
}

#endif

#if defined(KHASH_VECTOR) && !defined(KHASHV_SCALAR)

    static void khashv_prep_seed32(khashvSeed* seed_prepped, uint32_t seed) {
        khashv_prep_seed32_vector(seed_prepped, seed);
    }

    static void khashv_prep_seed64(khashvSeed* seed_prepped, uint64_t seed) {
        khashv_prep_seed64_vector(seed_prepped, seed);
    }

    static void khashv_prep_seed128(khashvSeed* seed_prepped, const uint32_t seed[4]) {
        khashv_prep_seed128_vector(seed_prepped, seed);
    }

    static uint32_t khashv32(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
        return khashv32_vector(seed, data, data_len);
    }

    static uint64_t khashv64(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
        return khashv64_vector(seed, data, data_len);
    }

#else

    static void khashv_prep_seed32(khashvSeed* seed_prepped, uint32_t seed) {
        khashv_prep_seed32_scalar(seed_prepped, seed);
    }

    static void khashv_prep_seed64(khashvSeed* seed_prepped, uint64_t seed) {
        khashv_prep_seed64_scalar(seed_prepped, seed);
    }

    static void khashv_prep_seed128(khashvSeed* seed_prepped, const uint32_t seed[4]) {
        khashv_prep_seed128_scalar(seed_prepped, seed);
    }

    static uint32_t khashv32(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
        return khashv32_scalar(seed, data, data_len);
    }

    static uint64_t khashv64(const khashvSeed* seed, const uint8_t* data, size_t data_len) {
        return khashv64_scalar(seed, data, data_len);
    }

#endif


#ifdef __cplusplus
}
#endif
#endif
