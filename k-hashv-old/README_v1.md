# K-HASHV
A single header hash function with both vectorized and scalar versions. The function is quite fast when vectorized achieving approximately an average of **~9.6 GB/s** on a 7 year old Xeon E3-1230 v5.

Additionally, it also passes all the SMHasher hash function quality tests: https://github.com/rurban/smhasher

Moreover, it is quite easy to choose a new function at runtime by just using new seed as shown below:
```C
#include "khashv.h"

void foo() {
    /*
    code ....
    */
    khashvSeed seed;
    khashv_prep_seed64(&seed, a_64_bit_value);
    uint64_t hash = khashv64(&seed, your_data, data_len);
    /*
    code ....
    */
}
```
### Note
This is **not a cryptographic hash function**, and it should not be used in for such applications.

## Performance
When testing on 1.25 GB and 512 KB of random data I get the following on averages:
<table>
<thead><tr><th>Processor</th><th>1.25 GB Time</th><th>1.25 GB Speed</th> <th>512 KB Time</th><th>512 KB Speed</th><th>OS</th><th>Compiler</th><th>Type</th></tr></thead>
<tbody>
<tr> <td>Xeon E3-1230 v5</td> <td>0.1298 s</td> <td>9.6285 GB/s</td> <td>052.5107 us</td> <td>9.2987 GB/s</td><td>Linux</td><td>GCC 12.1.0</td><td><strong>Vectorized<strong></td></tr>
<tr> <td>Xeon E3-1230 v5</td> <td>1.1911 s</td> <td>1.0495 GB/s</td> <td>494.1932 us</td> <td>0.9880 GB/s</td><td>Linux</td><td>GCC 12.1.0</td><td><strong>Scalar<strong></td></tr>
<tr> <td>Xeon E3-1230 v5</td> <td>0.1418 s</td> <td>8.8142 GB/s</td> <td>055.9333 us</td> <td>8.7297 GB/s</td><td>Linux</td><td>Clang 14.0.6</td><td><strong>Vectorized<strong></td></tr>
<tr> <td>Ryzen 9 7900</td> <td>0.1227 s</td> <td>10.1881 GB/s</td> <td>046.0273 us</td> <td>10.6085 GB/s</td><td>Linux</td><td>GCC 12.2.1</td><td><strong>Vectorized<strong></td></tr>
<tr> <td>Ryzen 9 7900</td> <td>0.8693 s</td> <td>1.4379 GB/s</td> <td>375.0820 us</td> <td>1.3018 GB/s</td><td>Linux</td><td>GCC 12.2.1</td><td><strong>Scalar<strong></td></tr>
</tbody>
</table>

The scalar version is slower at a tad over ~1 GB/s on my system when compiling test_speed.c with gcc using `-O3`.
On windows Microsoft's compiler does not seem to generate as performant code from the intrinsics, but the GCC mingw64 compiler generates pretty comparable numbers for me at least.

Definitely, want to add other machines to this table. But if you are curious how it performs on your machine compile test_speed.c with `-O3 -march=native` and `-O3 -march=native -D KHASHV_SCALAR`.

## Functions
```C
// Prepares a seed from a 32-bit value
void khashv_prep_seed32(khashvSeed* seed_prepped, uint32_t seed)
```

```C
// Prepares a seed from a 64-bit value
void khashv_prep_seed64(khashvSeed* seed_prepped, uint64_t seed)
```

```C
// Sets  128-bits to be the seed
void khashv_prep_seed128(khashvSeed* seed_prepped, const uint32_t seed[4])
```

```C
// Produces a 32-bit hash from the input data
uint32_t khashv32(const khashvSeed* seed, const uint8_t* data, size_t data_len)
```

```C
// Produces a 64-bit hash from the input data
uint64_t khashv64(const khashvSeed* seed, const uint8_t* data, size_t data_len)
```

## K-HASHV 64 Output
Here is the output of the 64 bit hash of the integers \[0, 259199\] using 0x1dcedff1a8b17e89 as the seed.

<img src="./khashv64-seed-1dcedff1a8b17e89.png" alt="drawing" width="800"/>

## K-HASHV 32 Output

Here is the output of the 32 bit hash of the integers \[0, 518399\] using 0x6bb75f13 as the seed.

<img src="./khashv32-seed-6bb75f13.png" alt="drawing" width="800"/>

The output of the above images was generated by basically doing the following for a hash.

```C
for(int i = 0; i < sizeof(hash_bytes); i++) {
    pixel[img_offset + i].r = hash_bytes[i];
    pixel[img_offset + i].g = hash_bytes[i];
    pixel[img_offset + i].b = hash_bytes[i];
    pixel[img_offset + i].a = 255;
}
```

## Things TODO
When thinking about things to improve the code and hash function these are the first few things that come to mind for me.

1. The main thing would be try to get both Clang and MSVC to output code that runs as fast GCC or as close as possible. They both seem to do some silly things when compared to GCC losing some performance when looking at the generated assembly. Microsoft's compiler being the worst, and probably the fastest fix for me to implement would be to write some assembly code. However, it then would no longer be a single header file hash function since MSVC does not support inline assembly for 64-bit builds, and thusly would require a separate file.

2. Then probably consider using intrinsics for some other systems like ARM NEON, but the for now there is scalar code and code written using GCC's vector built-ins that will generate vectorized code for other architectures that GCC supports.

3. Probably, the next thing I could think of is to choose a better value for S1 and S2 that are used to basically substitute bytes. The current values where found randomly checking a small set of criteria. Mainly focusing on each bit of S1 and S2 as columns. Then Xor-ing them effectively creating an 8 bit input boolean function, and making sure the entire thing maps each input to a unique value. There likely are better values that could chosen, and criteria to look at that look at all bits at once. However, the search space is huge effectively 2^(2\*8\*16) possible permutations for S1 and S2. However, the current values do seem to work well, from my testing.

### Suggestions
I am open to any other suggestions or improvments.