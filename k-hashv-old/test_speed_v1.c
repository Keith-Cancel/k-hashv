#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <float.h>
#include "khashv.h"

#if defined(__MINGW32__) || defined(_WIN32)
    #include <windows.h>

    #define get_timer(x) QueryPerformanceCounter(&x)

    typedef LARGE_INTEGER timer;

    uint64_t time_ns(timer* start, timer* stop) {
        LARGE_INTEGER freq;
        if(!QueryPerformanceFrequency(&freq)) {
            return UINT64_MAX;
        }
        double ns    = stop->QuadPart - start->QuadPart;
        double ratio = 1000000000.0; // 1 billion ns = 1 second
        ratio /=  (double)freq.QuadPart;
        ns *= ratio;
        return (uint64_t)ns;
    }

#else
    #include <time.h>
    #define get_timer(x) clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &x)
    typedef struct timespec timer;

    uint64_t time_ns(timer* start, timer* stop) {
        int secs = stop->tv_sec - start->tv_sec;
        if(secs > 0) {
            uint64_t t0_ns = start->tv_sec * 1000000000;
            uint64_t t1_ns = stop->tv_sec  * 1000000000;
            t0_ns += start->tv_nsec;
            t1_ns += stop->tv_nsec;
            return t1_ns - t0_ns;
        }
        return stop->tv_nsec - start->tv_nsec;
    }
#endif

#define MB_TO_BYTES(x) (1024ULL * 1024ULL * (x))

double get_gbs(double t_ns, double gigs) {
    t_ns /= 1000000000;
    return gigs / t_ns;
}

double get_secs(double t_ns) {
    return t_ns / 1000000000.0;
}

double get_usecs(double t_ns) {
    return t_ns / 1000.0;
}

void populate_memory(const khashvSeed* seed, uint8_t* bytes, size_t size) {
    printf("Populating Memory: ");
    // Use the hash to populate memory with pseudo random bytes
    uint64_t state[2] = { 0x4d9ef2f9a304588a,	0x58ca10a39947b63b };
    for(size_t i = 0; i < size; i += sizeof(uint64_t)) {
        if(i != 0 && (i & 0x1ffffff) == 0) {
            printf(".");
            fflush(stdout);
        }
        state[0] = khashv64(seed, (uint8_t*)state, sizeof(uint64_t) * 2);
        memcpy(bytes + i, state, sizeof(uint64_t));
    }
    puts(" Populated!");
}

int gig_tests(khashvSeed seed) {
    size_t   size  = MB_TO_BYTES(1280);
    uint8_t* bytes = malloc(size);
    if(bytes == NULL) {
        fprintf(stderr, "Can not allocate memory for test!\n");
        return 1;
    }
    populate_memory(&seed, bytes, size);

    double   gigs       = (double)size / (double)MB_TO_BYTES(1024);
    double   sum        = 0;
    double   fastest    = DBL_MAX;

    const uint32_t hashes[12] = {
        0xa9ca46b1, 0x8c9f5264, 0x2094ffd9, 0x93946e70, 0x9b71dd71,
        0x2abeec74, 0x6bca7368, 0x151fff30, 0xc4228495, 0xfad35669,
        0x9f151590, 0x20a4045b
    };

    printf("Tests on %.3lf GB block: ", gigs);
    fflush(stdout);
    for(unsigned i = 0; i < 12; i++) {
        timer t0;
        timer t1;
        get_timer(t0);
        uint32_t h = khashv32(&seed, bytes, size);
        get_timer(t1);

        if(h != hashes[i]) {
            printf("Bad Hash: 0x%08x, expected: 0x%08x !!!\n", h, hashes[i]);
        }

        double t = time_ns(&t0, &t1);
        if(t < fastest) {
            fastest = t;
        }
        sum += t;
        bytes[i] += 1;
    }

    double avg = sum / 12;
    printf(
        "Avg: %lf GB/s, Avg Time: %lf s, Fastest: %lf GB/s\n",
        get_gbs (avg, gigs),
        get_secs(avg),
        get_gbs (fastest, gigs)
    );
    fflush(stdout);
    free(bytes);
    return 0;
}

int half_mb_tests(khashvSeed seed) {
    size_t   size  = 1024 * 512;
    uint8_t* bytes = malloc(size);
    if(bytes == NULL) {
        fprintf(stderr, "Can not allocate memory for test!\n");
        return 1;
    }
    populate_memory(&seed, bytes, size);

    double   gigs       = (double)size / (double)MB_TO_BYTES(1024);
    double   sum        = 0;
    double   fastest    = DBL_MAX;
    unsigned count      = 96;
    const uint32_t hashes[96] = {
        0x249b844b, 0x852e481c, 0xf7ce4779, 0x5b1e79c0, 0xc6280b69,
        0x18aaed1f, 0x360a7b70, 0x6691373f, 0x62b0e7d2, 0x503f2a13,
        0x55784198, 0x0449e145, 0xc1fec259, 0xfdde4bcc, 0x3d040585,
        0x2d54b62c, 0x70f06c7e, 0xcc7a642f, 0xe784348b, 0xe360bb8a,
        0xd4653bab, 0x129aac4f, 0xdf09ac90, 0xc770d23f, 0x1865b60c,
        0x366d8ca9, 0x80b13f6f, 0x7317d810, 0x7816b809, 0x919adedb,
        0x92713259, 0xb15e9216, 0x4cca4cd2, 0xb0bda9b9, 0xa3eb6a63,
        0x1801f592, 0x7f6ebdfe, 0xcfd5f33c, 0x000c7082, 0x17265e0b,
        0x6ba10359, 0x8c74f4eb, 0x803f3c08, 0x4ba6860d, 0x0716f9fb,
        0x6e3c84ae, 0xe77a48f4, 0xc2374c75, 0x97f403ee, 0x3010b84b,
        0x560ba778, 0x83103235, 0xfd4adabf, 0xa436bcf0, 0xaa8f96dc,
        0x29922bec, 0xd5468b54, 0x4b1921b8, 0x2a8ce2d5, 0x86e336f4,
        0x5fab2354, 0x0e07c225, 0xb181782a, 0xe799459f, 0xcf9541fd,
        0xcd510976, 0xe70010ea, 0x6202cb22, 0x7d253b79, 0x4d047b53,
        0xbd26b2ba, 0xc1df8a17, 0x48a6ed87, 0xa980b22c, 0x16b27278,
        0xb5736e7c, 0x368bd0b9, 0xeee76414, 0xfe58e49d, 0xf3500e6d,
        0xb57df9f5, 0xb52a7ed6, 0xaca79612, 0xccc9f98a, 0xa7140bd0,
        0x7e45d2f9, 0xb91ddced, 0x9444f706, 0xa477bfb2, 0xcf7e1d5b,
        0xd95eab3c, 0x737fa6e5, 0x5f548e79, 0x46539426, 0xef41aa94,
        0xc0357213
    };

    printf("Tests on 512 KB block: ");
    fflush(stdout);
    for(unsigned i = 0; i < count; i++) {
        timer t0;
        timer t1;
        get_timer(t0);
        uint32_t h = khashv32(&seed, bytes, size);
        get_timer(t1);

        if(h != hashes[i]) {
            printf("Bad Hash: 0x%08x, expected: 0x%08x !!!\n", h, hashes[i]);
        }

        double t = time_ns(&t0, &t1);
        if(t < fastest) {
            fastest = t;
        }
        sum += t;
        bytes[i] += 1;
    }

    double avg = sum / count;
    printf(
        "Avg: %lf GB/s, Avg Time: %lf us, Fastest: %lf GB/s\n",
        get_gbs  (avg, gigs),
        get_usecs(avg),
        get_gbs  (fastest, gigs)
    );
    fflush(stdout);
    free(bytes);
    return 0;
}

int main(int argc, char** argv) {
    khashvSeed seed;
    khashv_prep_seed64(&seed, 0xa9c163c960d480fb);

    if(gig_tests(seed)) {
        return 1;
    }
    if(half_mb_tests(seed)) {
        return 1;
    }
    return 0;
}