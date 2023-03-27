#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <float.h>
#include <math.h>
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
        0x8b4c1a33, 0x485105dc, 0xaf1deb0e, 0x2d4a890c, 0x8349b700,
        0x29a3b3b9, 0xf1ed93ef, 0x8559b73f, 0x11452eff, 0xefa5fe1f,
        0x5834c363, 0xeb7224a5
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
    double gbs = get_gbs (avg, gigs);
    double sec = get_secs(avg);
    double fgbs = get_gbs(fastest, gigs);
    gbs  = round(gbs  * 10000.0) / 10000.0;
    sec  = round(sec  * 10000.0) / 10000.0;
    fgbs = round(fgbs * 10000.0) / 10000.0;
    printf(
        "Avg: %.4lf GB/s, Avg Time: %.4lf s, Fastest: %.4lf GB/s\n",
        gbs,
        sec,
        fgbs
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
        0x3b181e13, 0x6df3efe4, 0xa1472e2f, 0xe7fe7261, 0x85db611b,
        0x95b68b46, 0xa4738539, 0xc67cd2b3, 0x4630444d, 0xb357f7a3,
        0x60ba4613, 0x20d50be8, 0x5908392d, 0xd5c1411e, 0xa315f311,
        0xe92b8d4a, 0x3504718c, 0x78d5d987, 0xac324986, 0xa9c146a3,
        0xea4120ac, 0x1ab20115, 0xb4cf0fc0, 0x3726e7c6, 0x781b19b4,
        0x897a635f, 0x49c879a6, 0x414f698e, 0xef3c3c66, 0x668de11e,
        0xf6f2af8d, 0x6db89e5f, 0xa2621047, 0x26736838, 0xca8539cf,
        0xe1e92796, 0xbd178553, 0x31aedc2d, 0x41f4377f, 0x0683f7a2,
        0xff1d7f6f, 0x4a788c33, 0xb4823086, 0xf3b45106, 0xf2e12a97,
        0x1505b0e8, 0x32d16f9d, 0xa4ccbd11, 0x61f6aa54, 0x8dc4eb8d,
        0xe7ac77ca, 0xb00dd338, 0x9330ce85, 0xae721ca9, 0x236eb8a2,
        0xcd7aba61, 0x2fbd751e, 0x978edc2c, 0x09ef6175, 0x78d12480,
        0x08b21322, 0x02826493, 0x36244a76, 0xb7e1489c, 0x365c631f,
        0x08188ea8, 0x92bd6910, 0xa7cf34d0, 0x9b91a005, 0x8c7cfc38,
        0xf732ae18, 0x87f2f485, 0xa42d236d, 0x967880e3, 0xf04cb79d,
        0xfd9d613f, 0xfa7ae694, 0xfb680e60, 0x2de7c7c9, 0xa5979af7,
        0x6b24f6a3, 0xfebb25de, 0x3163a706, 0x7d8d0a35, 0xb5cacfcf,
        0xdf774e72, 0xd06db96e, 0x16d7e8db, 0xf1e368e7, 0x21efe8d5,
        0x59d6f29f, 0xb0ee28bc, 0x849b575e, 0x96887453, 0x2eabdd1f,
        0x3cdc8fa8
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
    double gbs = get_gbs  (avg, gigs);
    double usec = get_usecs(avg);
    double fgbs = get_gbs  (fastest, gigs);
    gbs  = round(gbs  * 10000.0) / 10000.0;
    usec = round(usec * 10000.0) / 10000.0;
    fgbs = round(fgbs * 10000.0) / 10000.0;
    printf(
        "Avg: %.4lf GB/s, Avg Time: %.4lf us, Fastest: %.4lf GB/s\n",
        gbs,
        usec,
        fgbs
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
