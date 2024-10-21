
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <string>

typedef _Float16 half;

double half_to_double(_Float16 a) {
    const int srcSigBits = 10;
    const int srcBits = 16;
    const int srcExpBits = srcBits - srcSigBits - 1;
    const int srcInfExp = (1 << srcExpBits) - 1;
    const int srcExpBias = srcInfExp >> 1;
    
    static_assert(srcExpBits == 5, "");
    static_assert(srcInfExp == 31, "");
    static_assert(srcExpBias == 15, "");

    const uint16_t srcMinNormal = uint16_t(1) << srcSigBits;
    const uint16_t srcInfinity = uint16_t(srcInfExp) << srcSigBits;
    const uint16_t srcSignMask = uint16_t(1) << (srcSigBits + srcExpBits);
    const uint16_t srcAbsMask = srcSignMask - 1;
    const uint16_t srcQNaN = uint16_t(1) << (srcSigBits - 1);
    const uint16_t srcNaNCode = srcQNaN - 1;

    static_assert(srcMinNormal == 0x0400, "");
    static_assert(srcInfinity  == 0x7c00, "");
    static_assert(srcSignMask  == 0x8000, "");
    static_assert(srcAbsMask   == 0x7fff, "");
    static_assert(srcQNaN      == 0x0200, "");
    static_assert(srcNaNCode   == 0x01ff, "");

    const uint64_t dstSigBits = 52;
    const uint64_t dstBits = 64;
    const uint64_t dstExpBits = dstBits - dstSigBits - 1;
    const uint64_t dstInfExp = (1 << dstExpBits) - 1;
    const uint64_t dstExpBias = dstInfExp >> 1;

    static_assert(dstExpBits == 11, "");
    static_assert(dstInfExp  == 0x7ff, "");
    static_assert(dstExpBias == 0x3ff, "");

    const uint64_t dstMinNormal = uint64_t(1) << dstSigBits;

    static_assert(dstMinNormal == 0x0010000000000000, "");

    uint16_t aRep;
    std::memcpy(&aRep, &a, sizeof(a));

    const uint16_t aAbs = aRep & srcAbsMask;
    const uint16_t sign = aRep & srcSignMask;
    uint64_t absResult;

    if (static_cast<uint16_t>(aAbs - srcMinNormal) < srcInfinity - srcMinNormal) {
        // Normal
        absResult = static_cast<uint64_t>(aAbs) << (dstSigBits - srcSigBits);
        absResult += static_cast<uint64_t>(dstExpBias - srcExpBias) << dstSigBits;
        static_assert(static_cast<uint64_t>(dstExpBias - srcExpBias) << dstSigBits == 0x3F00000000000000, "");

    } else if (aAbs >= srcInfinity) {
        // Infinity or NaN
        absResult = static_cast<uint64_t>(dstInfExp) << dstSigBits;
        absResult |= static_cast<uint64_t>(aAbs & srcQNaN) << (dstSigBits - srcSigBits);
        absResult |= static_cast<uint64_t>(aAbs & srcNaNCode) << (dstSigBits - srcSigBits);
    } else if (aAbs) {
        // Denormal
        const int scale = __builtin_clz(aAbs) - __builtin_clz(srcMinNormal);
        static_assert(__builtin_clz(srcMinNormal) == 21, "");
        const int resultExponent = dstExpBias - srcExpBias - scale + 1;
        assert(22 <= __builtin_clz(aAbs) && __builtin_clz(aAbs) <= 31);
        assert(1 <= scale && scale <= 10);
        absResult = static_cast<uint64_t>(aAbs) << (dstSigBits - srcSigBits + scale);
        absResult ^= dstMinNormal;
        absResult |= static_cast<uint64_t>(resultExponent) << dstSigBits;
    } else {
        // Zero
        absResult = 0;
    }

    uint64_t result = absResult | static_cast<uint64_t>(sign) << (dstBits - srcBits);
    double resultf;
    std::memcpy(&resultf, &result, sizeof(result));
    return resultf;
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("Usage: %s <float>\n", argv[0]);
        return 1;
    }

    const uint16_t n = std::stoi(argv[1], nullptr, 16);
    _Float16 h;
    std::memcpy(&h, &n, sizeof(n));
    
    printf("%#.4x -> %.17g\n", n, half_to_double(h));
    printf("%#.4x -> %.17g\n", n, static_cast<double>(h));

    /* double n = std::strtod(argv[1], nullptr); */
    /* printf("n = %f\n", n); */

    /* return 0; */

    /* _Float16 h = static_cast<_Float16>(n); */
    FILE *out = std::fopen("out.bin", "wb");
    assert(out);

    for (uint32_t j = 0; j <= 0xffff; j++) {
        uint16_t i = j;
        _Float16 x;
        std::memcpy(&x, &i, sizeof(i));
        double a = static_cast<double>(x);
        double b = half_to_double(x);
        uint64_t aRep, bRep;
        std::memcpy(&aRep, &a, sizeof(a));
        std::memcpy(&bRep, &b, sizeof(b));
        if (aRep != bRep) {
            if (std::isnan(a) && std::isnan(b)) {
                continue;
            }
            printf("ERROR: %x\n", i);
            printf("aRep = %lx\nbRep = %lx\n", aRep, bRep);
            printf("a = %f\nb = %f\n", a, b);
            printf("isnan(a) = %d\nisnan(b) = %d\n", std::isnan(a), std::isnan(b));
            return 1;
        }
        std::fwrite(&i, sizeof(i), 1, out);
        std::fwrite(&i, sizeof(i), 1, out);
        std::fwrite(&i, sizeof(i), 1, out);
        std::fwrite(&i, sizeof(i), 1, out);
        std::fwrite(&a, sizeof(a), 1, out);
    }

    std::fclose(out);

    /* printf("half_to_double -> %f\n", half_to_double(h)); */
    /* printf("conversion     -> %f\n", static_cast<double>(h)); */
    return 0;
}
