#pragma once

#include "base.hpp"

// configuration
#define FIXED_ALLOW_INT64 FALSE
#define FIXED_ALLOW_RUNTIME_FLOAT FALSE

namespace cc
{
    template <uint8_t TIntBits, uint8_t TFractBits, typename TBaseType>
    struct Fixed;

    // count leading zeroes.
    static inline uint32_t CLZ(uint32_t value)
    {
#ifdef CLARINOID_PLATFORM_X86
        unsigned int count = __lzcnt(value);
#else
        unsigned int count = __builtin_clz(value);
#endif
        // #else
        //     // Fallback implementation
        //     unsigned int count = 0;
        //     while ((value & (1 << (31 - count))) == 0 && count < 32)
        //     {
        //         count++;
        //     }
        // #endif
        return count;
    }

    const uint32_t sqrt_integer_guess_table[33] = {
        55109,
        38968,
        27555,
        19484,
        13778,
        9742,
        6889,
        4871,
        3445,
        2436,
        1723,
        1218,
        862,
        609,
        431,
        305,
        216,
        153,
        108,
        77,
        54,
        39,
        27,
        20,
        14,
        10,
        7,
        5,
        4,
        3,
        2,
        1,
        0,
    };

    // Newton-Raphson integral square root. accepts a Q32, returns Q16. I would like to find a way to return a Q32 but I
    // don't see it yet.
    static inline uint32_t sqrt_Q32_to_Q16(uint32_t in)
    {
        int i = CLZ(in);
        uint32_t n = sqrt_integer_guess_table[i];
        n = ((in / n) + n) >> 1;
        n = ((in / n) + n) >> 1;
        n = ((in / n) + n) >> 1;
        return n;
    }

    template <int32_t i>
    struct StaticAbs
    {
        static constexpr int32_t value = i < 0 ? -i : i;
    };

    template <int32_t i>
    struct StaticValueBitsNeeded
    {
        static constexpr int32_t value_allow_zero = 1 + StaticValueBitsNeeded<(StaticAbs<i>::value >> 1)>::value_allow_zero;
        static constexpr int32_t value = value_allow_zero;
    };

    template <>
    struct StaticValueBitsNeeded<0>
    {
        static constexpr int32_t value = 1;
        static constexpr int32_t value_allow_zero = 0;
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    template <typename T, std::enable_if_t<std::is_signed<T>::value, int> = 0>
    static inline constexpr T FPAbs(T val)
    {
        return val < 0 ? -val : val;
    }

    template <typename T, std::enable_if_t<!std::is_signed<T>::value, int> = 0>
    static inline constexpr T FPAbs(T val)
    {
        return val;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    template <typename T>
    struct FPTypeInfo
    {
    };

    template <>
    struct FPTypeInfo<int8_t>
    {
        using MyType = int8_t;
        using SignedEquivalent = int8_t;
        using UnsignedEquivalent = uint8_t;
        static constexpr bool IsSigned = true;
        // using PromotedType = int16_t;
        static constexpr MyType PositiveUnitScale = 0x7f;
        static constexpr MyType NegativeUnitScale = -0x80;
        static constexpr uint8_t AvailableBits = 7;
    };

    template <>
    struct FPTypeInfo<uint8_t>
    {
        using MyType = uint8_t;
        using SignedEquivalent = int8_t;
        using UnsignedEquivalent = uint8_t;
        static constexpr bool IsSigned = false;
        // using PromotedType = uint16_t;
        static constexpr MyType PositiveUnitScale = 0x80;
        // static constexpr MyType NegativeUnitScale = 0x80; // for unsigned, no negative exists; make sure overloads take this into consideration.
        static constexpr uint8_t AvailableBits = 8;
    };

    template <>
    struct FPTypeInfo<int16_t>
    {
        using MyType = int16_t;
        using SignedEquivalent = int16_t;
        using UnsignedEquivalent = uint16_t;
        static constexpr bool IsSigned = true;
        // using PromotedType = int32_t;
        static constexpr MyType PositiveUnitScale = 0x7fff;
        static constexpr MyType NegativeUnitScale = -0x8000;
        static constexpr uint8_t AvailableBits = 15;
    };

    template <>
    struct FPTypeInfo<uint16_t>
    {
        using MyType = uint16_t;
        using SignedEquivalent = int16_t;
        using UnsignedEquivalent = uint16_t;
        static constexpr bool IsSigned = false;
        // using PromotedType = uint32_t;
        static constexpr MyType PositiveUnitScale = 0xffff;
        // static constexpr MyType NegativeUnitScale = 0;
        static constexpr uint8_t AvailableBits = 16;
    };

    template <>
    struct FPTypeInfo<int32_t>
    {
        using MyType = int32_t;
        using SignedEquivalent = int32_t;
        using UnsignedEquivalent = uint32_t;
        static constexpr bool IsSigned = true;
#if FIXED_ALLOW_INT64 == TRUE
        // using PromotedType = int64_t;
#else
        // using PromotedType = int32_t;
#endif // FIXED_ALLOW_INT64
        static constexpr MyType PositiveUnitScale = 0x7fffffff;
        static constexpr MyType NegativeUnitScale = -0x80000000;
        static constexpr uint8_t AvailableBits = 31;
    };

    template <>
    struct FPTypeInfo<uint32_t>
    {
        using MyType = uint32_t;
        using SignedEquivalent = int32_t;
        using UnsignedEquivalent = uint32_t;
        static constexpr bool IsSigned = false;
#if FIXED_ALLOW_INT64 == TRUE
        // using PromotedType = uint64_t;
#else
        // using PromotedType = uint32_t;
#endif // FIXED_ALLOW_INT64
        static constexpr MyType PositiveUnitScale = 0xffffffff;
        // static constexpr MyType NegativeUnitScale = 0;
        static constexpr uint8_t AvailableBits = 32;
    };

#if FIXED_ALLOW_INT64 == TRUE
    template <>
    struct FPTypeInfo<int64_t>
    {
        using MyType = int64_t;
        using SignedEquivalent = int64_t;
        using UnsignedEquivalent = uint64_t;
        static constexpr bool IsSigned = true;
        // using PromotedType = int64_t;
        static constexpr MyType PositiveUnitScale = 0x7fffffffffffffff;
        static constexpr MyType NegativeUnitScale = -0x8000000000000000;
        static constexpr uint8_t AvailableBits = 63;
    };

    template <>
    struct FPTypeInfo<uint64_t>
    {
        using MyType = uint64_t;
        using SignedEquivalent = int64_t;
        using UnsignedEquivalent = uint64_t;
        static constexpr bool IsSigned = false;
        // using PromotedType = uint64_t;
        static constexpr MyType PositiveUnitScale = 0xffffffffffffffff;
        // static constexpr MyType NegativeUnitScale = -0x8000000000000000;
        static constexpr uint8_t AvailableBits = 64;
    };
#endif // FIXED_ALLOW_INT64 == TRUE

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    // selects datatype that can hold the given # of bits
#if FIXED_ALLOW_INT64 == TRUE
    template <uint8_t valueBits>
    using FPAutoBaseTypeSigned =   //
        typename std::conditional< //
            (valueBits <= 7),
            int8_t,
            typename std::conditional< //
                (valueBits <= 15),     //
                int16_t,
                typename std::conditional<
                    (valueBits <= 31),
                    int32_t,
                    int64_t //
                    >::type //
                >::type     //
            >::type;

    template <uint8_t valueBits>
    using FPAutoBaseTypeUnsigned = //
        typename std::conditional< //
            (valueBits <= 8),
            uint8_t,
            typename std::conditional< //
                (valueBits <= 16),     //
                uint16_t,
                typename std::conditional<
                    (valueBits <= 32),
                    uint32_t,
                    uint64_t //
                    >::type  //
                >::type      //
            >::type;

#else
    template <uint8_t valueBits>
    using FPAutoBaseTypeSigned =   //
        typename std::conditional< //
            (valueBits <= 7),
            int8_t,
            typename std::conditional< //
                (valueBits <= 15),     //
                int16_t,
                int32_t>::type //
            >::type;

    template <uint8_t valueBits>
    using FPAutoBaseTypeUnsigned = //
        typename std::conditional< //
            (valueBits <= 8),
            uint8_t,
            typename std::conditional< //
                (valueBits <= 16),     //
                uint16_t,
                uint32_t>::type //
            >::type;
#endif // FIXED_ALLOW_INT64 == TRUE

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    // selects datatype that can hold the given # of bits
    template <uint8_t valueBits, bool isSigned>
    using FPAutoBaseType = typename std::conditional<isSigned, FPAutoBaseTypeSigned<valueBits>, FPAutoBaseTypeUnsigned<valueBits>>::type;

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    template <uint8_t bits, typename T = FPAutoBaseType<bits, false>>
    static constexpr T FillBits()
    {
        // ensure remaining code has bits > 0
        if (bits == 0)
            return 0;
        T ret = 1ULL << std::max(0, (bits - 1)); // avoid compile warning about negative shifts
        ret -= 1;
        ret <<= 1;
        ret |= 1;
        return ret;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    template <int B, class T>
    static constexpr auto const_shift(const T &a, ::std::enable_if_t<(B > 0)> * = 0)
    {
        return a << ::std::integral_constant<decltype(B), B>{};
    }
    template <int B, class T>
    static constexpr auto const_shift(const T &a, ::std::enable_if_t<(B < 0)> * = 0)
    {
        return a >> ::std::integral_constant<decltype(B), -B>{};
    }
    template <int B, class T>
    static constexpr auto const_shift(const T &a, ::std::enable_if_t<(B == 0)> * = 0)
    {
        return a;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    template <typename A, typename B, bool wantSignedResult>
    struct select_larger_type_with_signedness
    {
        using LargerType = std::conditional_t<(sizeof(A) >= sizeof(B)), A, B>;
        using TypeInfo = FPTypeInfo<LargerType>;
        using type = std::conditional_t<wantSignedResult, typename TypeInfo::SignedEquivalent, typename TypeInfo::UnsignedEquivalent>;
    };

    template <typename A, typename B, bool wantSignedResult>
    using select_larger_type_with_signedness_t = typename select_larger_type_with_signedness<A, B, wantSignedResult>::type;

    template <typename A, typename B>
    struct select_larger_type_preserving_signedness
    {
        using type = select_larger_type_with_signedness_t<A, B, FPTypeInfo<A>::IsSigned || FPTypeInfo<B>::IsSigned>;
    };

    template <typename A, typename B>
    using select_larger_type_preserving_signedness_t = typename select_larger_type_preserving_signedness<A, B>::type;

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    // helps make decisions about where to truncate.
    template <uint8_t TidealIntBits, uint8_t TidealFractBits, uint8_t TavailableBits>
    struct PrecisionTruncationHelper
    {
    private:
        static constexpr uint8_t IdealWidth = TidealIntBits + TidealFractBits;
        static constexpr uint8_t TotalRightShiftNeeded = std::max(0, IdealWidth - TavailableBits);

        static constexpr uint8_t FractBitsToRemove = std::min(TidealFractBits, TotalRightShiftNeeded);
        static constexpr uint8_t RightShiftRemaining = TotalRightShiftNeeded - FractBitsToRemove;
        static constexpr uint8_t IntBitsToRemove = TidealIntBits - RightShiftRemaining;

    public:
        static constexpr uint8_t ResultFractBits = TidealFractBits - FractBitsToRemove;
        static constexpr uint8_t ResultIntBits = TidealIntBits - IntBitsToRemove;
    };

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    // allows selectively promoting a type to the ideal type.
    template <uint8_t TDesiredBits, typename TExistingBaseType, bool TallowPromotion>
    struct BaseTypePromotionHelper
    {
    private:
        using ExistingTypeInfo = FPTypeInfo<TExistingBaseType>;
        using IdealBaseType = FPAutoBaseType<TDesiredBits, ExistingTypeInfo::IsSigned>;
        using LargerBaseType = select_larger_type_preserving_signedness_t<TExistingBaseType, IdealBaseType>;

    public:
        using BaseType = std::conditional_t<TallowPromotion, LargerBaseType, TExistingBaseType>;
    };

} // namespace clarinoid
