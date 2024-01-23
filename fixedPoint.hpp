#pragma once

#include "base.hpp"
#include "fixedUtil.hpp"

// TODO: avoid shifting negative values everywhere.
// TODO: fractions in UDL
// TODO: write mutating opeprators
// TODO: write fns and operators accepting runtime int & float params.
// TODO: write tests.

/*

Usage:

auto x = fixed<16>(); // make fixed from integer
auto x = fixed<3.2>(); // make fixed from floating point literal
auto x = fixed<9, 3>(); // make fixed from fraction.
auto x = Fract<2,2>(); // specify int & fract bits. remaining bits considered unused.
auto x = Fract<2,2>::FromNumber(...); // specify int & fract bits. remaining bits considered unused.
auto x = "24"_fp; // UDL converts integers
auto x = "-12.45"_fp; // UDL converts numbers
auto x = "-12.45"_fp8; // UDL can specify base type. _fp16, _fp32

See another very similar impl: https://github.com/eteran/cpp-utilities/blob/master/fixed/include/cpp-utilities/fixed.h
it's actually so extremely similar, it's a nice affirmation of my design.
- requiring specifying both int and fract bits.
- type info stuff
there are still some odd differences though. take multiplication:
template <size_t I1, size_t I2, size_t F>
CONSTEXPR14 typename std::conditional<I1 >= I2, fixed<I1, F>, fixed<I2, F>>::type operator*(fixed<I1, F> lhs, fixed<I2, F> rhs) {
    - both operands have the same fractional bits.
    - in fact, all Fixed<> values require full saturation of the type.

also there are some implementations that work without promotion for divide & multiplication which could be handy.

here it just does like C++, returning the larger of the types.

some notes in 2024:
- i have not done any benchmarking but for true optimal performance this NEEDS to be done for M68000.
  understanding the specific instructions etc needs to be done before any micro optimization.
  32-bit ops vs. 16-bit ops should be hand-selected.
- Automatic precision: not sure it's the right idea. When it's auto, the caller still needs to be
  aware of all precision at all stages and care for this. The whole point is to make things more automatic,
  so the caller doesn't need to babysit so much. but actually when doing this automatically it
  just means it's more error-prone.

  so it may be better to only promote bits, but never DATATYPE. if the caller is comfortable
  working in the larger datatype, then they can specify this.
- considering int16 is probably optimal, i think supporting signed/unsigned, and unit_type is required again.
  gotta use every bit out of those few bits.

- for m68000 see https://plutiedev.com/multiplication-division
    takeaways:
    - avoid division at all costs (~170 cycles best case)
    - multiplication is also very costly; favor shifting.
    - actually division of 32-bit values is probably going to be obnoxiously slow.

- flexible intbits/fractbits is cool looking, however it ignores a reason for doing FP in the first place:
    on M68000, most arith instructions are designed for 16-bit values. Using a static 16.16 format enables
    a lot of optimizations.


should really think about bit promotion. balancing int vs. fract bits promotion in multiply for example.
multiplying int16:4,4 by a int16:2,5, the result should be int16<5,10> clearly.
multiplying int16:7,4 by a int16:2,5, the result should be int16<9,6>
because we assume callers are not wasting int bits, but fract bits are more "floating".


MORE notes about types and flexible FP formats:
i guess most of the time it will be nice for intermediate arithmetic values to fluently deal with their precisions.
but for example reusing variables is tricky. consider:

auto x = MakeFixed<10>();
x *= MaxeFixed<20>(); // uh oh, this kind of thing is not possible. a new type is needed.

no big deal sometimes,
auto x1 = MakeFixed<10>();
auto x2 = x1 * MaxeFixed<20>(); // uh oh, this kind of thing is not possible. a new type is needed.

BUT, loops or similar constructs are a problem. Here's a simple one:

auto x = MakeFixed<10>();
if (condition) {
    x = MakeFixed<40>(); // uh oh!
}

OR,

auto x = MakeFixed<10>();
while(true) {
    x = x * 10;
}

the loop is interesting because you cannot just use a new type. you need to keep reusing the same datatype which will result
in a bunch of unnecessary shifting and likely invisible precision loss.

SOLUTION:
- use decltype() to find a fitting datatype before the loop.
- use Mutating*() methods to reuse variables


*/

// A fixed-point library.

// Don't want to lock ourselves into specific fixed point types like Q15, Q31, Q32.
// This library aims to be fluid about the FP format, in order to allow the compiler to fully
// optimize chained operations so intermediate datatypes are optimal, reducing the amount
// of shifting and precision loss.

// Using a fixed-point class is tricky, because for my purpose the point is to tightly control
// how operations are performed, which datatypes to select, and which precision levels are
// required.
//
// This library aims to make calling code more expressive and less filled with obscene shifting
// and masking, but the caller still has responsibility for managing precision.

// in order to optimize precision, callers must specify integral bits + fractional bits.
// this allows left-shifting when more precision is demanded by temp values, retaining as much
// precision as possible.

// in general CPUs will have a very performant base type (usually register sized), and then
// a VERY badly performing larger types which may use microcode etc and be like 100x slower.
// so base type can often auto-selected here, but largely it's the caller's responsibility
// to manage base type.
//
// GUIDELINES for callers:
// - use auto and pray your IDE can reveal the type
// - keep intbits as small as possible when you know the max value. every bit wasted here is at least bit of precision lost during ops.
//
// Performance testing on teensy shows a couple fun facts:
// - Signed vs. unsigned are exactly the same performance on all ops
// - Inline assembly does not help. Even with the ummul instruction. It forces the compiler
//   to rearrange how it does things, set up registers in a certain way, and loses performance.
// - 64-bit ops are about >5x slower than 32 in general
// - 32-bit all ops are blazing fast; effectively free. Except division (& mod) is twice as slow.
// - 8 and 16 bit are about ~3x slower than 32.
// - conversion to and from 32-bit is not free; it's about on par with other arith ops.

namespace cc
{

    // construct from integral literal template parameter
    // compile time optimization is effective. use this when possible.
    template <int32_t Literal,
              typename TBaseType = FPAutoBaseType<StaticValueBitsNeeded<Literal>::value, (Literal < 0)>> //
    constexpr auto fixed()
    {
        static_assert(!(Literal < 0 && std::is_unsigned<TBaseType>::value), "You are attempting to put a negative value in an unsigned base type.");
        constexpr int32_t TIntBits = StaticValueBitsNeeded<Literal>::value;
        return Fixed<TIntBits, 0, TBaseType>::FromUnderlyingValue(Literal); // 0 fract bits means no shift required.
    }

    // from runtime integral literal
    template <uint8_t TintBits,   // caller to specify
              uint8_t TfractBits, // caller to specify
              typename TBaseType, // caller to specify
              typename TinputType,
              std::enable_if_t<std::is_integral<TinputType>::value, int> = 0>
    constexpr auto fixed(TinputType n)
    {
        return Fixed<TintBits, TfractBits, TBaseType>::FromNumber(n);
    }

    template <int32_t Num, int32_t Den, typename BaseType>
    struct FixedFromFractionHelper
    {
        static_assert(Den != 0, "Denominator cannot be zero.");
        using TypeInfo = FPTypeInfo<BaseType>;
        static constexpr int32_t ResultIntPart = Num / Den;
        static constexpr uint8_t ResultIntBits = StaticValueBitsNeeded<ResultIntPart>::value;
        static constexpr uint8_t ResultFractBits = TypeInfo::AvailableBits - ResultIntBits;
        static constexpr bool ResultIsNegative = (Num < 0) != (Den < 0);
        using ResultBaseType = FPAutoBaseType<ResultIntBits + ResultFractBits, ResultIsNegative>;
        // shifting negative numbers is just wrong.
        static constexpr int32_t AbsNum = Num < 0 ? -Num : Num;
        static constexpr int32_t AbsDen = Den < 0 ? -Den : Den;
        // compile-time 64-bit can be achieved
        static constexpr ResultBaseType underlyingResultAbs = (ResultBaseType)(((long long)AbsNum << ResultFractBits) / AbsDen); //
        using ResultType = Fixed<ResultIntBits, ResultFractBits, ResultBaseType>;
        static constexpr ResultType result = ResultType::FromUnderlyingValue(ResultIsNegative ? -underlyingResultAbs : underlyingResultAbs);
    };

    // only the caller can actually decide on base type. there's no point trying to calculate that automatically.
    // for example what would be auto-selected for a repeating decimal ?
    template <int32_t Num,        // caller specify
              int32_t Den,        // caller specify
              typename TBaseType, // caller specify
              typename THelper = FixedFromFractionHelper<Num, Den, TBaseType>>
    static inline constexpr auto fixed()
    {
        THelper h;
        return THelper::result;
    }

    ///////////////////////////////////////////////////////////////////////////////////////////////////
    // specify both fract bits and int bits, so we know how much overhead there is and can choose small types as needed.
    template <uint8_t TIntBits, uint8_t TFractBits, typename TBaseType>
    struct Fixed
    {
        using MyT = Fixed<TIntBits, TFractBits, TBaseType>;
        static constexpr uint8_t kIntBits = TIntBits;
        static constexpr uint8_t kFractBits = TFractBits;
        using BaseType = TBaseType;
        static constexpr uint8_t kTotalBits = TIntBits + TFractBits;
        using TypeInfo = FPTypeInfo<BaseType>;
        static constexpr bool kIsSigned = TypeInfo::IsSigned;

        static_assert(kTotalBits <= TypeInfo::AvailableBits, "FP format too wide");
        static_assert(kTotalBits > 0, "FP format requires at least 1 bit to hold data otherwise undefined.");

        BaseType mValue;

        static constexpr BaseType gPositiveOne = BaseType(1ULL << std::min<uint8_t>(TypeInfo::AvailableBits - 1, kFractBits));

        static constexpr typename TypeInfo::UnsignedEquivalent mFractMask = FillBits<kFractBits>();
        static constexpr BaseType mRemoveSignMask = FillBits<TypeInfo::AvailableBits>();

        using CorrespondingSignedBaseType = typename TypeInfo::SignedEquivalent;
        using CorrespondingSignedTypeInfo = FPTypeInfo<CorrespondingSignedBaseType>;
        using CorrespondingSignedHelper = PrecisionTruncationHelper<kIntBits, kFractBits, CorrespondingSignedTypeInfo::AvailableBits>;
        using CorrespondingSignedType = Fixed<CorrespondingSignedHelper::ResultIntBits, CorrespondingSignedHelper::ResultFractBits, CorrespondingSignedBaseType>;

        static constexpr int8_t kUnusedBits = TypeInfo::AvailableBits - kTotalBits;

    private:
        explicit constexpr Fixed(BaseType v) : mValue(v)
        {
        }

    public:
        // convert a fixed type to another, which is really just a shift. but which direction, and before or after casting
        // is important.
        //
        // EXPLICIT is important here because truncation occurs easily.
        // consider for example Sqrt(unit_value); if explicit, then callers wouldn't realize why the result is incorrect.
        // meanwhile this ctor is invoked and truncates signbit & int part.
        //
        template <uint8_t TOtherIntBits,
                  uint8_t TOtherFractBits,
                  typename TOtherBaseType>
        explicit constexpr Fixed(const Fixed<TOtherIntBits, TOtherFractBits, TOtherBaseType> &v)
            : mValue(static_cast<BaseType>(const_shift<kFractBits - TOtherFractBits>(v.mValue)))
        {
            // TODO: static_asserts here to detect overflows.
            // TODO: runtime asserts for more overflows, signed truncation etc.
        }

        template <uint8_t TOtherIntBits,
                  uint8_t TOtherFractBits,
                  typename TOtherBaseType>
        constexpr void MutatingAssign(const Fixed<TOtherIntBits, TOtherFractBits, TOtherBaseType> &v)
        {
            // TODO: static_asserts here to detect overflows.
            // TODO: runtime asserts for more overflows, signed truncation etc.
            mValue = (static_cast<BaseType>(const_shift<kFractBits - TOtherFractBits>(v.mValue)));
        }

        static constexpr MyT FromUnderlyingValue(BaseType v)
        {
            return MyT{v};
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        static constexpr MyT FromNumber(T v)
        {
            return MyT{static_cast<BaseType>(v << kFractBits)};
        }

#if FIXED_ALLOW_RUNTIME_FLOAT == TRUE
        // runtime construction from floating point types
        template <typename T, std::enable_if_t<std::is_floating_point<T>::value, int> = 0>
        constexpr Fixed(const T &v) : mValue(static_cast<BaseType>((v < 0) ? (v * mNegativeScale) : (v * mPositiveScale)))
        {
        }

        // create from floating point values
        template <typename T, std::enable_if_t<std::is_floating_point<T>::value, int> = 0>
        static constexpr MyT FromNumber(T val)
        {
            return MyT{val};
        }

        constexpr double ToDouble() const
        {
            if (mValue < 0)
                return (double)mValue / mNegativeScale;
            return (double)mValue / mPositiveScale;
        }

        constexpr float ToFloat() const
        {
            if (mValue < 0)
                return (float)mValue / mNegativeScale;
            return (float)mValue / mPositiveScale;
        }
#endif // #if FIXED_ALLOW_RUNTIME_FLOAT == TRUE

        template <size_t N>
        struct Str
        {
            char str[N];
            char *end() { return str + N; }
            operator const char *() const { return str; }
        };

        Str<40> ToString(uint8_t maxDecimals = 5) const
        {
            Str<40> ret;
            // regarding negative numbers-- there's no negative zero, so we need to handle sign bit specially.
            bool isNegative = IsNegative();
            auto intPartAbs = FPAbs(IntPart());
            USHORT intPartSize = intToStrT(ret.str, intPartAbs, NumberBase::Decimal, isNegative);
            auto fract = Fract();
            if (maxDecimals > 0 && fract.IsNonZero())
            {
                char *fractBuf = ret.str + intPartSize;
                *fractBuf++ = '.';
                char *endMinusOne = std::min(ret.end() - 1, fractBuf + maxDecimals);
                auto base = Fixed<4, 0, FPAutoBaseType<4, false>>::FromUnderlyingValue(10);
                using accType = decltype(fract.MultiplyAllowingPromotion(base));
                accType fractPart{fract};
                constexpr auto scale = 1 << accType::kFractBits;

                while (fractPart.IsNonZero() && fractBuf < endMinusOne)
                {
                    fractPart.MutatingMultiply(base);
                    auto digit = fractPart.UnderlyingValue() / scale; // scalar type
                    *fractBuf++ = '0' + digit;
                    fractPart.MutatingFract();
                }
                *fractBuf++ = 0;
            }
            return ret;
        }

        template <uint8_t intbits, typename ResultType = Fixed<intbits, kFractBits, BaseType>>
        constexpr CL_NODISCARD ResultType SetIntBits() const
        {
            return ResultType{*this};
        }

        template <typename ResultType = Fixed<kIntBits + kUnusedBits, kFractBits, BaseType>>
        constexpr CL_NODISCARD ResultType MaximizeIntBits() const
        {
            return ResultType{*this};
        }

        template <uint8_t fractbits, typename ResultType = Fixed<kIntBits, fractbits, BaseType>>
        constexpr CL_NODISCARD ResultType SetFractBits() const
        {
            return ResultType{*this};
        }

        template <typename TNewBaseType, typename ResultType = Fixed<kIntBits, kFractBits, TNewBaseType>>
        constexpr CL_NODISCARD ResultType SetBaseType() const
        {
            return ResultType{*this};
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB>
        constexpr CL_NODISCARD Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> Convert() const
        {
            return Fixed<TIntBitsB, TFractBitsB, TBaseTypeB>{*this};
        }

    private:
        // describes a static execution plan for combining 2 operands of different types,
        // where the ideal result is the sum of bits (as in multiplication & division)
        template <uint8_t TIntBitsB, uint8_t TFractBitsB, typename TBaseTypeB, bool TallowBaseTypePromotion>
        struct OperandTypeHelper
        {
            using BFixedType = Fixed<TIntBitsB, TFractBitsB, TBaseTypeB>;

            // the most efficient mul does no shifting:
            // r = a * b;
            // where r now has intbits & fractbits of A & B added.
            //
            // that only works when the ideal type fits within the desired result base type
            // if it doesn't, then we may get away with shifting only 1 of them.
            //     r = (a >> x) * b;
            // or, r = a * (b >> x);
            //
            // that's possible when one of the input widths is naturally less than half of the result width.
            // then it can be kept and the other uses the biggest width.
            //
            // if both input widths are > available intermediate width, then both must be shifted first.
            // r = (a >> x) * (b >> (w-x));
            //

            // regarding auto type promotion:
            // * we have to assume the caller is maintaining intbits well, which means we should prioritize
            //   preserving intbits over fractbits. if we have to lose precision, better to do it on the fractional part rather than the
            //   most significant part.
            // * we should also not bother using a promoted intermediate type, again let caller use the type they're comfortable with.

            // unfortunately this will miss opportunities to use optimized cpu instructions for example 68k MULU.w and MULS.w where the CPU
            // does the promotion automatically.

            // example cases and how it will be executed

            //                        idealWidth                      RShiftNeeded
            // A         * B          (=width)    actualResultWidth   (loss of prec)   Ashift   Bshift
            // int8<2.2>   int8<2.1>  4.3 (=7)    int8<4.3>           0                                 ideal case; no shift needed.
            // int8<4.4>   int8<2.3>  6.7 (=13)   int8<6.1>           >> 6             >> 3     >> 3    loss of 6 bits of precision.
            //                                                                                          we should distribute the loss between A & B,
            //                                                                                          converting A to <4.3>, B to <2.0>.
            // int32<30.1> int8<6.1>  36.2 =38w   int32<31.0>         >> 7  in a very imbalanced situation like this (operands have very different precisions),
            //                                                              how to distribute the shifting? let's first take away precision from the bigger type
            //                                                              until it's in balance with the other, then distribute the remaining equally.

            // Let's calculate all this by starting with ideal intermediate width
            static constexpr uint8_t IdealIntbits = (kIntBits + TIntBitsB);
            static constexpr uint8_t IdealFractbits = (kFractBits + TFractBitsB);
            static constexpr uint8_t IdealWidth = IdealIntbits + IdealFractbits;

            // determine the return type.
            // just select the larger of the operands; the caller is not expecting to promote to a new type.
            // Keep sign bit.
            // A       B        Result
            // int8    uint8    int8
            // uint32  int8     int32
            // static constexpr bool ResultHasSignBit = TisSignedB || kIsSigned;
            using ResultBaseTypeWithoutPromotion = select_larger_type_preserving_signedness_t<BaseType, TBaseTypeB>;
            using ResultBaseType = typename BaseTypePromotionHelper<IdealWidth, ResultBaseTypeWithoutPromotion, TallowBaseTypePromotion>::BaseType;

            using ResultTypeInfo = FPTypeInfo<ResultBaseType>;

            static constexpr uint8_t ResultWidth = ResultTypeInfo::AvailableBits;
            static constexpr uint8_t TotalRightShiftNeeded = std::max(0, IdealWidth - ResultWidth);

            static constexpr uint8_t AWidth = kTotalBits;
            static constexpr uint8_t BWidth = BFixedType::kTotalBits;

            // TODO: Q: does this actually work? i feel like we need to consider int bits vs. fract bits; we can't shift off int bits.
            // first shift wider operand to be down to equalize widths
            static constexpr uint8_t ARightShiftToBringToBWidth = std::max(0, AWidth - BWidth);
            static constexpr uint8_t ARightShift1 = std::min(ARightShiftToBringToBWidth, TotalRightShiftNeeded);
            static constexpr uint8_t BRightShiftToBringToAWidth = std::max(0, (BWidth - AWidth));
            static constexpr uint8_t BRightShift1 = std::min(BRightShiftToBringToAWidth, TotalRightShiftNeeded);

            static constexpr uint8_t RightShiftRemainingAfterEqualizationShift = TotalRightShiftNeeded - (ARightShift1 + BRightShift1);

            // now distribute the remaining shift evenly.
            static constexpr uint8_t RightShiftForNarrowerOperand = RightShiftRemainingAfterEqualizationShift / 2;
            static constexpr uint8_t RightShiftForWiderOperand =
                RightShiftRemainingAfterEqualizationShift - RightShiftForNarrowerOperand;

            static constexpr uint8_t AWidthAfterShift1 = AWidth - ARightShift1;
            static constexpr uint8_t BWidthAfterShift1 = BWidth - BRightShift1;

            static constexpr uint8_t ARightShift2 =
                AWidthAfterShift1 > BWidthAfterShift1 ? RightShiftForWiderOperand : RightShiftForNarrowerOperand;
            static constexpr uint8_t BRightShift2 =
                AWidthAfterShift1 > BWidthAfterShift1 ? RightShiftForNarrowerOperand : RightShiftForWiderOperand;

            static constexpr uint8_t ARightShiftBeforeMul = ARightShift1 + ARightShift2;
            static constexpr uint8_t BRightShiftBeforeMul = BRightShift1 + BRightShift2;

            // and now calculate the FP format of the intermediate (result) type. it's simple: right-shifting steals
            // from fractbits, then intbits, for both operands. Result type is them added together.
            static constexpr uint8_t AFractBitsShiftedOut = std::min(kFractBits, ARightShiftBeforeMul);
            static constexpr uint8_t AShiftedFractBits = kFractBits - AFractBitsShiftedOut;
            static constexpr uint8_t ARightShiftRemaining = ARightShiftBeforeMul - AFractBitsShiftedOut;
            static constexpr uint8_t AShiftedIntBits = kIntBits - ARightShiftRemaining;

            static constexpr uint8_t BFractBitsShiftedOut = std::min(TFractBitsB, BRightShiftBeforeMul);
            static constexpr uint8_t BShiftedFractBits = TFractBitsB - BFractBitsShiftedOut;
            static constexpr uint8_t BRightShiftRemaining = BRightShiftBeforeMul - BFractBitsShiftedOut;
            static constexpr uint8_t BShiftedIntBits = TIntBitsB - BRightShiftRemaining;

            static constexpr uint8_t ResultFractBits = AShiftedFractBits + BShiftedFractBits;
            static constexpr uint8_t ResultIntBits = AShiftedIntBits + BShiftedIntBits;

            using ResultType = Fixed<ResultIntBits, ResultFractBits, ResultBaseType>;
        };

    public:
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = OperandTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB, false>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD ResultType Multiply(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            // important to retain original datatypes here. ResultBaseType can be 1 bit smaller than an operand if the operand is unsigned and the result is signed.
            BaseType ap = mValue;
            typename TypeHelper::ResultBaseType bp = b.mValue;

            ap >>= TypeHelper::ARightShiftBeforeMul;
            bp >>= TypeHelper::BRightShiftBeforeMul;

            auto intermediate = ap * bp;

            return ResultType::FromUnderlyingValue(intermediate);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD auto Multiply(T b) const
        {
            // for runtime operation, there's no way to know the precision. but at least it's integral so we know intbits is the one to increase.
            auto ret = MaximizeIntBits();
            ret.mValue *= b;
            return ret;
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = OperandTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB, true>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD ResultType MultiplyAllowingPromotion(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            // important to retain original datatypes here. ResultBaseType can be 1 bit smaller than an operand if the operand is unsigned and the result is signed.
            BaseType ap = mValue;
            typename TypeHelper::ResultBaseType bp = b.mValue;

            ap >>= TypeHelper::ARightShiftBeforeMul;
            bp >>= TypeHelper::BRightShiftBeforeMul;

            auto intermediate = ap * bp;

            return ResultType::FromUnderlyingValue(intermediate);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB>
        constexpr void MutatingMultiply(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b)
        {
            auto result = Multiply(b);
            MutatingAssign(result);
        }

        // division is very vexing. ranges cannot be predicted with 100% accuracy, things get wild quickly, and there are pitfalls.
        // doing any deduction on return type is too "magicky" and the caller will spend more effort trying to figure out why a type was
        // deduced wrong than on understanding the types that should be used.
        //
        // so there's no choice; this just sorta works & feels different than other functions in this class.
        // we will do a few basic things to try to get things working though... here are some ideas...
        // 1. (easy) select the bigger type, with sign preserve
        // 2. maximize precision within the datatype. it's not a guaranteed success, and depending on the inputs different strats should be chosen.
        //    as intermediate value, our own value gets shifted left by B's fractbits. so
        //    - make sure there's enough space for the intermediate value.
        //      * if there's no room for all those bits + intbits, then reduce precision before shifting.
        //    - saturate any remaining bits with fract bits.
        //
        // TIPS FOR CALLERS:
        // - squeeze this::kIntBits, to make as much room for precision as possible. the rest is handled more or less automatically.
        //
        // most of the time there won't be enough bits for the intermediate type. and depending on the input values, different strategies
        // could be used in reducing precision of the operands. for example if you're dividing a big number by a small number, better to reduce the
        // precision of operand A first if possible. Other scenarios could be reducing operand B first. Or balancing.
        // for 1st exercise i will balance.
        template <uint8_t TIntBitsB, uint8_t TFractBitsB, typename TBaseTypeB>
        struct DivideHelper
        {
            // the basic op is ((underlyingVal << FractBitsB) / divisorUnderlyingValue)
            //                        NatInterm.
            // A         / B          width       actualResultWidth
            // int8<2.2>   int8<2.1>  5           int8<???.???>        So the point here is that the resulting bits cannot be estimated.
            // the best we could do is make sure we can at least DO the operation at all. that means bringing A and/or B to formats which
            // can be represented within the datatype without overflowing. in the above case, the natural intermediate value fits fine in the int8 datatype.
            // the resulting format will be the same as `this`, to avoid having to do any additional shifts.
            //
            // int8<4.2>   int8<1.3>  9
            // so in this case, the datatype cannot hold the 9 required bits.
            // therefore like multiplication, first bring the operands to equal, then distribute evenly.
            // 1. int8<4.2> and int8<1.2>   reduced 1 bit of precision in B. 1 remaining.
            // 2. int8<4.1> and int8<1.2>   distributed the remaining among A & B.
            //
            // uint8<1.7>   uint8<1.7> 15
            // a more extreme example; here 15 bits are required for the intermediate, 6 too many.
            // 1.4          1.4
            // this will not always be possible. the most extreme example:
            // uint8<8.0>   uint8<0.8> 16
            // 8 too many, which brings operand B to 0 bits total. oops. it's impossible to perform this division.
            //
            // i lied, the MOST extreme example is mixing signed and unsigned. it may also be impossible for the same reason.
            // int8<0.7>   uint8<8.0> 15
            // 7 too many; impossible because 0.0 is not a valid fp format.
            //
            // then phase 2 is to fill out any remaining bits.
            // uint8<1.3>   uint8<2.2> 6
            // the intermediate will be 3.3 format, and there's space for 2 more bits of precision.
            // put that on operand A fractbits -->
            // uint8<1.5>   uint8<2.2> 8

            using BFixedType = Fixed<TIntBitsB, TFractBitsB, TBaseTypeB>;
            using IntermediateBaseType = select_larger_type_preserving_signedness_t<BaseType, TBaseTypeB>;
            using IntermediateTypeInfo = FPTypeInfo<IntermediateBaseType>;
            static constexpr uint8_t AFractBits = kFractBits;
            static constexpr uint8_t BFractBits = TFractBitsB;
            static constexpr uint8_t kNaturalWidth = kTotalBits + BFractBits;
            static constexpr uint8_t kBitsTooMany = std::max(0, kNaturalWidth - IntermediateTypeInfo::AvailableBits);

            // PHASE 1: if too many intermediate bits, reduce.
            // first by equalizing the fract parts of A & B, then distribute remaining equally.
            static constexpr uint8_t ARightShiftToBalance = std::max(0, AFractBits - BFractBits);
            static constexpr uint8_t ARightShift1 = std::min(ARightShiftToBalance, kBitsTooMany);
            static constexpr uint8_t BRightShiftToBalance = std::max(0, (BFractBits - AFractBits));
            static constexpr uint8_t BRightShift1 = std::min(BRightShiftToBalance, kBitsTooMany);

            static constexpr uint8_t ShiftRemainingAfterBalancing = kBitsTooMany - (ARightShift1 + BRightShift1);

            // now distribute the remaining shift evenly.
            static constexpr uint8_t BalancedShiftForOperandA = ShiftRemainingAfterBalancing / 2;
            static constexpr uint8_t BalancedShiftForOperandB =
                ShiftRemainingAfterBalancing - BalancedShiftForOperandA;

            static constexpr int8_t ATotalShift = ARightShift1 + BalancedShiftForOperandA;
            static constexpr int8_t BTotalShift = BRightShift1 + BalancedShiftForOperandB;
            static_assert(ATotalShift <= AFractBits, "there's no more precision to remove from operand A; this division is impossible to perform.");
            static_assert(BTotalShift <= BFractBits, "there's no more precision to remove from operand B; this division is impossible to perform.");

            static constexpr uint8_t ATempFractBits = AFractBits - ATotalShift;
            static constexpr uint8_t BTempFractBits = BFractBits - BTotalShift;

            // PHASE 2: if operand A has any bits available, saturate it with fract bits.
            static constexpr uint8_t IntermediateIntBits = kIntBits + BTempFractBits;
            static constexpr uint8_t IntermediateFractBits = ATempFractBits;
            static constexpr uint8_t ATempFreeBitsAvailable = IntermediateTypeInfo::AvailableBits - (IntermediateIntBits + IntermediateFractBits);

            // using ATempType = Fixed<kIntBits, ATempFractBits + ATempFreeBitsToFillWithFract, IntermediateBaseType>;
            using ATempType = Fixed<IntermediateIntBits, ATempFractBits + ATempFreeBitsAvailable, IntermediateBaseType>;
            using BTempType = Fixed<TIntBitsB, BTempFractBits, IntermediateBaseType>;
            using IntermediateType = ATempType; // the whole idea is to use ATemp as a sort of accumulator; it will therefore act as intermediate & result types as well.
            using ResultType = ATempType;
        };

        template <uint8_t TIntBitsB, uint8_t TFractBitsB, typename BaseTypeB>
        constexpr auto Divide(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &divisor) const
        {
            // bool resultNegative = this->IsNegative() != divisor.IsNegative();
            using helper = DivideHelper<TIntBitsB, TFractBitsB, BaseTypeB>;
            helper h;
            typename helper::ATempType atemp{*this};
            typename helper::BTempType btemp{divisor};

            DbgPrintF("atemp=%s btemp=%s c=%s, e=%s",
                      atemp.ToString().str,
                      btemp.ToString().str);

            // auto acc = (FPAbs(atemp.mValue) << helper::BTempFractBits) / FPAbs(btemp.mValue);
            // return helper::ResultType::FromUnderlyingValue(resultNegative ? -acc : acc);
            auto acc = (atemp.mValue << helper::BTempFractBits) / btemp.mValue;
            return helper::ResultType::FromUnderlyingValue(acc);
        }

        // TODO: implement a slow divide which is more accurate

        // modulo
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  // regarding sign bit, following C++ operator% behavior,
                  // the result always has the same sign as A. the sign of B is always ignored
                  typename ResultType = Fixed<kIntBits, (TypeInfo::AvailableBits - kIntBits), TBaseTypeB>>
        constexpr CL_NODISCARD ResultType Modulo(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            // this is simpler than divide, because the resulting result range is always less than the input range.
            // for this to work, they must be the same fractbits. so we need to find a unified format that fills the
            // datatype, retains sign, unifies fractbits.
            // A must not reduce intbits.
            // A     B        intermediate
            // 8.3   0.32     8.24
            // 32.0  0.32     32.0

            // that can be summarized to say:
            // 1. fully saturate A's type, preserving intbits
            // 2. convert B to the intermediate type.

            ResultType ta{*this};
            ResultType tb{b};

            auto x = ta.mValue;
            auto N = tb.mValue;
            auto intermediate = x % N;

            // Behavior of negatives is controversial. Some other algos to consider...
            // https://stackoverflow.com/a/4003293/402169
            // auto intermediate = (x < 0) ? (x % N + N) : (x % N);
            //
            // but the simplest motivation to use built-in C++ operator % is that it enables
            // simple implementation of operator %().

            return ResultType::FromUnderlyingValue(intermediate);
        }

        // TODO: test negatives
        constexpr CL_NODISCARD MyT Floor() const
        {
            auto x = mValue & ~mFractMask;
            return MyT::FromUnderlyingValue(x);
        }

        // TODO: test negatives
        constexpr CL_NODISCARD MyT Ceil() const
        {
            BaseType t = mValue & ~mFractMask;
            t += gPositiveOne;
            return MyT{t};
        }

        template <bool U = kIsSigned, std::enable_if_t<U, int> = 0>
        constexpr CL_NODISCARD MyT Abs() const
        {
            return FromUnderlyingValue(mValue < 0 ? -mValue : mValue);
        }

        template <bool U = kIsSigned, std::enable_if_t<!U, int> = 0>
        constexpr CL_NODISCARD MyT Abs() const
        {
            return *this;
        }

        constexpr CL_NODISCARD CorrespondingSignedType Negate() const
        {
            return CorrespondingSignedType::FromUnderlyingValue(-mValue);
        }

        constexpr bool IsNegative() const
        {
            return mValue < 0;
        }

        // NO corresponding "is positive", because it's not 100% clear in all situations if zero should be considered. i think we can safely say zero is not negative.

    private:
        // when subtracting (or adding a signed value), you may need to give up 1 bit of precision to add a sign bit.
        // when negating explicitly, that's an error (see the static_assert).
        // but when adding/subtracting, sometimes the caller knows that the result will remain unsigned.
        // therefore, when there's not enough space in the type for the sign bit, just leave it out.
        // that makes 32.0 - 32.0 behave in a way callers will expect, for example.
        //
        // the operands also must be brought into a unified format.
        //
        // The return base type will be the larger bitness of A or B, plus one.
        // return type MUST always have a sign bit because there's no way to know at compile-time if the result will be negative.
        //
        // 8.8 minus 6.10 => 8.10, signed. larger of 2, unify fractbits, room for sign.
        // 31.0 minus 0.31 => wants 31.31, preserve intbits and truncate fract with sign => 31.0 int32.
        // 32.0<uint32> minus signed 8.8 would be uint32 (no promotion to 64; intbits must be preserved and truncates)
        // 8.8 minus 32.0<uint32> => wants 32.8; no promotion to 64 bit so 32.0 uint32.
        // 31.0<int32> minus 32.0 => wants 32.0; preserve intbits => 32.0 uint32
        // 31.0<int32> minus 0.32 => wants 31.32; preserve intbits and truncate fractbits => 31.0 int32.
        // 31.0<uint32> minus 0.32 <int64> => wants 31.32 into 64-bit type => 31.32 <int64>
        // 32.0<uint32> minus 0.32 <int64> => wants 32.32 into 64-bit type => truncate fractbits to 32.31 <int64>
        // 32.0<uint32> minus 32.16 <uint64> => wants 32.16 into 64-bit result type => 32.16 int64.

        template <uint8_t TIntBitsB, uint8_t TFractBitsB, typename TBaseTypeB>
        struct DifferenceTypeHelper
        {
        private:
            using FixedB = Fixed<TIntBitsB, TFractBitsB, TBaseTypeB>;
            // if EITHER type has a sign, then we need to consider the fact that subtracting can overflow by 1.
            // for example, a 0.15 - -0.15 can result in a value of +1.999.
            // other way to explain:
            // value of 7.999 (requiring only 3 int bits) minus -7.999 (also 3 intbits) = 15.9999 requiring 4 integer bits. so this is the larger of both, plus one due to possibility of overflow.
            static constexpr uint8_t IdealIntBits = std::max(TIntBitsB, kIntBits) + ((kIsSigned || FixedB::kIsSigned) ? 1 : 0);
            static constexpr uint8_t IdealFractBits = std::max(TFractBitsB, kFractBits);
            using ResultBaseType = select_larger_type_with_signedness_t<BaseType, TBaseTypeB, true>;
            using ResultTypeInfo = FPTypeInfo<ResultBaseType>;

            // prevent too many bits.
            static constexpr uint8_t ResultIntBits = std::min(ResultTypeInfo::AvailableBits, IdealIntBits);

            // now truncate as needed.
            static constexpr uint8_t ResultFractBits =
                std::min<uint8_t>(IdealFractBits, ResultTypeInfo::AvailableBits - ResultIntBits);

            // don't allow types with 0.0 fp format.
            // static constexpr uint8_t ResultFractBitsWithCorrection = ((ResultIntBits + ResultFractBits) == 0) ? 1 : ResultFractBits;

        public:
            using ResultType = Fixed<ResultIntBits, ResultFractBits, ResultBaseType>;
            using FractResultType = Fixed<0, std::max(1, (int)ResultFractBits), ResultBaseType>;
        };

    public:
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = DifferenceTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD ResultType Subtract(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            // TODO: handle the case where the resulting type is SMALLER than the operands (when adding sign bit)
            ResultType ta{*this};
            ResultType tb{b};
            return ResultType::FromUnderlyingValue(ta.mValue - tb.mValue);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB>
        constexpr void MutatingSubtract(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b)
        {
            auto result = Subtract(b);
            MutatingAssign(result);
        }

        // // always returns a non-negative value.
        // template <typename TypeHelper = DifferenceTypeHelper<TIntBits, TFractBits, BaseType>,
        //           typename ResultType = typename TypeHelper::FractResultType,
        //           typename T = BaseType>
        // constexpr CL_NODISCARD ResultType Fract() const
        // {
        //     // this will always be the same base type, just different intbits specification so optimal with elision.
        //     return ResultType{this->Subtract(Floor())};
        // }

        // always returns a non-negative value.
        // there are multiple ways of doing fract()... one is like this just extracting the positive fractional part directly from mValue.
        // another is subtracting the floor(). i am not sure which is more "correct" -- I believe they both have uses, but maybe just need different names
        constexpr CL_NODISCARD MyT Fract() const
        {
            // Extract the fractional part directly from mValue
            return MyT{static_cast<typename TypeInfo::UnsignedEquivalent>(FPAbs(mValue)) & mFractMask};
        }

        // does not compute a return type; modifies own value.
        constexpr void MutatingFract()
        {
            auto result = Fract();
            MutatingAssign(result);
        }

        bool HasNonZeroFractPart() const
        {
            return !!(mValue & mFractMask);
        }

        bool IsNonZero() const
        {
            return !!mValue;
        }

        // shifting values with a sign bit is problematic. remove the sign bit and replace it.
        template <bool U = kIsSigned, std::enable_if_t<U, int> = 0>
        constexpr CL_NODISCARD BaseType IntPart() const
        {
            if (IsNegative())
            {
                return -(-mValue >> kFractBits);
            }
            return mValue >> kFractBits;
        }

        // for unsigned types, we avoid masking the int part.
        template <bool U = kIsSigned, std::enable_if_t<!U, int> = 0>
        constexpr CL_NODISCARD BaseType IntPart() const
        {
            return mValue >> kFractBits;
        }

        constexpr CL_NODISCARD BaseType UnderlyingValue() const
        {
            return mValue;
        }

        // only difference between this and difference helper is that this doesn't force the output to be signed, and handles carry over differently because there's no "subtracting a negative" trickery.
        template <uint8_t TIntBitsB, uint8_t TFractBitsB, typename TBaseTypeB>
        struct SumHelper
        {
            using FixedB = Fixed<TIntBitsB, TFractBitsB, TBaseTypeB>;

            // as with DifferenceHelper, adding 2 numbers may carry over +1 from the fractional part, which means 1 additional bit is required.
            // see the example above.
            static constexpr uint8_t IdealIntBits = std::max(TIntBitsB, kIntBits) + 1;
            static constexpr uint8_t IdealFractBits = std::max(TFractBitsB, kFractBits);
            using ResultBaseType = select_larger_type_preserving_signedness_t<BaseType, TBaseTypeB>;
            using ResultTypeInfo = FPTypeInfo<ResultBaseType>;

            // prevent too many bits.
            static constexpr uint8_t ResultIntBits = std::min(ResultTypeInfo::AvailableBits, IdealIntBits);

            // now truncate as needed.
            static constexpr uint8_t ResultFractBits =
                std::min<uint8_t>(IdealFractBits, ResultTypeInfo::AvailableBits - ResultIntBits);

            using ResultType = Fixed<ResultIntBits, ResultFractBits, ResultBaseType>;
            using FractResultType = Fixed<0, ResultFractBits, ResultBaseType>;
        };

        // add
        // even though we could theoretically think of ways in which this is different than subtract, none
        // of them really matter. If difference is robust to signedness and unsignedness, this will be equally.
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = SumHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD ResultType Add(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            ResultType ta{*this};
            ResultType tb{b};
            return ResultType::FromUnderlyingValue(ta.mValue + tb.mValue);
        }

    private:
        // shifting can just be changing fractbits. if there are no more bits remaining,
        // then the underlying value will be shifted. sign not touched.
        template <uint8_t amt>
        struct ShiftHelper
        {
            // in order to shift left only by changing FP format bits,
            // there needs to be room to shift fractbits down by amt.
            //
            // In the case where fast method is enabled, the underlying value is not to be touched; only the FP format.
            // it really works; precision is simply moved from fract to int part, and instead of introducing new zeroes,
            // we just change the FP format.
            static constexpr bool EnableFastShiftLeft = kFractBits >= amt;
            using ShiftLeftResultType = typename std::conditional<EnableFastShiftLeft,
                                                                  Fixed<kIntBits + amt, kFractBits - amt, BaseType>,
                                                                  MyT>::type; // otherwise, the underlying value must be
                                                                              // shifted, and the format doesn't change.

            // for shifting right, the fast method is available when there is width enough.
            //  orig         slow result    fast result =>
            //  11111111     01111111       11111111       11111111   11111111  <-- and no more available.
            //  iiifffff     iiifffff       iiffffff       ifffffff   ffffffff
            //  the difference
            static constexpr bool EnableFastShiftRight = (kFractBits + amt) <= TypeInfo::MaxFractBits;
            static constexpr uint8_t FastRightShiftedIntBits = std::max(0, kIntBits - amt);
            using ShiftRightResultType =
                typename std::conditional<EnableFastShiftRight,
                                          Fixed<FastRightShiftedIntBits, kFractBits + amt, BaseType>,
                                          MyT>::type; // otherwise, the underlying value must be
                                                      // shifted, and the format doesn't change.
        };

    public:
        // Fast method (just change intbits & fractbits and use same value)
        template <uint8_t amt,
                  typename Helper = ShiftHelper<amt>,
                  typename ResultType = typename Helper::ShiftLeftResultType,
                  typename std::enable_if<Helper::EnableFastShiftLeft, int>::type = 0>
        constexpr CL_NODISCARD ResultType ShiftLeft() const
        {
            return ResultType::FromUnderlyingValue(mValue);
        }

        // "Slow" method (underlying value must be shifted)
        template <uint8_t amt,
                  typename Helper = ShiftHelper<amt>,
                  typename ResultType = typename Helper::ShiftLeftResultType,
                  typename std::enable_if<!Helper::EnableFastShiftLeft, int>::type = 0>
        constexpr CL_NODISCARD ResultType ShiftLeft() const
        {
            return ResultType::FromUnderlyingValue(mValue << amt);
        }

        // Fast method (just change intbits & fractbits and use same value)
        template <uint8_t amt,
                  typename Helper = ShiftHelper<amt>,
                  typename ResultType = typename Helper::ShiftRightResultType,
                  typename std::enable_if<Helper::EnableFastShiftRight, int>::type = 0>
        constexpr CL_NODISCARD ResultType ShiftRight() const
        {
            return ResultType::FromUnderlyingValue(mValue);
        }

        // "Slow" method (underlying value must be shifted)
        template <uint8_t amt,
                  typename Helper = ShiftHelper<amt>,
                  typename ResultType = typename Helper::ShiftRightResultType,
                  typename std::enable_if<!Helper::EnableFastShiftRight, int>::type = 0>
        constexpr CL_NODISCARD ResultType ShiftRight() const
        {
            return ResultType::FromUnderlyingValue(mValue >> amt);
        }

        // comparison
        // in general we use the difference_type because it has the right properties:
        // - unifies types
        // - preserves signs
        // - favors preserving intbits
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = DifferenceTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD bool IsGreaterThan(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            ResultType ta{*this};
            ResultType tb{b};
            return ta.mValue > tb.mValue;
        }

        // if you compare against an integral value shift off the fract bits and compare.
        // but need to make distinction when there's a non-zero fractional part.
        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool IsGreaterThan(T b) const
        {
            return HasNonZeroFractPart() ? (IntPart() >= b) : (IntPart() > b);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = DifferenceTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD bool IsGreaterThanOrEquals(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            ResultType ta{*this};
            ResultType tb{b};
            return ta.mValue >= tb.mValue;
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool IsGreaterThanOrEquals(T b) const
        {
            return IntPart() >= b;
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = DifferenceTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD bool IsLessThan(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            ResultType ta{*this};
            ResultType tb{b};
            return ta.mValue < tb.mValue;
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool IsLessThan(T b) const
        {
            return IntPart() < b;
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename TBaseTypeB,
                  typename TypeHelper = DifferenceTypeHelper<TIntBitsB, TFractBitsB, TBaseTypeB>,
                  typename ResultType = typename TypeHelper::ResultType>
        constexpr CL_NODISCARD bool IsLessThanOrEquals(const Fixed<TIntBitsB, TFractBitsB, TBaseTypeB> &b) const
        {
            ResultType ta{*this};
            ResultType tb{b};
            return ta.mValue <= tb.mValue;
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool IsLessThanOrEquals(T b) const
        {
            return HasNonZeroFractPart() ? (IntPart() < b) : (IntPart() <= b);
        }

        // clamp
        // intbits can be reduced to the min of operands.
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB,
                  uint8_t TIntBitsC,
                  uint8_t TFractBitsC,
                  typename BaseTypeC,
                  uint8_t TResultIntBits = std::min(kIntBits, std::min(TIntBitsB, TIntBitsC)),
                  typename ResultType = Fixed<TResultIntBits, kFractBits, BaseType>>
        constexpr CL_NODISCARD ResultType Clamp(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &lower,
                                                const Fixed<TIntBitsC, TFractBitsC, BaseTypeC> &upper) const
        {
            if (IsGreaterThan(upper))
            {
                return ResultType{upper};
            }
            if (IsLessThan(lower))
            {
                return ResultType{lower};
            }
            return ResultType{*this};
        }

        // store as int32_t to allow loading directly into register without extending. operations involving this table
        // python sine.py 256 257 -32767 32767
        static constexpr int16_t AudioWaveformSine[257] = {
            0, 804, 1608, 2410, 3212, 4011, 4808, 5602, 6393, 7179, 7962, 8739, 9512, 10278,
            11039, 11793, 12539, 13279, 14010, 14732, 15446, 16151, 16846, 17530, 18204, 18868, 19519, 20159,
            20787, 21403, 22005, 22594, 23170, 23731, 24279, 24811, 25329, 25832, 26319, 26790, 27245, 27683,
            28105, 28510, 28898, 29268, 29621, 29956, 30273, 30571, 30852, 31113, 31356, 31580, 31785, 31971,
            32137, 32285, 32412, 32521, 32609, 32678, 32728, 32757, 32767, 32757, 32728, 32678, 32609, 32521,
            32412, 32285, 32137, 31971, 31785, 31580, 31356, 31113, 30852, 30571, 30273, 29956, 29621, 29268,
            28898, 28510, 28105, 27683, 27245, 26790, 26319, 25832, 25329, 24811, 24279, 23731, 23170, 22594,
            22005, 21403, 20787, 20159, 19519, 18868, 18204, 17530, 16846, 16151, 15446, 14732, 14010, 13279,
            12539, 11793, 11039, 10278, 9512, 8739, 7962, 7179, 6393, 5602, 4808, 4011, 3212, 2410,
            1608, 804, 0, -804, -1608, -2410, -3212, -4011, -4808, -5602, -6393, -7179, -7962, -8739,
            -9512, -10278, -11039, -11793, -12539, -13279, -14010, -14732, -15446, -16151, -16846, -17530, -18204, -18868,
            -19519, -20159, -20787, -21403, -22005, -22594, -23170, -23731, -24279, -24811, -25329, -25832, -26319, -26790,
            -27245, -27683, -28105, -28510, -28898, -29268, -29621, -29956, -30273, -30571, -30852, -31113, -31356, -31580,
            -31785, -31971, -32137, -32285, -32412, -32521, -32609, -32678, -32728, -32757, -32767, -32757, -32728, -32678,
            -32609, -32521, -32412, -32285, -32137, -31971, -31785, -31580, -31356, -31113, -30852, -30571, -30273, -29956,
            -29621, -29268, -28898, -28510, -28105, -27683, -27245, -26790, -26319, -25832, -25329, -24811, -24279, -23731,
            -23170, -22594, -22005, -21403, -20787, -20159, -19519, -18868, -18204, -17530, -16846, -16151, -15446, -14732,
            -14010, -13279, -12539, -11793, -11039, -10278, -9512, -8739, -7962, -7179, -6393, -5602, -4808, -4011,
            -3212, -2410, -1608, -804, 0};

        // calculates sin() with period 0-1 over a fixed point integral value.
        // return value is a Q23 because it's what the interpolation natively returns.
        inline CL_NODISCARD Fixed<0, 23, int32_t> Sine_2pi() const
        {
            auto x1 = Fract();                    // normalize range
            auto x2 = Fixed<0, 16, uint16_t>{x1}; // convert to Q16 to ease calculations below (otherwise we're dipping into int64 territory)
            // if x2 fractbits were Q8, the mask would just be the integral fract portion. mValue & 0xff.
            // but it's a Q16 so shift out
            auto i1 = x2.mValue >> 8; // upper word
            auto a = AudioWaveformSine[i1];
            auto b = AudioWaveformSine[i1 + 1];
            // In order for the addition to work, a must be converted from Q15 to Q23.
            // (b-a)*xp is Q23 (Q15 + Q8)
            int32_t xp = x2.mValue & 0xff; // lower word = interpolate.
            auto t = (a << 8) + (b - a) * xp;
            using ResultType = Fixed<0, 23, int32_t>;
            return ResultType::FromUnderlyingValue(t);
        }

        // calculates sin() with period 0-1 over a fixed point integral value.
        // return value is a Q23 because it's what the interpolation natively returns.
        inline CL_NODISCARD Fixed<0, 15, int16_t> Sine_2pi_nointerp() const
        {
            auto x2 = Fixed<0, 8, uint8_t>{*this}; // convert to Q8 for direct LUT index
            auto a = AudioWaveformSine[x2.mValue & 0xff];
            return Fixed<0, 15, int16_t>::FromUnderlyingValue(a);
        }

        // smaller LUT with lower precision; fine for screen effects and never touches 32-bit types.
        static constexpr int8_t AudioWaveformSine16[] = {
            0, 49, 90, 117, 127, 117, 90, 49, 0, -49, -90, -117, -127, -117, -90, -49, 0 //
        };

        // calculates sin() with period 0-1 over a fixed point integral value
        // return value is a Q23 because it's what the interpolation natively returns.
        inline CL_NODISCARD Fixed<0, 11, int16_t> Sine16_2pi() const
        {
            auto x1 = Fract();                  // normalize range
            auto x2 = Fixed<0, 8, uint8_t>{x1}; // convert to Q8 to ease calculations
            auto i1 = x2.mValue >> 4;
            auto a = AudioWaveformSine16[i1]; // a is Q7
            auto b = AudioWaveformSine16[i1 + 1];
            int16_t xp = x2.mValue & 0xf;     // lower = interpolate.
            auto t = (a << 4) + (b - a) * xp; // 7+4 = Q11
            using ResultType = Fixed<0, 11, int16_t>;
            return ResultType::FromUnderlyingValue(t);
        }

        // calculates sin() with period 0-1 over a fixed point integral value.
        // return value is a Q23 because it's what the interpolation natively returns.
        inline CL_NODISCARD Fixed<0, 7, int8_t> Sine16_2pi_nointerp() const
        {
            auto x2 = Fixed<0, 4, uint8_t>{*this}; // convert to Q4 for direct LUT index
            auto a = AudioWaveformSine16[x2.mValue & 0xf];
            return Fixed<0, 7, int8_t>::FromUnderlyingValue(a);
        }

        // Calculates square root of a unit value. In other words, it cannot do sqrt of anything outside of [0,1)
        // TRUNCATES any integer part.
        // TRUNCATES sign bit.
        inline constexpr Fixed<0, 16, uint16_t> SqrtUnit() const
        {
            auto t = Fixed<0, 32, uint32_t>{*this};
            using ReturnType = Fixed<0, 16, uint16_t>;
            return ReturnType::FromUnderlyingValue(sqrt_Q32_to_Q16(t.mValue));
        }

        // non-mutating operators. only don't support any division operators.
        // *
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD auto operator*(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return Multiply(b);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD auto operator*(T b) const
        {
            return Multiply(b);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD auto operator/(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return Divide(b);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD auto operator%(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return Modulo(b);
        }

        constexpr CL_NODISCARD CorrespondingSignedType operator-() const
        {
            return Negate();
        }

        // +
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD auto operator+(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return Add(b);
        }

        // -
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD auto operator-(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return Subtract(b);
        }

        // >
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD bool operator>(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return IsGreaterThan(b);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool operator>(const T &b) const
        {
            return IsGreaterThan(b);
        }

        // >=
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD bool operator>=(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return IsGreaterThanOrEquals(b);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool operator>=(const T &b) const
        {
            return IsGreaterThanOrEquals(b);
        }

        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD bool operator<(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return IsLessThan(b);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool operator<(const T &b) const
        {
            return IsLessThan(b);
        }

        // <=
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD bool operator<=(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return IsLessThanOrEquals(b);
        }

        template <typename T, std::enable_if_t<std::is_integral<T>::value, int> = 0>
        constexpr CL_NODISCARD bool operator<=(const T &b) const
        {
            return IsLessThanOrEquals(b);
        }

        // ==
        template <uint8_t TIntBitsB,
                  uint8_t TFractBitsB,
                  typename BaseTypeB>
        constexpr CL_NODISCARD bool operator==(const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b) const
        {
            return IsApproximatelyEqualTo(b);
        }
    };

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // t MUST be [0,1]; if not it will get truncated not clamped. the reason for this is to make sure intbits has as much space as possible.
    template <uint8_t TIntBitsA,
              uint8_t TFractBitsA,
              typename BaseTypeA,
              uint8_t TIntBitsB,
              uint8_t TFractBitsB,
              typename BaseTypeB,
              uint8_t TIntBitsC,
              uint8_t TFractBitsC,
              typename BaseTypeC,
              typename TypeHelper = typename Fixed<TIntBitsA, TFractBitsA, BaseTypeA>::
                  template DifferenceTypeHelper<TIntBitsB, TFractBitsB, BaseTypeB>,
              typename ResultType = typename TypeHelper::ResultType>
    ResultType Lerp(const Fixed<TIntBitsA, TFractBitsA, BaseTypeA> &a,
                    const Fixed<TIntBitsB, TFractBitsB, BaseTypeB> &b,
                    const Fixed<TIntBitsC, TFractBitsC, BaseTypeC> &t01)
    {
        ResultType r{a + (b - a) * t01.Fract()}; // calling Fract() drops intbits to 0, keeping overhead under control.
        return r;
    }

    static inline Fixed<0, 16, uint16_t> sqrt(const Fixed<0, 32, uint32_t> &x)
    {
        using ReturnType = Fixed<0, 16, uint16_t>;
        return ReturnType::FromUnderlyingValue(sqrt_Q32_to_Q16(x.mValue));
    }

    // it would be nice (but not possible) to deduce the types during parsing the int, but because of syntax limitations
    // it's not possible. simply: the string is a param pack, and deduction would require more template params which i can't do.
    // fortunately at compile-time int64 is ok. just not at runtime.

    // var template args are so awkward to work with; for parsing i cannot use a call-and-return thing like,
    // while(chars left ) { if (next token is a number) parseNumber else ...
    // because of how the var args are passed around, i have to do everything in sequence chain.
    // parseNumber() needs to forward to the next phase.

    template <typename BaseType, char... chars>
    struct staticCharsToIntHelper
    {
        using max_int_t = int64_t; // don't use unsigned even for values with no signbit, to simplify narrowing.
        static constexpr int8_t kMaxUnsignedIntAvailableBits = 63;

        struct parse_int_result
        {
            max_int_t kIntValue; // always non-negative.
            int kSignBit;
            max_int_t kFractValue; // always non-negative.
            max_int_t kFractScale; // for parsing decimal parts, this is the denominator part. so parsing the "025" part of 0.025 results in a scale of 1000. (25/1000)
        };

        enum class ParsePhase
        {
            NumInt,
            NumFract,
        };

        template <
            ParsePhase Tphase,
            max_int_t TcompletedIntPart, // result storage
            // accumulation state
            max_int_t Taccumulator,
            max_int_t Tscale,
            int TsignBit,
            char head, char... tail //
            >
        static constexpr auto parse_signed_number()
        {
            constexpr bool headIsDigit = (head >= '0' && head <= '9');
            constexpr auto headAsDigit = head - '0';

            if constexpr (head == '-')
            {
                static_assert(TsignBit == 1, "parse error; multiple negative signs?");
                return parse_signed_number<Tphase, TcompletedIntPart, Taccumulator, Tscale, -TsignBit, tail...>();
            }

            constexpr auto nextAccumulator = headIsDigit ? (Taccumulator * 10 + headAsDigit) : Taccumulator;
            constexpr auto nextScale = headIsDigit ? (Tscale * 10) : Tscale;

            if constexpr (head == '.')
            {
                // we are finished parsing the int part.
                return parse_signed_number<ParsePhase::NumFract,
                                        nextAccumulator, // set TcompletedIntPart
                                        0, 1, 1, tail...>();
            }
            constexpr bool moreToParse = (sizeof...(tail) > 0);
            if constexpr (moreToParse)
            {
                return parse_signed_number<Tphase, TcompletedIntPart, nextAccumulator, nextScale, TsignBit, tail...>();
            }

            if constexpr (Tphase == ParsePhase::NumFract)
            {
                // the accumulator is the fractional part, scale is useful.
                return parse_int_result{TcompletedIntPart, TsignBit, nextAccumulator, nextScale};
            }

            return parse_int_result{nextAccumulator, TsignBit, 0, 1};
        }

        static constexpr auto ParseResult = parse_signed_number<ParsePhase::NumInt, 0, 0, 1, 1, chars...>();
        static constexpr int8_t kResultIntBits = StaticValueBitsNeeded<ParseResult.kIntValue>::value;
        static constexpr bool kIsSigned = (ParseResult.kSignBit < 0);
        static constexpr bool kHasFractPart = ParseResult.kFractValue != 0;
        using TypeInfo = FPTypeInfo<BaseType>;
        static constexpr int8_t kResultFractBits = kHasFractPart ? (TypeInfo::AvailableBits - kResultIntBits) : 0; // fill all bits with fract if it exists.
        // generate the fract part:
        // - make it as huge as possible in uint64_t
        // - divide by the scale
        // - convert to dest format & add to result.
        static constexpr int8_t kFractPartScaleBitsNeeded = StaticValueBitsNeeded<ParseResult.kFractScale>::value;
        static constexpr int8_t kFractTempAvailableBits = kMaxUnsignedIntAvailableBits;
        static constexpr int8_t kFractShift = kFractTempAvailableBits - kFractPartScaleBitsNeeded;
        static constexpr max_int_t kTempFractHuge = ParseResult.kFractValue << kFractShift;
        static constexpr max_int_t kTempFractDivided = kTempFractHuge / ParseResult.kFractScale;
        static constexpr int8_t kTempFractCorrectionShift = kResultFractBits - kFractShift;
        static constexpr BaseType kIntUnderlyingValue = (ParseResult.kIntValue << kResultFractBits) * ParseResult.kSignBit;
        static constexpr BaseType kFractUnderlyingValue = BaseType(const_shift<kTempFractCorrectionShift>(kTempFractDivided));
        using FixedType = Fixed<kResultIntBits, kResultFractBits, BaseType>;

        static constexpr auto FixedValue = FixedType::FromUnderlyingValue(kIntUnderlyingValue + kFractUnderlyingValue);

        // this is here for debugging; a compile-time double representation of the parsed value.
        static constexpr double kDoubleValue = double(kIntUnderlyingValue + kFractUnderlyingValue) / (1 << kResultFractBits);
    };

    template <typename T, T... chars>
    static constexpr auto operator""_fp()
    {
        using helper = staticCharsToIntHelper<FPGeneralBaseType, chars...>;
        return helper::FixedValue;
    }

    template <typename T, T... chars>
    static constexpr auto operator""_fp8()
    {
        using helper = staticCharsToIntHelper<int8_t, chars...>;
        return helper::FixedValue;
    }

    template <typename T, T... chars>
    static constexpr auto operator""_fp16()
    {
        using helper = staticCharsToIntHelper<int16_t, chars...>;
        return helper::FixedValue;
    }

    template <typename T, T... chars>
    static constexpr auto operator""_fp32()
    {
        using helper = staticCharsToIntHelper<int32_t, chars...>;
        return helper::FixedValue;
    }

    void fn()
    {

        // constexpr auto x = staticCharsToIntHelper<'1', '6'>::kSignedIntValue;
        // constexpr auto y = staticCharsToIntHelper<'1', '0', '0', '0'>::kSignedIntValue;

         using point325 = staticCharsToIntHelper<uint32_t, '3', '.', '2', '5'>;
         constexpr auto y1 = point325::kDoubleValue;
        // constexpr auto y3 = point325::ParseResult.kFractValue;
        // constexpr auto y2 = point325::ParseResult.kFractScale;
        // constexpr auto y5 = point325::FixedValue;
        constexpr auto xa = "-19.387987987"_fp32;
        DbgPrintF("%s", xa.ToString().str);

        //constexpr auto b = "12"_fp;
        //constexpr auto c = "-123.45"_fp;
        // constexpr auto x0 = "-6"_fp;
        // constexpr auto ao = "-65"_fp;
        // constexpr auto eu = " - 653"_fp;
        // constexpr auto e4 = "-6530"_fp;

        DbgPrintF("%d", 1);
    }

} // namespace clarinoid
