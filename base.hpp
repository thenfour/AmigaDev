#pragma once

#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include <proto/dos.h>
#include <exec/execbase.h>
#include <hardware/intbits.h>
#include <exec/types.h>

using uint8_t = UBYTE;
using int8_t = BYTE;
using uint16_t = USHORT;
using int16_t = SHORT;
using uint32_t = ULONG;
using int32_t = LONG;
using uint64_t = unsigned long long;
using int64_t = long long;

using uintptr_t = uint32_t;
using size_t = long unsigned int;

#define CL_NODISCARD

enum class NumberBase
{
    Binary,
    Decimal,
    Hexadecimal
};

// will not null-term

// Function to safely copy a null-terminated string into a buffer
USHORT strCopy(char *dest, const char *src)
{
    USHORT i = 0;
    while (src[i] != '\0')
    {
        dest[i] = src[i];
        ++i;
    }
    dest[i] = '\0'; // Ensure null termination
    return i;       // Number of characters copied (excluding null terminator)
}

// returns # of characters written, not including nullterm.
template <typename T>
USHORT intToStrT(char *buffer, T absValue, NumberBase base, bool isNegative)
{
    char *start = buffer;
    if (absValue == 0)
    {
        if (isNegative)
        {
            *buffer++ = '-';
            *buffer++ = '0';
            *buffer++ = 0;
            return 2;
        }

        *buffer++ = '0';
        *buffer++ = 0;
        return 1;
    }
    else
    {
        // Convert the number to string in reverse order
        do
        {
            T digit = absValue % (base == NumberBase::Decimal ? 10 : (base == NumberBase::Hexadecimal ? 16 : 2));
            if (digit < 10)
            {
                *buffer++ = '0' + digit;
            }
            else
            {
                *buffer++ = 'a' + (digit - 10);
            }
            absValue /= (base == NumberBase::Decimal ? 10 : (base == NumberBase::Hexadecimal ? 16 : 2));
        } while (absValue > 0);

        if (isNegative)
        {
            *buffer++ = '-';
        }

        // Reverse the string
        *buffer = 0;
        USHORT ret = buffer - start;
        char *end = buffer - 1;
        while (start < end)
        {
            char temp = *start;
            *start++ = *end;
            *end-- = temp;
        }
        return ret;
    }
}

// returns # of characters written, not including nullterm.
template <typename T>
USHORT intToStrT(char *buffer, T value, NumberBase base)
{
    bool isNegative = value < 0;
    return intToStrT(buffer, isNegative ? -value : value, base, isNegative);
}

inline USHORT intToStr(char *buffer, USHORT value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}

inline USHORT intToStr(char *buffer, SHORT value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}

inline USHORT intToStr(char *buffer, BYTE value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}

inline USHORT intToStr(char *buffer, UBYTE value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}

inline USHORT intToStr(char *buffer, int32_t value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}
inline USHORT intToStr(char *buffer, uint32_t value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}
inline USHORT intToStr(char *buffer, int value, NumberBase base)
{
    return intToStrT(buffer, value, base);
}

template <typename T>
inline USHORT intToStr(char *buffer, T value, NumberBase base)
{
    return 0;
}

inline void formatDecimal(char *str, const char *format)
{
    while (*format != '\0')
    {
        *str++ = *format++;
    }
    *str = '\0';
}

// like sprintf but only supporting %d, %x
template <typename T, typename... Args>
inline void formatDecimal(char *str, const char *format, T value, Args... args)
{
    *str = 0;
    while (*format)
    {
        if (*format == '%')
        {
            format++;
            if (*format == 'd')
            {
                str += intToStr(str, value, NumberBase::Decimal);
                formatDecimal(str, format + 1, args...); // Recursive call
                return;
            }
            else if (*format == 'x')
            {
                str += intToStr(str, value, NumberBase::Hexadecimal);
                formatDecimal(str, format + 1, args...); // Recursive call
                return;
            }
            else if (*format == 'b')
            {
                str += intToStr(str, value, NumberBase::Binary);
                formatDecimal(str, format + 1, args...); // Recursive call
                return;
            }
            else if (*format == 's')
            {
                const char *stringValue = reinterpret_cast<const char *>(value);
                USHORT len = strCopy(str, stringValue);
                formatDecimal(str + len, format + 1, args...); // Recursive call
                return;
            }
        }
        *str++ = *format++;
    }
}

inline void DbgPrintStr(const char *str)
{
    long (*UaeDbgLog)(long mode, const char *string) = (long (*)(long, const char *))0xf0ff60;
    if (*((UWORD *)UaeDbgLog) == 0x4eb9 || *((UWORD *)UaeDbgLog) == 0xa00e)
    {
        UaeDbgLog(86, str);
    }
    // else what?
}

template <typename T, typename... Args>
inline void DbgPrintF(const char *format, T value, Args... args)
{
    char buffer[200];
    formatDecimal(buffer, format, value, args...);
    DbgPrintStr(buffer);
}

// convenient overload when no formatting.
inline void debug_text_format(short x, short y, uint32_t color, const char *str)
{
    debug_text(x, y, str, color);
}

template <typename T, typename... Args>
inline void debug_text_format(short x, short y, uint32_t color, const char *format, T value, Args... args)
{
    char buffer[200];
    formatDecimal(buffer, format, value, args...);
    debug_text_format(x, y, color, buffer);
}

// if you pass in 7, return %01111111
constexpr inline USHORT setRightmostBits(UBYTE bitCount)
{
    return (1 << bitCount) - 1;
}
// similar, but sets the leftmost bits instead of rightmost.
constexpr inline USHORT setLeftmostBits(UBYTE bitCount)
{
    return ~setRightmostBits(16 - bitCount);
}

inline constexpr uint32_t operator"" _b(const char *str, size_t)
{
    uint32_t result = 0;
    while (*str != '\0')
    {
        switch (*str)
        {
        case '1':
            result <<= 1;
            result |= 1;
            break;
        case '0':
            result <<= 1;
            break;
        default:
            break;
        }
        ++str;
    }
    return result;
}

// template <typename T>
// inline constexpr T CombineFlags(T a, T b)
// {
//     return (T)((int)a | (int)b);
// }

template <typename T, typename... Args>
inline constexpr T CombineFlags(T first, Args... args)
{
    return (T)((int)first | ... | (int)args);
}

template <typename T>
inline static constexpr bool is_flag_set(T flagValue, T flagToTest)
{
    return ((int)flagValue & (int)flagToTest) == (int)flagToTest;
}

namespace std
{

    // Implementation of integral_constant
    template <typename T, T v>
    struct integral_constant
    {
        static constexpr T value = v;
        typedef T value_type;
        typedef integral_constant type;
        constexpr operator value_type() const noexcept { return value; }
    };

    // Define true_type and false_type
    using true_type = integral_constant<bool, true>;
    using false_type = integral_constant<bool, false>;

    template <bool B, class T = void>
    struct enable_if
    {
    };

    template <class T>
    struct enable_if<true, T>
    {
        typedef T type;
    };

    // Implementation of std::is_same
    template <typename T, typename U>
    struct is_same : false_type
    {
    };

    template <typename T>
    struct is_same<T, T> : true_type
    {
    };

    // Helper variable template for std::is_same
    template <typename T, typename U>
    inline constexpr bool is_same_v = is_same<T, U>::value;

    // Implementation of is_integral
    template <typename T>
    struct is_integral : false_type
    {
    };
    template <>
    struct is_integral<bool> : true_type
    {
    };
    template <>
    struct is_integral<char> : true_type
    {
    };
    template <>
    struct is_integral<signed char> : true_type
    {
    };
    template <>
    struct is_integral<unsigned char> : true_type
    {
    };
    template <>
    struct is_integral<short> : true_type
    {
    };
    template <>
    struct is_integral<unsigned short> : true_type
    {
    };
    template <>
    struct is_integral<int> : true_type
    {
    };
    template <>
    struct is_integral<unsigned int> : true_type
    {
    };
    template <>
    struct is_integral<long> : true_type
    {
    };
    template <>
    struct is_integral<unsigned long> : true_type
    {
    };
    template <>
    struct is_integral<long long> : true_type
    {
    };
    template <>
    struct is_integral<unsigned long long> : true_type
    {
    };

    // Implementation of is_floating_point
    template <typename T>
    struct is_floating_point : false_type
    {
    };
    template <>
    struct is_floating_point<float> : true_type
    {
    };
    template <>
    struct is_floating_point<double> : true_type
    {
    };
    template <>
    struct is_floating_point<long double> : true_type
    {
    };

    // Implementation of is_arithmetic
    template <typename T>
    struct is_arithmetic : integral_constant<bool, is_integral<T>::value || is_floating_point<T>::value>
    {
    };

    // Implementation of is_signed
    template <typename T>
    struct is_signed : integral_constant<bool, is_arithmetic<T>::value && T(-1) < T(0)>
    {
    };

    template <typename T>
    inline constexpr bool is_signed_v = is_signed<T>::value;

    // Implementation of is_unsigned
    template <typename T>
    struct is_unsigned : integral_constant<bool, is_arithmetic<T>::value && T(0) < T(-1)>
    {
    };
    template <typename T>
    inline constexpr bool is_unsigned_v = is_unsigned<T>::value;

    // Custom implementation of convertible_to type trait
    template <typename From, typename To>
    struct convertible_to
    {
    private:
        // Two helper functions to check convertibility
        static void test(To);
        static std::false_type test(...);

        static From make_from();

    public:
        // Check if calling test(make_from()) chooses the test(To) overload
        static constexpr bool value = std::is_same_v<decltype(test(make_from())), void>;
    };

    // Helper variable template for ease of use
    template <typename From, typename To>
    inline constexpr bool convertible_to_v = convertible_to<From, To>::value;

    template <bool _Test, class _Ty = void>
    using enable_if_t = typename enable_if<_Test, _Ty>::type;

    template <bool _Test, class _Ty1, class _Ty2>
    struct conditional
    { // Choose _Ty1 if _Test is true, and _Ty2 otherwise
        using type = _Ty1;
    };

    template <class _Ty1, class _Ty2>
    struct conditional<false, _Ty1, _Ty2>
    {
        using type = _Ty2;
    };

    template <bool _Test, class _Ty1, class _Ty2>
    using conditional_t = typename conditional<_Test, _Ty1, _Ty2>::type;

    template <typename T>
    constexpr T min(const T &a, const T &b)
    {
        return (a < b) ? a : b;
    }

    template <typename T, typename... Args>
    constexpr T min(T a, T b, Args... args)
    {
        return min(min(a, b), args...);
    }

    template <typename T>
    constexpr T max(const T &a, const T &b)
    {
        return (a > b) ? a : b;
    }

    template <typename T, typename... Args>
    constexpr T max(T a, T b, Args... args)
    {
        return max(max(a, b), args...);
    }

} // namespace std

namespace cc
{

    void __assert(bool condition, const char *msg, const char *conditionAsString, const char *file, int line)
    {
        if (condition)
            return;
        DbgPrintF("ASSERTION FAILED: (%s) : %s (file:%s:%d)", conditionAsString, msg, file, line);
        Exit(0);
    }

}

#define ASSERT(condition, msg) (cc::__assert(condition, msg, #condition, __FILE__, __LINE__))

namespace cc
{
    template <int TrangeStart, int TrangeSize>
    inline int wrap_into_range(int x)
    {
        constexpr int rangeEnd = TrangeStart + TrangeSize - 1;
        int wrappedValue;

        if (x >= TrangeStart && x <= rangeEnd)
        {
            return x; // Value is already within range
        }
        int adjustedValue = x - TrangeStart;
        int adjustedRange = rangeEnd - TrangeStart + 1;

        wrappedValue = adjustedValue % adjustedRange;
        if (wrappedValue < 0)
        {
            wrappedValue += adjustedRange;
        }
        wrappedValue += TrangeStart;

        return wrappedValue;
    }

    template <int TrangeStart, int TrangeSize>
    inline int clamp_into_range(int x)
    {
        constexpr int rangeEnd = TrangeStart + TrangeSize - 1;

        if (x < TrangeStart)
        {
            return TrangeStart;
        }
        else if (x > rangeEnd)
        {
            return rangeEnd;
        }
        else
        {
            return x;
        }
    }

    template <typename T, size_t N>
    struct Array
    {
        // Array elements
        T data[N];

        // Size of the array
        constexpr size_t size() const { return N; }

        // Access elements
        constexpr T &operator[](size_t index) { return data[index]; }
        constexpr const T &operator[](size_t index) const { return data[index]; }

        // Methods to get pointers to the data
        constexpr T *begin() { return &data[0]; }
        constexpr const T *begin() const { return &data[0]; }
        constexpr T *end() { return &data[N]; }
        constexpr const T *end() const { return &data[N]; }

        // Fill the array with a value
        constexpr void fill(const T &value)
        {
            for (size_t i = 0; i < N; ++i)
            {
                data[i] = value;
            }
        }
    };

    static constexpr bool isPowerOfTwo(int value)
    {
        return value != 0 && (value & (value - 1)) == 0;
    }

} // namespace cc