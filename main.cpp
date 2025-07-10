//
//  main.cpp
//
//  Copyright © 2025 Robert Guequierre
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

import std;

#include <cassert>
#include <cstdint>
#include <cstdlib>

/*
123
123l
123i
123s
123b

123u
123ul
123ui
123us
123ub

0x12
0x12l
0x12i
0x12s
0x12b

0x12u
0x12ul
0x12ui
0x12us
0x12ub


0b10
0b10l
0b10i
0b10s
0b10b

0b10u
0b10ul
0b10ui
0b10us
0b10ub

[|0x|0b] DEC|HEX|BIN [|u][|l|i|s|b]

[Sized][Base2][Unsigned]IntegerLiteral

0b1000'0xyz

...
[Sized][Base2][Exp]FloatLiteral
 */

//====----------------------------------------------------------------------====
// * Expectations
//====----------------------------------------------------------------------====

static_assert( std::is_unsigned_v<char>, "Compile with -funsigned-char" );

//====----------------------------------------------------------------------====
//
// * Lex category
//
//====----------------------------------------------------------------------====

enum class LexCategory : uint8_t
{
    Space      = 0,
    RangeBegin = 1,
    RangeEnd   = 2,
    Symbol     = 3,
    Digit      = 4, // Decimal
    Identifier = 5,
    Unused     = 0x80,
    Invalid    = 0xff
};

//====----------------------------------------------------------------------====
//
// * Character value types (Direct)
//
//====----------------------------------------------------------------------====

constexpr bool isDecimalDigit(const uint32_t value) noexcept
{
    static_assert( '0' < '9' );

    return '0' <= value && value <= '9';
}

constexpr bool isHexDigit(const uint32_t value) noexcept
{
    static_assert( 'A' < 'F' );
    static_assert( 'a' < 'f' );

    return isDecimalDigit(value) 
        || ('A' <= value && value <= 'F')
        || ('a' <= value && value <= 'f');
}

constexpr bool isBinaryDigit(const uint32_t value) noexcept
{
    return '0' == value || '1' == value;
}

constexpr bool canBeginIdentifierASCII(const uint32_t value) noexcept
{
    static_assert( 'A' < 'Z' );
    static_assert( 'a' < 'z' );

    return ('A' <= value && value <= 'Z')
        || ('_' == value)
        || ('a' <= value && value <= 'z');
}

constexpr bool canContinueIdentifierASCII(const uint32_t value) noexcept
{
    return canBeginIdentifierASCII(value) || isDecimalDigit(value);
}

//====----------------------------------------------------------------------====
//
// * Lex character map
//
//====----------------------------------------------------------------------====

struct LexCharacterMap
{
    uint8_t     value;
    LexCategory begin;
    bool        isHexDigit;
    bool        isBinaryDigit;
    bool        canContinueIdentifier;
    uint8_t     pairedValue;
};

static_assert( 1 == alignof(LexCharacterMap), "Unexpected alignment" );
static_assert( 6 ==  sizeof(LexCharacterMap), "Unexpected size" );

//====----------------------------------------------------------------------====
//
// * Character value types (Mapped)
//
//====----------------------------------------------------------------------====

namespace detail
{

constexpr bool isDecimalDigit(const LexCharacterMap& map) noexcept
{
    return LexCategory::Digit == map.begin;
}

constexpr bool isHexDigit(const LexCharacterMap& map) noexcept
{
    return map.isHexDigit;
}

constexpr bool isBinaryDigit(const LexCharacterMap& map) noexcept
{
    return map.isBinaryDigit;
}

constexpr bool canBeginIdentifier(const LexCharacterMap& map) noexcept
{
    return LexCategory::Identifier == map.begin;
}

constexpr bool canContinueIdentifier(const LexCharacterMap& map) noexcept
{
    return map.canContinueIdentifier;
}

} // namespace detail

//====----------------------------------------------------------------------====
//
// * ASCII Character to map
// 
//   Adapted from https://www.unicode.org/Public/MAPPINGS/ISO8859/8859-1.TXT
//   Terms of use in UNICODE-LICENSE-V3.txt
//
//====----------------------------------------------------------------------====

namespace detail
{

using enum LexCategory;

const LexCharacterMap ASCIILexCharacterMap[] = {

    // val  begin       HEX    BIN    id+    pair
    { 0x00, Invalid,    false, false, false, 0x00 },
    { 0x01, Invalid,    false, false, false, 0x00 },
    { 0x02, Invalid,    false, false, false, 0x00 },
    { 0x03, Invalid,    false, false, false, 0x00 },
    { 0x04, Invalid,    false, false, false, 0x00 },
    { 0x05, Invalid,    false, false, false, 0x00 },
    { 0x06, Invalid,    false, false, false, 0x00 },
    { 0x07, Invalid,    false, false, false, 0x00 },
    { 0x08, Invalid,    false, false, false, 0x00 },
    { 0x09, Invalid,    false, false, false, 0x00 },
    { 0x0a, Invalid,    false, false, false, 0x00 },
    { 0x0b, Invalid,    false, false, false, 0x00 },
    { 0x0c, Invalid,    false, false, false, 0x00 },
    { 0x0d, Invalid,    false, false, false, 0x00 },
    { 0x0e, Invalid,    false, false, false, 0x00 },
    { 0x0f, Invalid,    false, false, false, 0x00 },
    { 0x10, Invalid,    false, false, false, 0x00 },
    { 0x11, Invalid,    false, false, false, 0x00 },
    { 0x12, Invalid,    false, false, false, 0x00 },
    { 0x13, Invalid,    false, false, false, 0x00 },
    { 0x14, Invalid,    false, false, false, 0x00 },
    { 0x15, Invalid,    false, false, false, 0x00 },
    { 0x16, Invalid,    false, false, false, 0x00 },
    { 0x17, Invalid,    false, false, false, 0x00 },
    { 0x18, Invalid,    false, false, false, 0x00 },
    { 0x19, Invalid,    false, false, false, 0x00 },
    { 0x1a, Invalid,    false, false, false, 0x00 },
    { 0x1b, Invalid,    false, false, false, 0x00 },
    { 0x1c, Invalid,    false, false, false, 0x00 },
    { 0x1d, Invalid,    false, false, false, 0x00 },
    { 0x1e, Invalid,    false, false, false, 0x00 },
    { 0x1f, Invalid,    false, false, false, 0x00 },

    // val  begin       HEX    BIN    id+    pair
    { 0x20, Space,      false, false, false, 0x00 }, // ' ' SPACE
    { 0x21, Symbol,     false, false, false, 0x00 }, // '!' EXCLAMATION MARK
    { 0x22, RangeBegin, false, false, false, '\"' }, // '\"' QUOTATION MARK
    { 0x23, Symbol,     false, false, false, 0x00 }, // '#' NUMBER SIGN
    { 0x24, Unused,     false, false, false, 0x00 }, // '$' DOLLAR SIGN
    { 0x25, Symbol,     false, false, false, 0x00 }, // '%' PERCENT SIGN
    { 0x26, Symbol,     false, false, false, 0x00 }, // '&' AMPERSAND
    { 0x27, RangeBegin, false, false, false, '\'' }, // '\'' APOSTROPHE
    { 0x28, RangeBegin, false, false, false, ')'  }, // '(' LEFT PARENTHESIS
    { 0x29, RangeEnd,   false, false, false, '('  }, // ')' RIGHT PARENTHESIS
    { 0x2a, Symbol,     false, false, false, 0x00 }, // '*' ASTERISK
    { 0x2b, Symbol,     false, false, false, 0x00 }, // '+' PLUS SIGN
    { 0x2c, Symbol,     false, false, false, 0x00 }, // ',' COMMA
    { 0x2d, Symbol,     false, false, false, 0x00 }, // '-' HYPHEN-MINUS
    { 0x2e, Symbol,     false, false, false, 0x00 }, // '.' FULL STOP
    { 0x2f, Symbol,     false, false, false, 0x00 }, // '/' SOLIDUS
    { 0x30, Digit,      true,  true,  true,  0x00 }, // '0' DIGIT ZERO
    { 0x31, Digit,      true,  true,  true,  0x00 }, // '1' DIGIT ONE
    { 0x32, Digit,      true,  false, true,  0x00 }, // '2' DIGIT TWO
    { 0x33, Digit,      true,  false, true,  0x00 }, // '3' DIGIT THREE
    { 0x34, Digit,      true,  false, true,  0x00 }, // '4' DIGIT FOUR
    { 0x35, Digit,      true,  false, true,  0x00 }, // '5' DIGIT FIVE
    { 0x36, Digit,      true,  false, true,  0x00 }, // '6' DIGIT SIX
    { 0x37, Digit,      true,  false, true,  0x00 }, // '7' DIGIT SEVEN
    { 0x38, Digit,      true,  false, true,  0x00 }, // '8' DIGIT EIGHT
    { 0x39, Digit,      true,  false, true,  0x00 }, // '9' DIGIT NINE
    { 0x3a, Symbol,     false, false, false, 0x00 }, // ':' COLON
    { 0x3b, Symbol,     false, false, false, 0x00 }, // ';' SEMICOLON
    { 0x3c, Symbol,     false, false, false, 0x00 }, // '<' LESS-THAN SIGN
    { 0x3d, Symbol,     false, false, false, 0x00 }, // '=' EQUALS SIGN
    { 0x3e, Symbol,     false, false, false, 0x00 }, // '>' GREATER-THAN SIGN
    { 0x3f, Symbol,     false, false, false, 0x00 }, // '?' QUESTION MARK

    // val  begin       HEX    BIN    id+    pair
    { 0x40, Unused,     false, false, false, 0x00 }, // '@' COMMERCIAL AT
    { 0x41, Identifier, true,  false, true,  0x00 }, // 'A' LATIN CAPITAL LETTER A
    { 0x42, Identifier, true,  false, true,  0x00 }, // 'B' LATIN CAPITAL LETTER B
    { 0x43, Identifier, true,  false, true,  0x00 }, // 'C' LATIN CAPITAL LETTER C
    { 0x44, Identifier, true,  false, true,  0x00 }, // 'D' LATIN CAPITAL LETTER D
    { 0x45, Identifier, true,  false, true,  0x00 }, // 'E' LATIN CAPITAL LETTER E
    { 0x46, Identifier, true,  false, true,  0x00 }, // 'F' LATIN CAPITAL LETTER F
    { 0x47, Identifier, false, false, true,  0x00 }, // 'G' LATIN CAPITAL LETTER G
    { 0x48, Identifier, false, false, true,  0x00 }, // 'H' LATIN CAPITAL LETTER H
    { 0x49, Identifier, false, false, true,  0x00 }, // 'I' LATIN CAPITAL LETTER I
    { 0x4a, Identifier, false, false, true,  0x00 }, // 'J' LATIN CAPITAL LETTER J
    { 0x4b, Identifier, false, false, true,  0x00 }, // 'K' LATIN CAPITAL LETTER K
    { 0x4c, Identifier, false, false, true,  0x00 }, // 'L' LATIN CAPITAL LETTER L
    { 0x4d, Identifier, false, false, true,  0x00 }, // 'M' LATIN CAPITAL LETTER M
    { 0x4e, Identifier, false, false, true,  0x00 }, // 'N' LATIN CAPITAL LETTER N
    { 0x4f, Identifier, false, false, true,  0x00 }, // 'O' LATIN CAPITAL LETTER O
    { 0x50, Identifier, false, false, true,  0x00 }, // 'P' LATIN CAPITAL LETTER P
    { 0x51, Identifier, false, false, true,  0x00 }, // 'Q' LATIN CAPITAL LETTER Q
    { 0x52, Identifier, false, false, true,  0x00 }, // 'R' LATIN CAPITAL LETTER R
    { 0x53, Identifier, false, false, true,  0x00 }, // 'S' LATIN CAPITAL LETTER S
    { 0x54, Identifier, false, false, true,  0x00 }, // 'T' LATIN CAPITAL LETTER T
    { 0x55, Identifier, false, false, true,  0x00 }, // 'U' LATIN CAPITAL LETTER U
    { 0x56, Identifier, false, false, true,  0x00 }, // 'V' LATIN CAPITAL LETTER V
    { 0x57, Identifier, false, false, true,  0x00 }, // 'W' LATIN CAPITAL LETTER W
    { 0x58, Identifier, false, false, true,  0x00 }, // 'X' LATIN CAPITAL LETTER X
    { 0x59, Identifier, false, false, true,  0x00 }, // 'Y' LATIN CAPITAL LETTER Y
    { 0x5a, Identifier, false, false, true,  0x00 }, // 'Z' LATIN CAPITAL LETTER Z
    { 0x5b, RangeBegin, false, false, false, ']'  }, // '[' LEFT SQUARE BRACKET
    { 0x5c, Unused,     false, false, false, 0x00 }, // '\\' REVERSE SOLIDUS
    { 0x5d, RangeEnd,   false, false, false, '['  }, // ']' RIGHT SQUARE BRACKET
    { 0x5e, Symbol,     false, false, false, 0x00 }, // '^' CIRCUMFLEX ACCENT
    { 0x5f, Identifier, false, false, true,  0x00 }, // '_' LOW LINE

    // val  begin       HEX    BIN    id+    pair
    { 0x60, RangeBegin, false, false, false, '`'  }, // '`' GRAVE ACCENT
    { 0x61, Identifier, true,  false, true,  0x00 }, // 'a' LATIN SMALL LETTER A
    { 0x62, Identifier, true,  false, true,  0x00 }, // 'b' LATIN SMALL LETTER B
    { 0x63, Identifier, true,  false, true,  0x00 }, // 'c' LATIN SMALL LETTER C
    { 0x64, Identifier, true,  false, true,  0x00 }, // 'd' LATIN SMALL LETTER D
    { 0x65, Identifier, true,  false, true,  0x00 }, // 'e' LATIN SMALL LETTER E
    { 0x66, Identifier, true,  false, true,  0x00 }, // 'f' LATIN SMALL LETTER F
    { 0x67, Identifier, false, false, true,  0x00 }, // 'g' LATIN SMALL LETTER G
    { 0x68, Identifier, false, false, true,  0x00 }, // 'h' LATIN SMALL LETTER H
    { 0x69, Identifier, false, false, true,  0x00 }, // 'i' LATIN SMALL LETTER I
    { 0x6a, Identifier, false, false, true,  0x00 }, // 'j' LATIN SMALL LETTER J
    { 0x6b, Identifier, false, false, true,  0x00 }, // 'k' LATIN SMALL LETTER K
    { 0x6c, Identifier, false, false, true,  0x00 }, // 'l' LATIN SMALL LETTER L
    { 0x6d, Identifier, false, false, true,  0x00 }, // 'm' LATIN SMALL LETTER M
    { 0x6e, Identifier, false, false, true,  0x00 }, // 'n' LATIN SMALL LETTER N
    { 0x6f, Identifier, false, false, true,  0x00 }, // 'o' LATIN SMALL LETTER O
    { 0x70, Identifier, false, false, true,  0x00 }, // 'p' LATIN SMALL LETTER P
    { 0x71, Identifier, false, false, true,  0x00 }, // 'q' LATIN SMALL LETTER Q
    { 0x72, Identifier, false, false, true,  0x00 }, // 'r' LATIN SMALL LETTER R
    { 0x73, Identifier, false, false, true,  0x00 }, // 's' LATIN SMALL LETTER S
    { 0x74, Identifier, false, false, true,  0x00 }, // 't' LATIN SMALL LETTER T
    { 0x75, Identifier, false, false, true,  0x00 }, // 'u' LATIN SMALL LETTER U
    { 0x76, Identifier, false, false, true,  0x00 }, // 'v' LATIN SMALL LETTER V
    { 0x77, Identifier, false, false, true,  0x00 }, // 'w' LATIN SMALL LETTER W
    { 0x78, Identifier, false, false, true,  0x00 }, // 'x' LATIN SMALL LETTER X
    { 0x79, Identifier, false, false, true,  0x00 }, // 'y' LATIN SMALL LETTER Y
    { 0x7a, Identifier, false, false, true,  0x00 }, // 'z' LATIN SMALL LETTER Z
    { 0x7b, RangeBegin, false, false, false, '}'  }, // '{' LEFT CURLY BRACKET
    { 0x7c, Symbol,     false, false, false, 0x00 }, // '|' VERTICAL LINE
    { 0x7d, RangeEnd,   false, false, false, '{'  }, // '}' RIGHT CURLY BRACKET
    { 0x7e, Symbol,     false, false, false, 0x00 }, // '~' TILDE
    { 0x7f, Invalid,    false, false, false, 0x00 }, // DELETE
};

static_assert( 128 == std::size(ASCIILexCharacterMap) );

} // namespace detail

//====----------------------------------------------------------------------====
//
// * Character value types (ASCII table)
//
//====----------------------------------------------------------------------====

constexpr bool isDecimalDigit2(const uint32_t value) noexcept
{
    return ( value < std::size(detail::ASCIILexCharacterMap) )
        ? detail::isDecimalDigit(detail::ASCIILexCharacterMap[value])
        : false;
}

constexpr bool isHexDigit2(const uint32_t value) noexcept
{
    return ( value < std::size(detail::ASCIILexCharacterMap) )
        ? detail::isHexDigit(detail::ASCIILexCharacterMap[value])
        : false;
}

constexpr bool isBinaryDigit2(const uint32_t value) noexcept
{
    return ( value < std::size(detail::ASCIILexCharacterMap) )
        ? detail::isBinaryDigit(detail::ASCIILexCharacterMap[value])
        : false;
}

constexpr bool canBeginIdentifierASCII2(const uint32_t value) noexcept
{
    return ( value < std::size(detail::ASCIILexCharacterMap) )
        ? detail::canBeginIdentifier(detail::ASCIILexCharacterMap[value])
        : false;
}

constexpr bool canContinueIdentifierASCII2(const uint32_t value) noexcept
{
    return ( value < std::size(detail::ASCIILexCharacterMap) )
        ? detail::canContinueIdentifier(detail::ASCIILexCharacterMap[value])
        : false;
}

//====----------------------------------------------------------------------====
//
// * Simple testing framework
//
//====----------------------------------------------------------------------====

namespace detail
{

// Adapted from: https://fekir.info/post/cpp-test-suite-in-100-lines-of-code/
//
void expect( std::string_view     expectation, 
             bool                 cond, 
             std::string_view     expr, 
             std::source_location loc ) noexcept
{
    if ( !cond ) {
        std::println( std::cerr, "Exected {} at line {} in {}: {}",
                      expectation, loc.line(), loc.file_name(), expr );
    }
}

} // namespace detail

#define EXPECT_TRUE(expr_) \
    detail::expect( "true", expr_, #expr_, std::source_location::current() )

#define EXPECT_FALSE(expr_) \
    detail::expect( "false", !(expr_), #expr_, std::source_location::current() )

//====----------------------------------------------------------------------====
//
// * Tests
//
//====----------------------------------------------------------------------====

void testDecimalDigitsASCII(void) noexcept
{
    static_assert( '0' < '9' );

    auto value = uint32_t{ 0 };

    for ( ; value < '0'; ++value ) {
        EXPECT_FALSE( isDecimalDigit(value) );
        EXPECT_FALSE( isDecimalDigit2(value) );
    }

    // '0'...'9'
    for ( ; value <= '9'; ++value ) {
        EXPECT_TRUE( isDecimalDigit(value) );
        EXPECT_TRUE( isDecimalDigit2(value) );
    }

    for ( ; value < 128u; ++value ) {
        EXPECT_FALSE( isDecimalDigit(value) );
        EXPECT_FALSE( isDecimalDigit2(value) );
    }
}

void testHexDigitsASCII(void) noexcept
{
    static_assert( '0' < '9' );
    static_assert( '9' < 'A' );
    static_assert( 'A' < 'F' );
    static_assert( 'F' < 'a' );
    static_assert( 'a' < 'f' );

    auto value = uint32_t{ 0 };

    for ( ; value < '0'; ++value ) {
        EXPECT_FALSE( isHexDigit(value) );
        EXPECT_FALSE( isHexDigit2(value) );
    }

    // '0'...'9'
    for ( ; value <= '9'; ++value ) {
        EXPECT_TRUE( isHexDigit(value) );
        EXPECT_TRUE( isHexDigit2(value) );
    }

    for ( ; value < 'A'; ++value ) {
        EXPECT_FALSE( isHexDigit(value) );
        EXPECT_FALSE( isHexDigit2(value) );
    }

    // 'A'...'F'
    for ( ; value <= 'F'; ++value ) {
        EXPECT_TRUE( isHexDigit(value) );
        EXPECT_TRUE( isHexDigit2(value) );
    }

    for ( ; value < 'a'; ++value ) {
        EXPECT_FALSE( isHexDigit(value) );
        EXPECT_FALSE( isHexDigit2(value) );
    }

    // 'a'...'f'
    for ( ; value <= 'f'; ++value ) {
        EXPECT_TRUE( isHexDigit(value) );
        EXPECT_TRUE( isHexDigit2(value) );
    }

    for ( ; value < 128u; ++value ) {
        EXPECT_FALSE( isHexDigit(value) );
        EXPECT_FALSE( isHexDigit2(value) );
    }
}

void testBinaryDigitsASCII(void) noexcept
{
    static_assert( '0' < '1' );

    auto value = uint32_t{ 0 };

    for ( ; value < '0'; ++value ) {
        EXPECT_FALSE( isBinaryDigit(value) );
        EXPECT_FALSE( isBinaryDigit2(value) );
    }

    // '0'...'1'
    for ( ; value <= '1'; ++value ) {
        EXPECT_TRUE( isBinaryDigit(value) );
        EXPECT_TRUE( isBinaryDigit2(value) );
    }

    for ( ; value < 128u; ++value ) {
        EXPECT_FALSE( isBinaryDigit(value) );
        EXPECT_FALSE( isBinaryDigit2(value) );
    }
}

void testBeginIdentifierASCII(void) noexcept
{
    static_assert( 'A' < 'Z' );
    static_assert( 'Z' < '_' );
    static_assert( '_' < 'a' );
    static_assert( 'a' < 'z' );

    auto value = uint32_t{ 0 };

    for ( ; value < 'A'; ++value ) {
        EXPECT_FALSE( canBeginIdentifierASCII(value) );
        EXPECT_FALSE( canBeginIdentifierASCII2(value) );
    }

    // 'A'...'Z'
    for ( ; value <= 'Z'; ++value ) {
        EXPECT_TRUE( canBeginIdentifierASCII(value) );
        EXPECT_TRUE( canBeginIdentifierASCII2(value) );
    }

    for ( ; value < '_'; ++value ) {
        EXPECT_FALSE( canBeginIdentifierASCII(value) );
        EXPECT_FALSE( canBeginIdentifierASCII2(value) );
    }

    // '_'
    EXPECT_TRUE( canBeginIdentifierASCII(value) );
    EXPECT_TRUE( canBeginIdentifierASCII2(value) );
    ++value;

    for ( ; value < 'a'; ++value ) {
        EXPECT_FALSE( canBeginIdentifierASCII(value) );
        EXPECT_FALSE( canBeginIdentifierASCII2(value) );
    }

    // 'a'...'z'
    for ( ; value <= 'z'; ++value ) {
        EXPECT_TRUE( canBeginIdentifierASCII(value) );
        EXPECT_TRUE( canBeginIdentifierASCII2(value) );
    }

    for ( ; value < 128u; ++value ) {
        EXPECT_FALSE( canBeginIdentifierASCII(value) );
        EXPECT_FALSE( canBeginIdentifierASCII2(value) );
    }
}

void testContinueIdentifierASCII(void) noexcept
{
    static_assert( '0' < '9' );
    static_assert( '9' < 'A' );
    static_assert( 'A' < 'Z' );
    static_assert( 'Z' < '_' );
    static_assert( '_' < 'a' );
    static_assert( 'a' < 'z' );

    auto value = uint32_t{ 0 };

    for ( ; value < '0'; ++value ) {
        EXPECT_FALSE( canContinueIdentifierASCII(value) );
        EXPECT_FALSE( canContinueIdentifierASCII2(value) );
    }

    // '0'...'9'
    for ( ; value <= '9'; ++value ) {
        EXPECT_TRUE( canContinueIdentifierASCII(value) );
        EXPECT_TRUE( canContinueIdentifierASCII2(value) );
    }

    for ( ; value < 'A'; ++value ) {
        EXPECT_FALSE( canContinueIdentifierASCII(value) );
        EXPECT_FALSE( canContinueIdentifierASCII2(value) );
    }

    // 'A'...'Z'
    for ( ; value <= 'Z'; ++value ) {
        EXPECT_TRUE( canContinueIdentifierASCII(value) );
        EXPECT_TRUE( canContinueIdentifierASCII2(value) );
    }

    for ( ; value < '_'; ++value ) {
        EXPECT_FALSE( canContinueIdentifierASCII(value) );
        EXPECT_FALSE( canContinueIdentifierASCII2(value) );
    }

    // '_'
    EXPECT_TRUE( canContinueIdentifierASCII(value) );
    EXPECT_TRUE( canContinueIdentifierASCII2(value) );
    ++value;
    
    for ( ; value < 'a'; ++value ) {
        EXPECT_FALSE( canContinueIdentifierASCII(value) );
        EXPECT_FALSE( canContinueIdentifierASCII2(value) );
    }

    // 'a'...'z'
    for ( ; value <= 'z'; ++value ) {
        EXPECT_TRUE( canContinueIdentifierASCII(value) );
        EXPECT_TRUE( canContinueIdentifierASCII2(value) );
    }

    for ( ; value < 128u; ++value ) {
        EXPECT_FALSE( canContinueIdentifierASCII(value) );
        EXPECT_FALSE( canContinueIdentifierASCII2(value) );
    }
}

void testASCIITokenCharacterMap(void) noexcept
{
    static_assert( 128u == std::size(detail::ASCIILexCharacterMap) );

    using enum LexCategory;

    for (const auto& map : detail::ASCIILexCharacterMap)
    {
        if ( RangeBegin == map.begin || RangeEnd == map.begin )
        {
            EXPECT_TRUE( 0x00 != map.pairedValue );
        }
        else
        {
            EXPECT_TRUE( 0x00 == map.pairedValue );
        }

        if ( Identifier == map.begin )
        {
            EXPECT_FALSE( map.isBinaryDigit );
            EXPECT_TRUE ( map.canContinueIdentifier );
        }
        else if ( Digit == map.begin )
        {
            EXPECT_TRUE( map.isHexDigit );
            EXPECT_TRUE( map.canContinueIdentifier );
        }
        else
        {
            EXPECT_FALSE( map.isHexDigit );
            EXPECT_FALSE( map.isBinaryDigit );
            EXPECT_FALSE( map.canContinueIdentifier );
        }
    }

    // Further expectations about what should be tested will continue to become
    //  apparent over time. Add them as they arise
}

//====----------------------------------------------------------------------====
// * main
//====----------------------------------------------------------------------====

int main( [[maybe_unused]] const int         argc,
          [[maybe_unused]] const char* const argv[] )
{
    testDecimalDigitsASCII();
    testHexDigitsASCII();
    testBinaryDigitsASCII();
    testBeginIdentifierASCII();
    testContinueIdentifierASCII();
    testASCIITokenCharacterMap();

    return EXIT_SUCCESS;
}

