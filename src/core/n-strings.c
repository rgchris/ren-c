//
//  File: %n-strings.c
//  Summary: "native functions for strings"
//  Section: natives
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2017 Rebol Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
//=////////////////////////////////////////////////////////////////////////=//
//

#include "sys-core.h"

#include "datatypes/sys-money.h"


//
//  delimit: native [
//
//  {Joins a block of values into TEXT! with delimiters}
//
//      return: "Null if blank input or block's contents are all null"
//          [<opt> text!]
//      delimiter [<opt> blank! char! text!]
//      line "Will be copied if already a text value"
//          [<blank> text! block!]
//  ]
//
REBNATIVE(delimit)
{
    INCLUDE_PARAMS_OF_DELIMIT;

    REBVAL *line = ARG(line);
    if (IS_TEXT(line))
        return rebValueQ("copy", line, rebEND); // !!! Review performance

    assert(IS_BLOCK(line));

    if (Form_Reduce_Throws(
        D_OUT,
        VAL_ARRAY(line),
        VAL_INDEX(line),
        VAL_SPECIFIER(line),
        ARG(delimiter)
    )){
        return R_THROWN;
    }

    return D_OUT;
}


//
//  deflate: native [
//
//  "Compress data using DEFLATE: https://en.wikipedia.org/wiki/DEFLATE"
//
//      return: [binary!]
//      data "If text, it will be UTF-8 encoded"
//          [binary! text!]
//      /part "Length of data (elements)"
//          [any-value!]
//      /envelope "ZLIB (adler32, no size) or GZIP (crc32, uncompressed size)"
//          [word!]
//  ]
//
REBNATIVE(deflate)
{
    INCLUDE_PARAMS_OF_DEFLATE;

    REBLEN limit = Part_Len_May_Modify_Index(ARG(data), ARG(part));

    REBSIZ size;
    const REBYTE *bp = VAL_BYTES_LIMIT_AT(&size, ARG(data), limit);

    REBSTR *envelope;
    if (not REF(envelope))
        envelope = Canon(SYM_NONE);  // Note: nullptr is gzip (for bootstrap)
    else {
        envelope = VAL_WORD_SPELLING(ARG(envelope));
        switch (STR_SYMBOL(envelope)) {
          case SYM_ZLIB:
          case SYM_GZIP:
            break;

          default:
            fail (PAR(envelope));
        }
    }

    size_t compressed_size;
    void *compressed = Compress_Alloc_Core(
        &compressed_size,
        bp,
        size,
        envelope
    );

    return rebRepossess(compressed, compressed_size);
}


//
//  inflate: native [
//
//  "Decompresses DEFLATEd data: https://en.wikipedia.org/wiki/DEFLATE"
//
//      return: [binary!]
//      data [binary! handle!]
//      /part "Length of compressed data (must match end marker)"
//          [any-value!]
//      /max "Error out if result is larger than this"
//          [integer!]
//      /envelope "ZLIB, GZIP, or DETECT (http://stackoverflow.com/a/9213826)"
//          [word!]
//  ]
//
REBNATIVE(inflate)
//
// GZIP is a slight variant envelope which uses a CRC32 checksum.  For data
// whose original size was < 2^32 bytes, the gzip envelope stored that size...
// so memory efficiency is achieved even if max = -1.
//
// Note: That size guarantee exists for data compressed with rebGzipAlloc() or
// adhering to the gzip standard.  However, archives created with the GNU
// gzip tool make streams with possible trailing zeros or concatenations:
//
// http://stackoverflow.com/a/9213826
{
    INCLUDE_PARAMS_OF_INFLATE;

    REBINT max;
    if (REF(max)) {
        max = Int32s(ARG(max), 1);
        if (max < 0)
            fail (PAR(max));
    }
    else
        max = -1;

    REBYTE *data;
    REBSIZ size;
    if (IS_BINARY(ARG(data))) {
        data = VAL_BIN_AT(ARG(data));
        size = Part_Len_May_Modify_Index(ARG(data), ARG(part));
    }
    else {
        data = VAL_HANDLE_POINTER(REBYTE, ARG(data));
        size = VAL_HANDLE_LEN(ARG(data));
    }

    REBSTR *envelope;
    if (not REF(envelope))
        envelope = Canon(SYM_NONE);  // Note: nullptr is gzip (for bootstrap)
    else {
        switch (VAL_WORD_SYM(ARG(envelope))) {
          case SYM_ZLIB:
          case SYM_GZIP:
          case SYM_DETECT:
            envelope = VAL_WORD_SPELLING(ARG(envelope));
            break;

          default:
            fail (PAR(envelope));
        }
    }

    size_t decompressed_size;
    void *decompressed = Decompress_Alloc_Core(
        &decompressed_size,
        data,
        size,
        max,
        envelope
    );

    return rebRepossess(decompressed, decompressed_size);
}


//
//  debase: native [
//
//  {Decodes binary-coded string (BASE-64 default) to binary value.}
//
//      return: [binary!]
//          ; Comment said "we don't know the encoding" of the return binary
//      value [binary! text!]
//      /base "The base to convert from: 64, 16, or 2 (defaults to 64)"
//          [integer!]
//  ]
//
REBNATIVE(debase)
{
    INCLUDE_PARAMS_OF_DEBASE;

    REBSIZ size;
    const REBYTE *bp = VAL_BYTES_AT(&size, ARG(value));

    REBINT base = 64;
    if (REF(base))
        base = VAL_INT32(ARG(base));
    else
        base = 64;

    if (!Decode_Binary(D_OUT, bp, size, base, 0))
        fail (Error_Invalid_Data_Raw(ARG(value)));

    return D_OUT;
}


//
//  enbase: native [
//
//  {Encodes data into a binary, hexadecimal, or base-64 ASCII string.}
//
//      return: [text!]
//      value "If text, will be UTF-8 encoded"
//          [binary! text!]
//      /base "Binary base to use: 64, 16, or 2 (BASE-64 default)"
//          [integer!]
//  ]
//
REBNATIVE(enbase)
{
    INCLUDE_PARAMS_OF_ENBASE;

    REBINT base;
    if (REF(base))
        base = VAL_INT32(ARG(base));
    else
        base = 64;

    REBSIZ size;
    const REBYTE *bp = VAL_BYTES_AT(&size, ARG(value));

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    const bool brk = false;
    switch (base) {
      case 64:
        Form_Base64(mo, bp, size, brk);
        break;

      case 16:
        Form_Base16(mo, bp, size, brk);
        break;

      case 2:
        Form_Base2(mo, bp, size, brk);
        break;

      default:
        fail (PAR(base));
    }

    return Init_Text(D_OUT, Pop_Molded_String(mo));
}


//
//  enhex: native [
//
//  "Converts string to use URL-style hex encoding (%XX)"
//
//      return: [any-string!]
//          "See http://en.wikipedia.org/wiki/Percent-encoding"
//      string [any-string!]
//          "String to encode, all non-ASCII or illegal URL bytes encoded"
//  ]
//
REBNATIVE(enhex)
{
    INCLUDE_PARAMS_OF_ENHEX;

    // The details of what ASCII characters must be percent encoded
    // are contained in RFC 3896, but a summary is here:
    //
    // https://stackoverflow.com/a/7109208/
    //
    // Everything but: A-Z a-z 0-9 - . _ ~ : / ? # [ ] @ ! $ & ' ( ) * + , ; =
    //
  #if !defined(NDEBUG)
    const char *no_encode =
        "ABCDEFGHIJKLKMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" \
            "-._~:/?#[]@!$&'()*+,;=";
  #endif

    DECLARE_MOLD (mo);
    Push_Mold (mo);

    REBLEN len = VAL_LEN_AT(ARG(string));
    REBCHR(const*) cp = VAL_STRING_AT(ARG(string));

    REBUNI c;
    cp = NEXT_CHR(&c, cp);

    REBLEN i;
    for (i = 0; i < len; cp = NEXT_CHR(&c, cp), ++i) {
        //
        // !!! Length 4 should be legal here, but a warning in an older GCC
        // is complaining that Encode_UTF8_Char reaches out of array bounds
        // when it does not appear to.  Possibly related to this:
        //
        // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=43949
        //
        REBYTE encoded[6];
        REBLEN encoded_size;

        if (c >= 0x80)  // all non-ASCII characters *must* be percent encoded
            encoded_size = Encode_UTF8_Char(encoded, c);
        else {
            // "Everything else must be url-encoded".  Rebol's LEX_MAP does
            // not have a bit for this in particular, though maybe it could
            // be retooled to help more with this.  For now just use it to
            // speed things up a little.

            encoded[0] = cast(REBYTE, c);
            encoded_size = 1;

            switch (GET_LEX_CLASS(c)) {
            case LEX_CLASS_DELIMIT:
                switch (GET_LEX_VALUE(c)) {
                case LEX_DELIMIT_LEFT_PAREN:
                case LEX_DELIMIT_RIGHT_PAREN:
                case LEX_DELIMIT_LEFT_BRACKET:
                case LEX_DELIMIT_RIGHT_BRACKET:
                case LEX_DELIMIT_SLASH:
                case LEX_DELIMIT_SEMICOLON:
                    goto leave_as_is;

                case LEX_DELIMIT_SPACE: // includes control characters
                case LEX_DELIMIT_END: // 00 null terminator
                case LEX_DELIMIT_LINEFEED:
                case LEX_DELIMIT_RETURN: // e.g. ^M
                case LEX_DELIMIT_LEFT_BRACE:
                case LEX_DELIMIT_RIGHT_BRACE:
                case LEX_DELIMIT_DOUBLE_QUOTE:
                    goto needs_encoding;

                case LEX_DELIMIT_UTF8_ERROR: // not for c < 0x80
                default:
                    panic ("Internal LEX_DELIMIT table error");
                }
                goto leave_as_is;

            case LEX_CLASS_SPECIAL:
                switch (GET_LEX_VALUE(c)) {
                case LEX_SPECIAL_AT:
                case LEX_SPECIAL_COLON:
                case LEX_SPECIAL_APOSTROPHE:
                case LEX_SPECIAL_PLUS:
                case LEX_SPECIAL_MINUS:
                case LEX_SPECIAL_BLANK:
                case LEX_SPECIAL_PERIOD:
                case LEX_SPECIAL_COMMA:
                case LEX_SPECIAL_POUND:
                case LEX_SPECIAL_DOLLAR:
                    goto leave_as_is;

                default:
                    goto needs_encoding; // what is LEX_SPECIAL_WORD?
                }
                goto leave_as_is;

            case LEX_CLASS_WORD:
                if (
                    (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')
                    or c == '?' or c == '!' or c == '&'
                    or c == '*' or c == '=' or c == '~'
                ){
                    goto leave_as_is; // this is all that's leftover
                }
                goto needs_encoding;

            case LEX_CLASS_NUMBER:
                goto leave_as_is; // 0-9 needs no encoding.
            }

        leave_as_is:;
          #if !defined(NDEBUG)
            assert(strchr(no_encode, c) != NULL);
          #endif
            Append_Codepoint(mo->series, c);
            continue;
        }

    needs_encoding:;
      #if !defined(NDEBUG)
        if (c < 0x80)
           assert(strchr(no_encode, c) == NULL);
      #endif

        REBLEN n;
        for (n = 0; n != encoded_size; ++n) {
            Append_Codepoint(mo->series, '%');

            // Use uppercase hex digits, per RFC 3896 2.1, which is also
            // consistent with JavaScript's encodeURIComponent()
            //
            // https://tools.ietf.org/html/rfc3986#section-2.1
            //
            Append_Codepoint(mo->series, Hex_Digits[(encoded[n] & 0xf0) >> 4]);
            Append_Codepoint(mo->series, Hex_Digits[encoded[n] & 0xf]);
        }
    }

    Init_Any_String(D_OUT, VAL_TYPE(ARG(string)), Pop_Molded_String(mo));
    return D_OUT;
}


//
//  dehex: native [
//
//  "Converts URL-style encoded strings, %XX is interpreted as UTF-8 byte."
//
//      return: [any-string!]
//          "Decoded string, with the same string type as the input."
//      string [any-string!]
//          "See http://en.wikipedia.org/wiki/Percent-encoding"
//  ]
//
REBNATIVE(dehex)
{
    INCLUDE_PARAMS_OF_DEHEX;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    // RFC 3986 says the encoding/decoding must use UTF-8.  This temporary
    // buffer is used to hold up to 4 bytes (and a terminator) that need
    // UTF-8 decoding--the maximum one UTF-8 encoded codepoint may have.
    //
    REBYTE scan[5];
    REBSIZ scan_size = 0;

    REBLEN len = VAL_LEN_AT(ARG(string));
    REBCHR(const*) cp = VAL_STRING_AT(ARG(string));

    REBUNI c;
    cp = NEXT_CHR(&c, cp);

    REBLEN i;
    for (i = 0; i < len;) {
        if (c != '%')
            Append_Codepoint(mo->series, c);
        else {
            if (i + 2 >= len)
               fail ("Percent decode has less than two codepoints after %");

            cp = NEXT_CHR(&c, cp);
            ++i;
            if (c > UINT8_MAX)
                c = '\0'; // LEX_DELIMIT, will cause error below
            REBYTE lex1 = Lex_Map[cast(REBYTE, c)];

            cp = NEXT_CHR(&c, cp);
            ++i;
            if (c > UINT8_MAX)
                c = '\0'; // LEX_DELIMIT, will cause error below
            REBYTE lex2 = Lex_Map[cast(REBYTE, c)];

            // If class LEX_WORD or LEX_NUMBER, there is a value contained in
            // the mask which is the value of that "digit".  So A-F and
            // a-f can quickly get their numeric values.
            //
            REBYTE d1 = lex1 & LEX_VALUE;
            REBYTE d2 = lex2 & LEX_VALUE;

            if (
                lex1 < LEX_WORD or (d1 == 0 and lex1 < LEX_NUMBER)
                or lex2 < LEX_WORD or (d2 == 0 and lex2 < LEX_NUMBER)
            ){
                fail ("Percent must be followed by 2 hex digits, e.g. %XX");
            }

            // !!! We might optimize here for ASCII codepoints, but would
            // need to consider it a "flushing point" for the scan buffer,
            // in order to not gloss over incomplete UTF-8 sequences.
            //
            REBYTE b = (d1 << 4) + d2;
            scan[scan_size++] = b;
        }

        cp = NEXT_CHR(&c, cp); // c may be '\0', guaranteed if `i == len`
        ++i;

        // If our scanning buffer is full (and hence should contain at *least*
        // one full codepoint) or there are no more UTF-8 bytes coming (due
        // to end of string or the next input not a %XX pattern), then try
        // to decode what we've got.
        //
        if (scan_size > 0 and (c != '%' or scan_size == 4)) {
            assert(i != len or c == '\0');

          decode_codepoint:
            scan[scan_size] = '\0';
            const REBYTE *next; // goto would cross initialization
            REBUNI decoded;
            if (scan[0] < 0x80) {
                decoded = scan[0];
                next = &scan[0]; // last byte is only byte (see Back_Scan)
            }
            else {
                next = Back_Scan_UTF8_Char(&decoded, scan, &scan_size);
                if (next == NULL)
                    fail ("Bad UTF-8 sequence in %XX of dehex");
            }
            Append_Codepoint(mo->series, decoded);
            --scan_size; // one less (see why it's called "Back_Scan")

            // Slide any residual UTF-8 data to the head of the buffer
            //
            REBLEN n;
            for (n = 0; n < scan_size; ++n) {
                ++next; // pre-increment (see why it's called "Back_Scan")
                scan[n] = *next;
            }

            // If we still have bytes left in the buffer and no more bytes
            // are coming, this is the last chance to decode those bytes,
            // keep going.
            //
            if (scan_size != 0 and c != '%')
                goto decode_codepoint;
        }
    }

    Init_Any_String(D_OUT, VAL_TYPE(ARG(string)), Pop_Molded_String(mo));
    return D_OUT;
}


//
//  deline: native [
//
//  {Converts string terminators to standard format, e.g. CR LF to LF.}
//
//      return: [any-string! block!]
//      string "Will be modified (unless /LINES used)"
//          [any-string!]
//      /lines "Return block of lines (works for LF, CR, CR-LF endings)"
//  ]
//
REBNATIVE(deline)
{
    INCLUDE_PARAMS_OF_DELINE;

    REBVAL *val = ARG(string);

    if (REF(lines))
        return Init_Block(D_OUT, Split_Lines(val));

    REBSTR *s = VAL_STRING(val);
    REBLEN len_head = STR_LEN(s);

    REBLEN len_at = VAL_LEN_AT(val);

    REBCHR(*) dest = VAL_STRING_AT(val);
    REBCHR(const*) src = dest;

    REBLEN n;
    for (n = 0; n < len_at;) {
        REBUNI c;
        src = NEXT_CHR(&c, src);
        ++n;
        if (c == CR) {
            dest = WRITE_CHR(dest, LF);
            src = NEXT_CHR(&c, src);
            ++n; // will see NUL terminator before loop check, so is safe
            if (c == LF) {
                --len_head; // don't write carraige return, note loss of char
                continue;
            }
        }
        dest = WRITE_CHR(dest, c);
    }

    TERM_STR_LEN_SIZE(s, len_head, dest - VAL_STRING_AT(val));

    RETURN (ARG(string));
}


//
//  enline: native [
//
//  {Converts string terminators to native OS format, e.g. LF to CRLF.}
//
//      return: [any-string!]
//      string [any-string!] "(modified)"
//  ]
//
REBNATIVE(enline)
{
    INCLUDE_PARAMS_OF_ENLINE;

    REBVAL *val = ARG(string);

    REBSTR *s = VAL_STRING(val);
    REBLEN idx = VAL_INDEX(val);

    REBLEN len;
    REBSIZ size = VAL_SIZE_LIMIT_AT(&len, val, UNKNOWN);

    REBLEN delta = 0;

    // Calculate the size difference by counting the number of LF's
    // that have no CR's in front of them.
    //
    // !!! The REBCHR(*) interface isn't technically necessary if one is
    // counting to the end (one could just go by bytes instead of characters)
    // but this would not work if someone added, say, an ENLINE/PART...since
    // the byte ending position of interest might not be end of the string.

    REBCHR(*) cp = STR_AT(s, idx);

    REBUNI c_prev = '\0';

    REBLEN n;
    for (n = 0; n < len; ++n) {
        REBUNI c;
        cp = NEXT_CHR(&c, cp);
        if (c == LF and c_prev != CR)
            ++delta;
        c_prev = c;
    }

    if (delta == 0)
        RETURN (ARG(string)); // nothing to do

    REBLEN old_len = MISC(s).length;
    EXPAND_SERIES_TAIL(SER(s), delta);  // corrupts MISC(str).length
    MISC(s).length = old_len + delta;  // just adding CR's

    // One feature of using UTF-8 for strings is that CR/LF substitution can
    // stay a byte-oriented process..because UTF-8 doesn't reuse bytes in the
    // ASCII range, and CR and LF are ASCII.  So as long as the "sliding" is
    // done in terms of byte sizes and not character lengths, it should work.

    Free_Bookmarks_Maybe_Null(s);  // !!! Could this be avoided sometimes?

    REBYTE* bp = STR_HEAD(s); // expand may change the pointer
    REBSIZ tail = STR_SIZE(s); // size in bytes after expansion

    // Add missing CRs

    while (delta > 0) {
        bp[tail--] = bp[size]; // Copy src to dst.
        if (bp[size] == LF and (size == 0 or bp[size - 1] != CR)) {
            bp[tail--] = CR;
            --delta;
        }
        --size;
    }

    RETURN (ARG(string));
}


//
//  entab: native [
//
//  "Converts spaces to tabs (default tab size is 4)."
//
//      string "(modified)"
//          [any-string!]
//      /size "Specifies the number of spaces per tab"
//          [integer!]
//  ]
//
REBNATIVE(entab)
{
    INCLUDE_PARAMS_OF_ENTAB;

    REBINT tabsize;
    if (REF(size))
        tabsize = Int32s(ARG(size), 1);
    else
        tabsize = TAB_SIZE;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    REBLEN len = VAL_LEN_AT(ARG(string));

    REBCHR(const*) up = VAL_STRING_AT(ARG(string));
    REBLEN index = VAL_INDEX(ARG(string));

    REBINT n = 0;
    for (; index < len; index++) {
        REBUNI c;
        up = NEXT_CHR(&c, up);

        // Count leading spaces, insert TAB for each tabsize:
        if (c == ' ') {
            if (++n >= tabsize) {
                Append_Codepoint(mo->series, '\t');
                n = 0;
            }
            continue;
        }

        // Hitting a leading TAB resets space counter:
        if (c == '\t') {
            Append_Codepoint(mo->series, '\t');
            n = 0;
        }
        else {
            // Incomplete tab space, pad with spaces:
            for (; n > 0; n--)
                Append_Codepoint(mo->series, ' ');

            // Copy chars thru end-of-line (or end of buffer):
            for (; index < len; ++index) {
                if (c == '\n') {
                    //
                    // !!! The original code didn't seem to actually move the
                    // append pointer, it just changed the last character to
                    // a newline.  Was this the intent?
                    //
                    Append_Codepoint(mo->series, '\n');
                    break;
                }
                Append_Codepoint(mo->series, c);
                up = NEXT_CHR(&c, up);
            }
        }
    }

    enum Reb_Kind kind = VAL_TYPE(ARG(string));
    return Init_Any_String(D_OUT, kind, Pop_Molded_String(mo));
}


//
//  detab: native [
//
//  "Converts tabs to spaces (default tab size is 4)."
//
//      string "(modified)"
//          [any-string!]
//      /size "Specifies the number of spaces per tab"
//          [integer!]
//  ]
//
REBNATIVE(detab)
{
    INCLUDE_PARAMS_OF_DETAB;

    REBLEN len = VAL_LEN_AT(ARG(string));

    REBINT tabsize;
    if (REF(size))
        tabsize = Int32s(ARG(size), 1);
    else
        tabsize = TAB_SIZE;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    // Estimate new length based on tab expansion:

    REBCHR(const*) cp = VAL_STRING_AT(ARG(string));
    REBLEN index = VAL_INDEX(ARG(string));

    REBLEN n = 0;

    for (; index < len; ++index) {
        REBUNI c;
        cp = NEXT_CHR(&c, cp);

        if (c == '\t') {
            Append_Codepoint(mo->series, ' ');
            n++;
            for (; n % tabsize != 0; n++)
                Append_Codepoint(mo->series, ' ');
            continue;
        }

        if (c == '\n')
            n = 0;
        else
            ++n;

        Append_Codepoint(mo->series, c);
    }

    enum Reb_Kind kind = VAL_TYPE(ARG(string));
    return Init_Any_String(D_OUT, kind, Pop_Molded_String(mo));
}


//
//  lowercase: native [
//
//  "Converts string of characters to lowercase."
//
//      string "(modified if series)"
//          [any-string! char!]
//      /part "Limits to a given length or position"
//          [any-number! any-string!]
//  ]
//
REBNATIVE(lowercase)
{
    INCLUDE_PARAMS_OF_LOWERCASE;

    Change_Case(D_OUT, ARG(string), ARG(part), false);
    return D_OUT;
}


//
//  uppercase: native [
//
//  "Converts string of characters to uppercase."
//
//      string "(modified if series)"
//          [any-string! char!]
//      /part "Limits to a given length or position"
//          [any-number! any-string!]
//  ]
//
REBNATIVE(uppercase)
{
    INCLUDE_PARAMS_OF_UPPERCASE;

    Change_Case(D_OUT, ARG(string), ARG(part), true);
    return D_OUT;
}


//
//  to-hex: native [
//
//  {Converts numeric value to a hex issue! datatype (with leading # and 0's).}
//
//      value [integer! tuple!]
//      /size "Specify number of hex digits in result"
//          [integer!]
//  ]
//
REBNATIVE(to_hex)
{
    INCLUDE_PARAMS_OF_TO_HEX;

    REBVAL *arg = ARG(value);

    REBLEN len;
    if (REF(size))
        len = cast(REBLEN, VAL_INT64(ARG(size)));
    else
        len = UNKNOWN;

    DECLARE_MOLD (mo);
    Push_Mold(mo);

    if (IS_INTEGER(arg)) {
        if (len == UNKNOWN || len > MAX_HEX_LEN)
            len = MAX_HEX_LEN;

        Form_Hex_Pad(mo, VAL_INT64(arg), len);
    }
    else if (IS_TUPLE(arg)) {
        REBLEN n;
        if (
            len == UNKNOWN
            || len > 2 * MAX_TUPLE
            || len > cast(REBLEN, 2 * VAL_TUPLE_LEN(arg))
        ){
            len = 2 * VAL_TUPLE_LEN(arg);
        }
        for (n = 0; n != VAL_TUPLE_LEN(arg); n++)
            Form_Hex2(mo, VAL_TUPLE(arg)[n]);
        for (; n < 3; n++)
            Form_Hex2(mo, 0);
    }
    else
        fail (PAR(value));

    // !!! Issue should be able to use string from mold buffer directly when
    // UTF-8 Everywhere unification of ANY-WORD! and ANY-STRING! is done.
    //
    assert(len == STR_SIZE(mo->series) - mo->offset);
    if (NULL == Scan_Issue(D_OUT, BIN_AT(SER(mo->series), mo->offset), len))
        fail (PAR(value));

    Drop_Mold(mo);
    return D_OUT;
}


//
//  find-script: native [
//
//  {Find a script header within a binary string. Returns starting position.}
//
//      return: [<opt> binary! text!]
//      script [binary! text!]
//  ]
//
REBNATIVE(find_script)
{
    INCLUDE_PARAMS_OF_FIND_SCRIPT;

    REBVAL *arg = ARG(script);

    REBSIZ size;
    const REBYTE *bp = VAL_BYTES_AT(&size, arg);

    REBINT offset = Scan_Header(bp, size);
    if (offset == -1)
        return nullptr;

    Move_Value(D_OUT, arg);

    if (IS_BINARY(arg)) {  // may not all be valid UTF-8
        VAL_INDEX(D_OUT) += offset;
        return D_OUT;
    }

    assert(IS_TEXT(arg));  // we know it was all valid UTF-8

    // Discover the codepoint index of the offset (this conceptually repeats
    // work in Scan_Header(), but since that works on arbitrary binaries it
    // doesn't always have a codepoint delta to return with the offset.)

    const REBYTE *header_bp = bp + offset;

    REBLEN index = VAL_INDEX(arg);
    REBCHR(*) cp = VAL_STRING_AT(arg);
    for (; cp != header_bp; cp = NEXT_STR(cp))
        ++index;

    VAL_INDEX(D_OUT) = index;
    return D_OUT;
}


//
//  invalid-utf8?: native [
//
//  {Checks UTF-8 encoding}
//
//      return: "NULL if correct, otherwise position in binary of the error"
//          [<opt> binary!]
//      data [binary!]
//  ]
//
REBNATIVE(invalid_utf8_q)
//
// !!! A motivation for adding this native was because R3-Alpha did not fully
// validate UTF-8 input, for perceived reasons of performance:
//
// https://github.com/rebol/rebol-issues/issues/638
//
// Ren-C reinstated full validation, as it only causes a hit when a non-ASCII
// sequence is read (which is relatively rare in Rebol).  However, it is
// helpful to have a function that will locate invalid byte sequences if one
// is going to try doing something like substituting a character at the
// invalid positions.
{
    INCLUDE_PARAMS_OF_INVALID_UTF8_Q;

    REBVAL *arg = ARG(data);
    REBYTE *utf8 = VAL_BIN_AT(arg);
    REBSIZ size = VAL_LEN_AT(arg);

    REBYTE *end = utf8 + size;

    REBLEN trail;
    for (; utf8 != end; utf8 += trail) {
        trail = trailingBytesForUTF8[*utf8] + 1;
        if (utf8 + trail > end or not isLegalUTF8(utf8, trail)) {
            Move_Value(D_OUT, arg);
            VAL_INDEX(D_OUT) = utf8 - VAL_BIN_HEAD(arg);
            return D_OUT;
        }
    }

    return nullptr;  // no invalid byte found
}
