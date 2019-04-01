//
//  File: %s-make.c
//  Summary: "binary and unicode string support"
//  Section: strings
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



//
//  Make_String_Core: C
//
// Makes a series to hold a string with enough capacity for a certain amount
// of encoded data.  Make_Unicode() is how more conservative guesses might
// be made, but better to either know -or- go via the mold buffer to get
// the exact right length.
//
REBSTR *Make_String_Core(REBSIZ encoded_capacity, REBFLGS flags)
{
    // !!! Even though this is a byte sized sequence, we add 2 bytes for the
    // terminator (and TERM_SEQUENCE() terminates with 2 bytes) because we're
    // in a stopgap position where the series contains REBUNIs, and sometimes
    // the null terminator is visited in enumerations.
    //
    REBSER *s = Make_Series_Core(
        encoded_capacity + 1,
        sizeof(REBYTE),
        flags | SERIES_FLAG_IS_STRING | SERIES_FLAG_UTF8_NONWORD
    );
    MISC(s).length = 0;
    LINK(s).bookmarks = nullptr;  // generated on demand
    TERM_SERIES(s);
    return STR(s);
}


//
//  Make_Unicode: C
//
// !!! This is a very conservative string generator for UTF-8 content which
// assumes that you could need as much as 4 bytes per codepoint.  It's not a
// new issue, considering that R3-Alpha had to come up with a string size
// not knowing whether the string would need 1 or 2 byte codepoints.  But
// for internal strings, getting this wrong and not doing an expansion could
// be a bug.  Just make big strings for now.
//
REBSTR *Make_Unicode(REBCNT codepoint_capacity)
{
    REBSTR *s = Make_String(codepoint_capacity * 2);
    ASSERT_SERIES_TERM(SER(s));
    return s;
}


//
//  Copy_Bytes: C
//
// Create a string series from the given bytes.
// Source is always latin-1 valid. Result is always 8bit.
//
REBSER *Copy_Bytes(const REBYTE *src, REBINT len)
{
    if (len < 0)
        len = LEN_BYTES(src);

    REBSER *dst = Make_Binary(len);
    memcpy(BIN_HEAD(dst), src, len);
    TERM_SEQUENCE_LEN(dst, len);

    return dst;
}


//
//  Insert_Char: C
//
// Insert a unicode char into a string.
//
void Insert_Char(REBSTR *dst, REBCNT index, REBCNT chr)
{
    if (index > STR_LEN(dst))
        index = STR_LEN(dst);
    Expand_Series(SER(dst), index, 4);  // !!! 4 is max codepoint size, review
    SET_CHAR_AT(dst, index, chr);
}


//
//  Copy_String_At_Limit: C
//
// !!! With UTF-8 Everywhere, copying strings will still be distinct from
// other series due to the length being counted in characters and not
// units of the series width.
//
REBSTR *Copy_String_At_Limit(const RELVAL *src, REBINT limit)
{
    REBCNT length_limit;
    REBSIZ size = VAL_SIZE_LIMIT_AT(&length_limit, src, limit);
    assert(length_limit <= size);

    REBSTR *dst = Make_String(size);
    memcpy(STR_HEAD(dst), VAL_STRING_AT(src), size);
    TERM_STR_LEN_SIZE(dst, length_limit, size);

    return dst;
}


//
//  Append_Codepoint: C
//
// Encode a codepoint onto the end of a UTF-8 string series.  This is used
// frequently by molding.
//
// !!! Should the mold buffer avoid paying for termination?  Might one save on
// resizing checks if an invalid UTF-8 byte were used to mark the end of the
// capacity (the way END markers are used on the data stack?)
//
REBSTR *Append_Codepoint(REBSTR *dst, REBUNI codepoint)
{
    assert(codepoint <= MAX_UNI);
    assert(not IS_STR_SYMBOL(dst));

    REBCNT old_len = STR_LEN(dst);

    // 4 bytes maximum for UTF-8 encoded character (6 is a lie)
    //
    // https://stackoverflow.com/a/9533324/211160
    //
    REBSIZ tail = STR_SIZE(dst);
    EXPAND_SERIES_TAIL(SER(dst), 4); // !!! Conservative, assume big codepoint
    tail += Encode_UTF8_Char(BIN_AT(SER(dst), tail), codepoint);

    // "length" grew by 1 codepoint, but "size" grew by 1 to 4 bytes
    //
    TERM_STR_LEN_SIZE(dst, old_len + 1, tail);

    return dst;
}


//
//  Make_Codepoint_String: C
//
// Create a string that holds a single codepoint.
//
REBSTR *Make_Codepoint_String(REBUNI codepoint)
{
    assert(codepoint <= MAX_UNI);

    REBSTR *s = Make_Unicode(1);
    TERM_STR_LEN_SIZE(s, 1, Encode_UTF8_Char(STR_HEAD(s), codepoint));
    return s;
}


//
//  Append_Ascii_Len: C
//
// Append unencoded data to a byte string, using plain memcpy().  If dst is
// NULL, a new byte-sized series will be created and returned.
//
// !!! Should debug build assert it's ASCII?  Most of these are coming from
// string literals in the source.
//
REBSTR *Append_Ascii_Len(REBSTR *dst, const char *ascii, REBCNT len)
{
    REBCNT old_size;
    REBCNT old_len;

    if (dst == NULL) {
        dst = Make_String(len);
        old_size = 0;
        old_len = 0;
    }
    else {
        old_size = STR_SIZE(dst);
        old_len = STR_LEN(dst);
        EXPAND_SERIES_TAIL(SER(dst), len);
    }

    memcpy(BIN_AT(SER(dst), old_size), ascii, len);

    TERM_STR_LEN_SIZE(dst, old_len + len, old_size + len);
    return dst;
}


//
//  Append_Ascii: C
//
// Append_Ascii_Len() variant that looks for a terminating 0 byte to
// determine the length.  Assumes one byte per character.
//
// !!! Should be in a header file so it can be inlined.
//
REBSTR *Append_Ascii(REBSTR *dst, const char *src)
{
    return Append_Ascii_Len(dst, src, strlen(src));
}


//
//  Append_Utf8: C
//
// Append a UTF8 byte series to a UTF8 binary.  Terminates.
//
REBSTR *Append_Utf8(REBSTR *dst, const char *utf8, size_t size)
{
    const bool crlf_to_lf = false;
    return Append_UTF8_May_Fail(dst, utf8, size, crlf_to_lf);
}


//
//  Append_Spelling: C
//
// Append the spelling of a REBSTR to a UTF8 binary.  Terminates.
//
void Append_Spelling(REBSTR *dst, REBSTR *spelling)
{
    Append_Utf8(dst, STR_UTF8(spelling), STR_SIZE(spelling));
}


//
//  Append_String: C
//
// Append a partial string to a UTF-8 binary series.
//
void Append_String(REBSTR *dst, const REBCEL *src, REBCNT limit)
{
    assert(not IS_STR_SYMBOL(dst));
    assert(ANY_STRING_KIND(CELL_KIND(src)));

    REBSIZ offset = VAL_OFFSET_FOR_INDEX(src, VAL_INDEX(src));

    REBCNT old_len = STR_LEN(dst);
    REBSIZ old_used = STR_SIZE(dst);

    REBCNT len;
    REBSIZ size = VAL_SIZE_LIMIT_AT(&len, src, limit);

    REBCNT tail = STR_SIZE(dst);
    Expand_Series(SER(dst), tail, size);  // series USED changes too

    memcpy(BIN_AT(SER(dst), tail), BIN_AT(VAL_SERIES(src), offset), size);
    TERM_STR_LEN_SIZE(dst, old_len + len, old_used + size);
}


//
//  Append_Int: C
//
// Append an integer string.
//
void Append_Int(REBSTR *dst, REBINT num)
{
    REBYTE buf[32];
    Form_Int(buf, num);

    Append_Ascii(dst, s_cast(buf));
}


//
//  Append_Int_Pad: C
//
// Append an integer string.
//
void Append_Int_Pad(REBSTR *dst, REBINT num, REBINT digs)
{
    REBYTE buf[32];
    if (digs > 0)
        Form_Int_Pad(buf, num, digs, -digs, '0');
    else
        Form_Int_Pad(buf, num, -digs, digs, '0');

    Append_Ascii(dst, s_cast(buf));
}



//
//  Append_UTF8_May_Fail: C
//
// Append UTF-8 data to a series underlying an ANY-STRING! (or create new one)
//
REBSTR *Append_UTF8_May_Fail(
    REBSTR *dst,  // if nullptr, that means make a new string
    const char *utf8,
    REBSIZ size,
    bool crlf_to_lf
){
    // This routine does not just append bytes blindly because:
    //
    // * If crlf_to_lf is set, then some characters might need to be removed
    // * We want to check for invalid byte sequences, as this can be called
    //   with arbitrary outside data from the API.
    // * It's needed to know how many characters (length) are in the series,
    //   not just how many bytes.  The higher level concept of "length" gets
    //   stored in the series MISC() field.
    // * In the future, some operations will be accelerated by knowing that
    //   a string only contains ASCII codepoints.

    const REBYTE *bp = cb_cast(utf8);

    DECLARE_MOLD (mo); // !!! REVIEW: don't need intermediate if no crlf_to_lf
    Push_Mold(mo);

    bool all_ascii = true;
    REBCNT num_codepoints = 0;

    REBSIZ bytes_left = size; // see remarks on Back_Scan_UTF8_Char's 3rd arg
    for (; bytes_left > 0; --bytes_left, ++bp) {
        REBUNI c = *bp;
        if (c >= 0x80) {
            bp = Back_Scan_UTF8_Char(&c, bp, &bytes_left);
            if (bp == NULL)
                fail (Error_Bad_Utf8_Raw()); // !!! Should Back_Scan() fail?

            all_ascii = false;
        }
        else if (c == CR && crlf_to_lf) {
            if (bp[1] == LF)
                continue; // skip the CR, do the decrement and get the LF
            c = LF;
        }

        ++num_codepoints;
        Append_Codepoint(mo->series, c);
    }

    UNUSED(all_ascii);

    // !!! The implicit nature of this is probably not the best way of
    // handling things, but... if the series we were supposed to be appending
    // to was the mold buffer, that's what we just did.  Consider making this
    // a specific call for Mold_Utf8() or similar.
    //
    if (dst == mo->series)
        return dst;

    if (not dst)
        return Pop_Molded_String(mo);

    REBCNT old_len = STR_LEN(dst);
    REBSIZ old_size = STR_SIZE(dst);

    EXPAND_SERIES_TAIL(SER(dst), size);
    memcpy(
        BIN_AT(SER(dst), old_size),
        BIN_AT(SER(mo->series), mo->offset),
        STR_SIZE(mo->series) - mo->offset
    );

    TERM_STR_LEN_SIZE(
        dst,
        old_len + num_codepoints,
        old_size + STR_SIZE(mo->series) - mo->offset
    );

    Drop_Mold(mo);

    return dst;
}


//
//  Join_Binary: C
//
// Join a binary from component values for use in standard
// actions like make, insert, or append.
// limit: maximum number of values to process
// limit < 0 means no limit
//
// WARNING: returns BYTE_BUF, not a copy!
//
REBBIN *Join_Binary(const REBVAL *blk, REBINT limit)
{
    REBSER *series = BYTE_BUF;

    REBCNT tail = 0;

    if (limit < 0)
        limit = VAL_LEN_AT(blk);

    SET_SERIES_LEN(series, 0);

    RELVAL *val;
    for (val = VAL_ARRAY_AT(blk); limit > 0; val++, limit--) {
        switch (VAL_TYPE(val)) {
        case REB_INTEGER:
            if (VAL_INT64(val) > 255 || VAL_INT64(val) < 0)
                fail (Error_Out_Of_Range(KNOWN(val)));
            EXPAND_SERIES_TAIL(series, 1);
            *BIN_AT(series, tail) = (REBYTE)VAL_INT32(val);
            break;

        case REB_BINARY: {
            REBCNT len = VAL_LEN_AT(val);
            EXPAND_SERIES_TAIL(series, len);
            memcpy(BIN_AT(series, tail), VAL_BIN_AT(val), len);
            break; }

        case REB_TEXT:
        case REB_FILE:
        case REB_EMAIL:
        case REB_URL:
        case REB_TAG: {
            REBCNT val_len;
            REBSIZ utf8_size = VAL_SIZE_LIMIT_AT(&val_len, val, UNKNOWN);

            REBSIZ offset = VAL_OFFSET_FOR_INDEX(val, VAL_INDEX(val));

            EXPAND_SERIES_TAIL(series, utf8_size);
            memcpy(
                BIN_AT(series, tail),
                BIN_AT(VAL_SERIES(val), offset),
                utf8_size
            );
            SET_SERIES_LEN(series, tail + utf8_size);
            break; }

        case REB_CHAR: {
            EXPAND_SERIES_TAIL(series, 6);
            REBCNT len =
                Encode_UTF8_Char(BIN_AT(series, tail), VAL_CHAR(val));
            SET_SERIES_LEN(series, tail + len);
            break; }

        default:
            fail (Error_Bad_Value_Core(val, VAL_SPECIFIER(blk)));
        }

        tail = SER_LEN(series);
    }

    *BIN_AT(series, tail) = 0;

    return series;  // SHARED FORM SERIES!
}
