//
//  File: %host-args.c
//  Summary: "Command line argument processing"
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2016 Rebol Open Source Contributors
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
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
//=////////////////////////////////////////////////////////////////////////=//
//
// OS independent
//
// Parses command line arguments and options, storing them
// in a structure to be used by the REBOL library.
//
//

#include <stdlib.h>
#include <string.h>

// REBCHR is defined differently in the host code from in the core code.  This
// makes it easy for the host to speak to its native character type (wchar_t
// on Windows, char elsewhere) and hard for the core, because the REBCHR is
// a struct--on purpose, to prevent code from assuming a character size.
//
// Since %host-main.c is where Parse_Args is called, and it uses %sys-core.h,
// we have to use sys-core here to make sure that Parse_Args() uses a
// compatible definition of REBCHR.  (The C++ build cares about the type
// difference in the linking.)
//
#include "sys-core.h"

// REBOL Option --Words:

const struct {const REBCHR *word; const int flag;} arg_words[] = {
    // Keep in Alpha order!
    {OS_STR_LIT("boot"),        RO_BOOT | RO_EXT},
    {OS_STR_LIT("cgi"),         RO_CGI | RO_QUIET},
    {OS_STR_LIT("debug"),       RO_DEBUG | RO_EXT},
    {OS_STR_LIT("do"),          RO_DO | RO_EXT},
    {OS_STR_LIT("halt"),        RO_HALT},
    {OS_STR_LIT("help"),        RO_HELP},
    {OS_STR_LIT("import"),      RO_IMPORT | RO_EXT},
    {OS_STR_LIT("quiet"),       RO_QUIET},
    {OS_STR_LIT("secure"),      RO_SECURE | RO_EXT},
    {OS_STR_LIT("trace"),       RO_TRACE},
    {OS_STR_LIT("verbose"),     RO_VERBOSE},
    {OS_STR_LIT("version"),     RO_VERSION | RO_EXT},
    {OS_STR_LIT(""),            0},
};

// REBOL Option -Characters (in alpha sorted order):

const struct arg_chr {const char cflg; const int flag;} arg_chars[] = {
    {'?',   RO_HELP},
    {'V',   RO_VERS},
    {'b',   RO_BOOT | RO_EXT},
    {'c',   RO_CGI | RO_QUIET},
    {'h',   RO_HALT},
    {'q',   RO_QUIET},
    {'s',   RO_SECURE_MIN},
    {'t',   RO_TRACE},
    {'v',   RO_VERS},
    {'w',   RO_NO_WINDOW},
    {'\0',  0},
};

// REBOL Option +Characters:

const struct arg_chr arg_chars2[] = {
    {'s',   RO_SECURE_MAX},
    {'\0',  0},
};


//
//  find_option_word: C
//
// Scan options, return flag bits, else zero.
//
static int find_option_word(REBCHR *word)
{
    int n;
    int i;
    REBCHR buf[16];

    // Some shells will pass us the line terminator. Ignore it.
    if (
        OS_CH_EQUAL(word[0], '\r')
        || OS_CH_EQUAL(word[0], '\n')
    ) {
        return RO_IGNORE;
    }

    OS_STRNCPY(buf, word, 15);

    for (i = 0; arg_words[i].flag; i++) {
        n = OS_STRNCMP(buf, arg_words[i].word, 15);
        if (n < 0) break;
        if (n == 0) return arg_words[i].flag;
    }
    return 0;
}


//
//  find_option_char: C
//
// Scan option char flags, return flag bits, else zero.
//
static int find_option_char(REBCHR chr, const struct arg_chr list[])
{
    int i;

    // Some shells will pass us the line terminator. Ignore it.
    if (OS_CH_EQUAL(chr, '\r') || OS_CH_EQUAL(chr, '\n'))
        return RO_IGNORE;

    for (i = 0; list[i].flag; i++) {
        if (OS_CH_VALUE(chr) < list[i].cflg) break;
        if (OS_CH_VALUE(chr) == list[i].cflg) return list[i].flag;
    }
    return 0;
}


//
//  Get_Ext_Arg: C
//
// Get extended argument field.
//
static int Get_Ext_Arg(int flag, REBARGS *rargs, REBCHR *arg)
{
    flag &= ~RO_EXT;

    switch (flag) {

    case RO_VERSION:
        rargs->version = arg;
        break;

    case RO_DO:
        rargs->do_arg = arg;
        break;

    case RO_DEBUG:
        rargs->debug = arg;
        break;

    case RO_SECURE:
        rargs->secure = arg;
        break;

    case RO_IMPORT:
        rargs->import = arg;
        break;

    case RO_BOOT:
        rargs->boot = arg;
        break;
    }

    return flag;
}


//
//  Parse_Args: C
//
// Parse REBOL's command line arguments, setting options
// and values in the provided args structure.
//
void Parse_Args(int argc, REBCHR **argv, REBARGS *rargs)
{
    REBCHR *arg;
    int flag;
    int i;

    CLEARS(rargs);

    // First arg is path to execuable (on most systems):
    if (argc > 0) rargs->exe_path = *argv;

    OS_GET_CURRENT_DIR(&rargs->home_dir);

    // Parse each argument:
    for (i = 1; i < argc ; i++) {
        arg = argv[i];
        if (arg == NULL) continue; // shell bug
        if (OS_CH_EQUAL(*arg, '-')) {
            if (OS_CH_EQUAL(arg[1], '-')) {
                if (OS_CH_EQUAL(arg[2], 0)) {
                    // -- (end of options)
                    ++i;
                    break;
                }
                // --option words
                flag = find_option_word(arg+2);
                if (!flag) goto error;
                if (flag & RO_EXT) {
                    if (++i < argc) flag = Get_Ext_Arg(flag, rargs, argv[i]);
                    else goto error;
                }
                rargs->options |= flag;
            }
            else {
                // -x option chars
                while (OS_CH_VALUE(*++arg) != '\0') {
                    flag = find_option_char(*arg, arg_chars);
                    if (!flag) goto error;
                    if (flag & RO_EXT) {
                        if (++i < argc) flag = Get_Ext_Arg(flag, rargs, argv[i]);
                        else goto error;
                    }
                    rargs->options |= flag;
                }
            }
        }
        else if (OS_CH_EQUAL(*arg, '+')) {
            // +x option chars
            while (OS_CH_VALUE(*++arg) != '\0') {
                flag = find_option_char(*arg, arg_chars2);
                if (!flag) goto error;
                if (flag & RO_EXT) {
                    if (++i < argc) flag = Get_Ext_Arg(flag, rargs, argv[i]);
                    else goto error;
                }
                rargs->options |= flag;
            }
        }
        else break;
    }

    // script filename
    if (i < argc) rargs->script = argv[i++];

    // the rest are script args
    if (i < argc) {
        // rargs->args must be a null-terminated array of pointers
        // but CommandLineToArgvW() may return a non-terminated array
        rargs->args = OS_ALLOC_N(REBCHR*, argc - i + 1);
        memcpy(rargs->args, &argv[i], (argc - i) * sizeof(REBCHR*));
        rargs->args[argc - i] = NULL;
    }

    // empty script name for only setting args
    if (rargs->script && OS_CH_VALUE(rargs->script[0]) == '\0')
        rargs->script = NULL;

    return;

error:
    // disregard command line options
    // leave exe_path and home_dir set
    rargs->options = RO_HELP;
    rargs->version = NULL;
    rargs->do_arg = NULL;
    rargs->debug = NULL;
    rargs->secure = NULL;
    rargs->import = NULL;
    rargs->boot = NULL;
}
