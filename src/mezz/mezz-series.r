REBOL [
    System: "REBOL [R3] Language Interpreter and Run-time Environment"
    Title: "REBOL 3 Mezzanine: Series Helpers"
    Rights: {
        Copyright 2012 REBOL Technologies
        REBOL is a trademark of REBOL Technologies
    }
    License: {
        Licensed under the Apache License, Version 2.0
        See: http://www.apache.org/licenses/LICENSE-2.0
    }
]


; !!! Although this follows the -OF naming convention, it doesn't fit the
; pattern of a reflector as it takes two arguments.  Moreover, it is a bit
; sketchy...it doesn't check to see that the two series are the same, and
; if all it's doing is plain subtraction it seems like a poor primitive to
; be stuck with giving a name and suggested greater semantics to.  Review.
;
offset-of: func [
    "Returns the offset between two series positions."
    series1 [any-series!]
    series2 [any-series!]
][
    (index of series2) - (index of series1)
]


last?: single?: func [
    "Returns TRUE if the series length is 1."
    series [any-series! port! map! tuple! bitset! object! any-word!]
][
    1 = length of series
]


extend: func [
    "Extend an object, map, or block type with word and value pair."
    return: [<opt> any-value!]
    obj [object! map! block! group!] {object to extend (modified)}
    word [any-word!]
    val [<opt> any-value!]
][
    append obj reduce [to-set-word word :val]
    :val
]


join-all: function [
    "Reduces and appends a block of values together."
    return: [<opt> any-path! any-series! quoted!]
        "Will be the type of the first non-null series produced by evaluation"
    block [block!]
        "Values to join together"
    <local> position base
][
    until [
        block: (evaluate @base block) else [return null]
        set? 'base  ; skip NULL evaluations
    ]
    join base block
]


remold: redescribe [
    {Reduces and converts a value to a REBOL-readable string.}
](
    adapt 'mold [
        value: reduce :value
    ]
)

array: function [
    {Makes and initializes a block of a given size}

    size "Size or block of sizes for each dimension"
        [integer! block!]
    /initial "Initial value (will be called each time if a function)"
        [any-value!]  ; refinement, so will default to BLANK!
][
    if block? size [
        if tail? rest: next size [rest: _]
        if not integer? size: first size [
            fail @size ["Expected INTEGER! size in BLOCK!, not" type of :size]
        ]
    ]
    block: make block! size
    case [
        block? :rest [
            loop size [block: insert/only block array/initial rest :initial]
        ]
        any-series? :initial [
            loop size [block: insert/only block copy/deep initial]
        ]
        action? :initial [
            loop size [block: insert/only block initial]  ; Called every time
        ]
        default [
            insert/dup/only block :initial size
        ]
    ]
    head of block
]


replace: function [
    {Replaces a search value with the replace value within the target series}

    target "Series to replace within (modified)"
        [any-series!]
    pattern "Value to be replaced (converted if necessary)"
        [<opt> any-value!]
    replacement "Value to replace with (called each time if a function)"
        [<opt> any-value!]

    ; !!! Note these refinments alias ALL, CASE, TAIL natives!
    /all "Replace all occurrences"
    /case "Case-sensitive replacement"
    /tail "Return target after the last replacement position"

    ; Consider adding an /any refinement to use find/any, once that works.
][
    if not set? 'pattern [return target]

    all_REPLACE: all
    all: :lib/all
    case_REPLACE: case
    case: :lib/case
    tail_REPLACE: tail
    tail: :lib/tail

    save-target: target

    ; !!! These conversions being missing seems a problem with FIND the native
    ; as a holdover from pre-open-source Rebol when mezzanine development
    ; had no access to source (?).  Correct answer is likely to fix FIND:
    ;
    ;    >> find "abcdef" <cde>
    ;    >> == "cdef" ; should probably be null
    ;
    ;    >> find "ab<cde>f" <cde>
    ;    == "cde>f" ; should be "<cde>f"
    ;
    ; Note that if a FORM actually happens inside of FIND, it could wind up
    ; happening repeatedly in the /ALL case if that happens.

    len: 1 unless case [
        ; leave bitset patterns as-is regardless of target type, len = 1
        bitset? :pattern [1]

        any-string? target [
            if not text? :pattern [pattern: form :pattern]
            length of :pattern
        ]

        binary? target [
            ; Target is binary, pattern is not, make pattern a binary
            if not binary? :pattern [pattern: to-binary :pattern]
            length of :pattern
        ]

        any-array? :pattern [length of :pattern]
    ]

    while [pos: find/(try if case_REPLACE [/case]) target :pattern] [
        either action? :replacement [
            ;
            ; If arity-0 action, value gets replacement and pos discarded
            ; If arity-1 action, pos will be argument to replacement
            ; If arity > 1, end of block will cause an error
            ;
            value: replacement pos

            ; Note: ACTION! parameter couldn't be passed as enfix ("no such
            ; thing as enfix actions, just bindings").  So REPLACEMENT can't
            ; quote backwards and (for instance) fetch value...but it *could*
            ; quote pos and find out it's called `pos` (for instance).
        ][
            value: :replacement  ; inert value, might be null
        ]

        target: change/part pos :value len

        if not all_REPLACE [break]
    ]

    either tail_REPLACE [target] [save-target]
]


;
; reword "$1 is $2." [1 "This" 2 "that"] => "This is that."
;
reword: function [
    {Make a string or binary based on a template and substitution values}

    source "Template series with escape sequences"
        [any-string! binary!]
    values "Keyword literals and value expressions"
        [map! object! block!]
    /case "Characters are case-sensitive"
    /escape "Escape char(s) or [prefix suffix] delimiters (default is $)"
        [char! any-string! word! binary! block!]

    <static>

    ; Note: this list should be the same as above with delimiters, with
    ; BLOCK! excluded.
    ;
    delimiter-types (
        make typeset! [char! any-string! word! binary!]
    )
    keyword-types (
        make typeset! [char! any-string! integer! word! binary!]
    )
][
    case_REWORD: case
    case: :lib/case

    out: make (type of source) length of source

    prefix: _
    suffix: _
    case [
        blank? escape [prefix: "$"]  ; refinement not used, so use default

        any [
            escape = ""
            escape = []
        ][
            prefix: _  ; pure search and replace, no prefix/suffix
        ]

        block? escape [
            parse escape [
                set prefix delimiter-types
                set suffix opt delimiter-types
                end
            ] else [
                fail ["Invalid /ESCAPE delimiter block" escape]
            ]
        ]

        default [
            prefix: ensure delimiter-types escape
        ]
    ]

    ; To be used in a parse rule, words must be turned into strings, though
    ; it would be nice if they didn't have to be, e.g.
    ;
    ;     parse "abc" ['abc] => true
    ;
    ; Integers have to be converted also.
    ;
    if match [integer! word!] prefix [prefix: to-text prefix]
    if match [integer! word!] suffix [suffix: to-text suffix]

    ; MAKE MAP! will create a map with no duplicates from the input if it
    ; is a BLOCK! (though differing cases of the same key will be preserved).
    ; This might be better with stricter checking, in case later keys
    ; overwrite earlier ones and obscure the invalidity of the earlier keys
    ; (or perhaps MAKE MAP! itself should disallow duplicates)
    ;
    if block? values [
        values: make map! values
    ]

    ; We match strings generated from the keywords, but need to know what
    ; generated the strings to look them up in the map.  Hence we build a rule
    ; that will look something like:
    ;
    ; [
    ;     "keyword1" (keyword-match: lit keyword1)
    ;     | "keyword2" (keyword-match: lit keyword2)
    ;     | fail
    ; ]
    ;
    ; Note that the enclosing rule has to account for `prefix` and `suffix`,
    ; this just matches the keywords, setting `keyword-match` if one did.
    ;
    keyword-match: _  ; variable that gets set by rule
    any-keyword-rule: collect [
        for-each [keyword value] values [
            if not match keyword-types keyword [
                fail ["Invalid keyword type:" keyword]
            ]

            keep reduce [
                if match [integer! word!] keyword [
                    to-text keyword  ; `parse "a1" ['a '1]` illegal for now
                ] else [
                    keyword
                ]

                compose lit (keyword-match: lit (keyword))
            ]

            keep/line '|
        ]
        keep 'false  ; add failure if no match, instead of removing last |
    ]

    rule: [
        a:  ; Begin marking text to copy verbatim to output
        any [
            to prefix  ; seek to prefix (may be blank!, this could be a no-op)
            b:  ; End marking text to copy verbatim to output
            prefix  ; consume prefix (if no-op, may not be at start of match)
            [
                [
                    any-keyword-rule suffix (
                        append/part out a offset? a b  ; output before prefix

                        v: select/(case_REWORD) values keyword-match
                        append out switch type of :v [
                            action! [
                                ; Give v the option of taking an argument, but
                                ; if it does not, evaluate to arity-0 result.
                                ;
                                (result: v :keyword-match)
                                :result
                            ]
                            block! [do :v]
                            default [:v]
                        ]
                    )
                    a:  ; Restart mark of text to copy verbatim to output
                ]
                    |
                skip  ; if wasn't at match, keep the ANY rule scanning ahead
            ]
        ]
        to end  ; Seek to end, just so rule succeeds
        (append out a)  ; finalize output - transfer any remainder verbatim
    ]

    parse/(case_REWORD) source rule else [fail]  ; should succeed
    return out
]


move: func [
    {Move a value or span of values in a series}

    source "Source series (modified)"
        [any-series!]
    offset "Offset to move by, or index to move to"
        [integer!]
    /part "Move part of a series by length"
        [integer!]
    /skip "Treat the series as records of fixed size"
        [integer!]
    /to "Move to an index relative to the head of the series"
][
    part: default [1]
    if skip [
        if 1 > skip [cause-error 'script 'out-of-range skip]
        offset: either to [offset - 1 * skip + 1] [offset * skip]
        part: part * skip
    ]
    part: take/part source part
    insert either to [at head of source offset] [
        lib/skip source offset
    ] part
]


extract: function [
    {Extracts a value from a series at regular intervals}

    series [any-series!]
    width "Size of each entry (the skip)"
        [integer!]
    /index "Extract from offset position(s)"
        [any-number! logic! block!]
    /default "Default value to use (will be called each time if a function)"
        [any-value!]
][
    value: default  ; Default value is "" for any-string! output
    default: :lib/default

    if zero? width [return make (type of series) 0]  ; avoid an infinite loop
    len: either positive? width [  ; Length to preallocate
        divide (length of series) width  ; Forward loop, use length
    ][
        divide (index of series) negate width  ; Backward loop, use position
    ]
    index: default [1]
    if block? index [
        parse index [some [any-number! | logic!] end] else [
            cause-error 'Script 'invalid-arg reduce [index]
        ]
        out: make (type of series) len * length of index
        if (not :value) and [any-string? out] [value: copy ""]
        iterate-skip series width [iterate index [
            val: pick series index/1 else [value]
            append/only out :val
        ]]
    ] else [
        out: make (type of series) len
        if (not :value) and [any-string? out] [value: copy ""]
        iterate-skip series width [
            val: pick series index else [value]
            append/only out :val
        ]
    ]
    out
]


alter: func [
    "Append value if not found, else remove it; returns true if added."

    series [any-series! port! bitset!] {(modified)}
    value
    /case
        "Case-sensitive comparison"
][
    case_ALTER: case
    case: :lib/case

    if bitset? series [
        if find series :value [
            remove/part series :value
            return false
        ]
        append series :value
        return true
    ]
    if remove (find/(try if case_ALTER [/case]) series :value) [
        append series :value
        return true
    ]
    return false
]


collect*: func [
    {Evaluate body, and return block of values collected via keep function}

    return: "Result block, or null if no KEEPs (prevent nulls with KEEP [])"
        [<opt> block!]
    body "Block to evaluate"
        [<blank> block!]
][
    let out
    let keeper: specialize* (  ; SPECIALIZE to hide series argument
        enclose* 'append func* [  ; Derive from APPEND for /ONLY /LINE /DUP
            f [frame!]
            <with> out
        ][
            :f/value then [  ; null won't run block, nor "count" as collected
                f/series: out: default [make block! 16]  ; no null return now
                :f/value  ; ELIDE leaves as result (DO F invalidates F/VALUE)
                elide do f
            ]
            ; ^-- failed THEN returns NULL
        ]
    )[
        series: <replaced>
    ]

    ; use FUNC for binding work of connecting KEEP with the keeper function
    ;
    reeval func compose [keep [action!] <with> return] body :keeper

    :out
]


; Classic version of COLLECT which assumes that the word you want to use
; is KEEP, and that the body needs to be deep copied and rebound (via FUNC)
; to a new variable to hold the keeping function.  Returns an empty block
; if nothing is collected.
;
collect: redescribe [
    {Evaluate body, and return block of values collected via KEEP function.
    Returns empty block if nothing KEEPed.}
] chain [  ; Gives empty block instead of null if no keeps
    :collect*
        |
    specialize 'else [branch: [copy []]]
]

collect-lines: redescribe [
    {Evaluate body, and return block of values collected via KEEP function.
    KEEPed blocks become spaced TEXT!.}
] adapt 'collect [  ; https://forum.rebol.info/t/945/1
    body: compose [
        keep: adapt* specialize* 'keep [
            line: true | only: false | part: _
        ] [value: spaced try :value]
        (as group! body)
    ]
]

collect-text: redescribe [
    {Evaluate body, and return block of values collected via KEEP function.
    Returns all values as a single spaced TEXT!, individual KEEPed blocks get UNSPACED.}
] chain [  ; https://forum.rebol.info/t/945/2
    adapt 'collect [
        body: compose [
            keep: adapt* specialize* 'keep [
                line: false | only: false | part: _
            ][
                value: unspaced try :value
            ]
            (as group! body)
        ]
    ]
        |
    :spaced
        |
    specialize* 'else [branch: [copy ""]]
]

format: function [
    "Format a string according to the format dialect."
    rules {A block in the format dialect. E.g. [10 -10 #"-" 4]}
    values
    /pad
][
    pad: default [space]
    rules: blockify :rules
    values: blockify :values

    ; Compute size of output (for better mem usage):
    val: 0
    for-each rule rules [
        if word? :rule [rule: get rule]

        val: me + switch type of :rule [
            integer! [abs rule]
            text! [length of rule]
            char! [1]
            default [0]
        ]
    ]

    out: make text! val
    insert/dup out pad val

    ; Process each rule:
    for-each rule rules [
        if word? :rule [rule: get rule]

        switch type of :rule [
            integer! [
                pad: rule
                val: form first values
                values: my next
                clear at val 1 + abs rule
                if negative? rule [
                    pad: rule + length of val
                    if negative? pad [out: skip out negate pad]
                    pad: length of val
                ]
                change out :val
                out: skip out pad ; spacing (remainder)
            ]
            text! [out: change out rule]
            char! [out: change out rule]
        ]
    ]

    ; Provided enough rules? If not, append rest:
    if not tail? values [append out values]
    head of out
]


printf: func [
    "Formatted print."
    return: <void>
    fmt "Format"
    val "Value or block of values"
][
    print format :fmt :val
]


split: function [
    {Split series in pieces: fixed/variable size, fixed number, or delimited}

    return: [block!]
    series "The series to split"
        [any-series!]
    dlm "Split size, delimiter(s) (if all integer block), or block rule(s)"
        [block! integer! char! bitset! text! tag!]
    /into "If dlm is integer, split in n pieces (vs. pieces of length n)"
][
    parse (try match block! dlm) [some integer! end] then [
        return map-each len dlm [
            if len <= 0 [
                series: skip series negate len
                continue  ; don't add to output
            ]
            copy/part series series: skip series len
        ]
    ]

    if tag? dlm [dlm: form dlm]  ; reserve other strings for future meanings

    result: collect [
        parse series if integer? dlm [
            size: dlm  ; alias for readability in integer case
            if size < 1 [fail "Bad SPLIT size given:" size]

            if into [
                count: size - 1
                piece-size: to integer! round/down (length of series) / size
                if zero? piece-size [piece-size: 1]

                [
                    count [copy series piece-size skip (keep/only series)]
                    copy series to end (keep/only series)
                ]
            ] else [
                [any [copy series 1 size skip (keep/only series)] end]
            ]
        ] else [
            ; A block that is not all integers, e.g. not `[1 1 1]`, acts as a
            ; PARSE rule (see %split.test.reb)
            ;
            ensure [bitset! text! char! block!] dlm

            [
                any [mk1: while [mk2: [dlm | end] break | skip] (
                    keep/only copy/part mk1 mk2
                )]
                end
            ]
        ]
    ]

    ; Special processing, to handle cases where the spec'd more items in
    ; /into than the series contains (so we want to append empty items),
    ; or where the dlm was a char/string/charset and it was the last char
    ; (so we want to append an empty field that the above rule misses).
    ;
    fill-val: does [copy either any-array? series [[]] [""]]
    add-fill-val: does [append/only result fill-val]
    if integer? dlm [
        if into [
            ; If the result is too short, i.e., less items than 'size, add
            ; empty items to fill it to 'size.  Loop here instead of using
            ; INSERT/DUP, because that wouldn't copy the value inserted.
            ;
            if size > length of result [
                loop (size - length of result) [add-fill-val]
            ]
        ]
    ] else [
        ; If the last thing in the series is a delimiter, there is an
        ; implied empty field after it, which we add here.
        ;
        (switch type of dlm [
            bitset! [did find dlm try last series]
            char! [dlm = last series]
            text! [
                (did find series dlm) and [empty? find-last/tail series dlm]
            ]
            block! [false]
        ]) and [
            add-fill-val
        ]
    ]

    return result
]


find-all: function [
    "Find all occurrences of a value within a series (allows modification)."

    'series [word!]
        "Variable for block, string, or other series"
    value
    body [block!]
        "Evaluated for each occurrence"
][
    verify [any-series? orig: get series]
    while [any [
        | set series find get series :value
        | (set series orig | false)  ; reset series and break loop
    ]][
        do body
        series: next series
    ]
]
