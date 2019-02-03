//
//  File: %sys-frame.h
//  Summary: {Accessors and Argument Pushers/Poppers for Function Call Frames}
//  Project: "Rebol 3 Interpreter and Run-time (Ren-C branch)"
//  Homepage: https://github.com/metaeducation/ren-c/
//
//=////////////////////////////////////////////////////////////////////////=//
//
// Copyright 2012 REBOL Technologies
// Copyright 2012-2019 Rebol Open Source Contributors
// REBOL is a trademark of REBOL Technologies
//
// See README.md and CREDITS.md for more information
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
// A single FRAME! can go through multiple phases of evaluation, some of which
// should expose more fields than others.  For instance, when you specialize
// a function that has 10 parameters so it has only 8, then the specialization
// frame should not expose the 2 that have been removed.  It's as if the
// KEYS OF the spec is shorter than the actual length which is used.
//
// Hence, each independent value that holds a frame must remember the function
// whose "view" it represents.  This field is only applicable to frames, and
// so it could be used for something else on other types
//
// Note that the binding on a FRAME! can't be used for this purpose, because
// it's already used to hold the binding of the function it represents.  e.g.
// if you have a definitional return value with a binding, and try to
// MAKE FRAME! on it, the paramlist alone is not enough to remember which
// specific frame that function should exit.
//


//=//// SIMULATED "THROWN" TYPE ///////////////////////////////////////////=//
//
// All THROWN values have two parts: the REBVAL arg being thrown and
// a REBVAL indicating the /NAME of a labeled throw.  (If the throw was
// created with plain THROW instead of THROW/NAME then its name is blank).
//
// You cannot fit both values into a single value's bits of course.  One way
// to approach the problem would be to create a new REB_THROWN type with
// two fields (like a PAIR!).  But since there can only be one thrown value
// in the system at a time, a more efficient trick is used instead.  The
// throw label is kept in the output cell, with the arg put off to the side.
//
// There are important technical reasons for favoring the label in the output:
//
// * RETURN is implemented as a throw whose label is a FRAME!.  That FRAME!
//   value can store either a REBFRM* which costs nothing extra, or a REBCTX*
//   which requires "reifying" the frame and making it GC-visible.  Reifying
//   would happen unconditionally if the frame is put into a global variable,
//   but so long as the FRAME! value bubbles up no higher than the REBFRM*
//   it points to, it can be used as-is.  With RETURN, it will be exactly the
//   right lifetime--since the originating frame is right where it stops.
//
// * When various stack levels are checking for their interest in a thrown
//   value, they look at the label...and if it's not what they want, they
//   pass it on.  So the label is checked many times, while the arg is only
//   caught once at its final location.
//
// Avoiding a separate REB_THROWN datatype involves ensuring that the entire
// concept of "throw-ness" is threaded through the stack.  This is done with
// the R_THROWN dispatcher result or bool-returning `XXX_Throws()` functions.
// It creates some danger that a thrown value will be used accidentally as a
// "normal" value.  This is tested in the debug build by `SPORADICALLY()`
// putting an unreadable blank in the output slot and taking the reificaiton
// hit of putting the label off to the side.
//


// It is more pleasant to have a uniform way of speaking of frames by pointer,
// so this macro sets that up for you, the same way DECLARE_LOCAL does.  The
// optimizer should eliminate the extra pointer.
//
// Just to simplify matters, the frame cell is set to a bit pattern the GC
// will accept.  It would need stack preparation anyway, and this simplifies
// the invariant so if a recycle happens before Eval_Core_Throws() gets to its
// body, it's always set to something.  Using an unreadable blank means we
// signal to users of the frame that they can't be assured of any particular
// value between evaluations; it's not cleared.
//

inline static void Prep_Feed_Core(struct Reb_Feed *feed)
{
    Prep_Stack_Cell(&feed->fetched); \
    Init_Unreadable_Blank(&feed->fetched); \
    Prep_Stack_Cell(&feed->lookback); \
    Init_Unreadable_Blank(&feed->lookback); \
}

inline static void Prep_Frame_Core(
    REBFRM *f,
    struct Reb_Feed *feed_ptr
){
    f->feed = feed_ptr;
    Prep_Stack_Cell(&f->cell);
    Init_Unreadable_Blank(&f->cell);
    f->dsp_orig = DS_Index;
}

#define DECLARE_FRAME_CORE(name,feed_ptr) \
    REBFRM name##struct; \
    Prep_Frame_Core(&name##struct, (feed_ptr)); \
    REBFRM * const name = &name##struct

#define DECLARE_FRAME(name) \
    struct Reb_Feed name##feed; \
    Prep_Feed_Core(&name##feed); \
    DECLARE_FRAME_CORE(name, &name##feed)

#define DECLARE_END_FRAME(name) \
    DECLARE_FRAME_CORE(name, &TG_Frame_Feed_End)

#define DECLARE_SUBFRAME(name, parent) \
    DECLARE_FRAME_CORE(name, (parent)->feed)


#if !defined(NDEBUG)
    inline static bool Is_Evaluator_Throwing_Debug(void) {
        return NOT_END(&TG_Thrown_Arg);
    }
#endif

#if defined(NDEBUG)
    #define VAL_THROWN_LABEL(thrown) \
        (thrown)
#else
    inline static const REBVAL *VAL_THROWN_LABEL(const REBVAL *thrown) {
        if (IS_END(&TG_Thrown_Label_Debug))
            return thrown;
        assert(IS_UNREADABLE_DEBUG(thrown));
        return &TG_Thrown_Label_Debug;
    }
#endif

inline static REB_R Init_Thrown_With_Label(
    REBVAL *out,
    const REBVAL *arg,
    const REBVAL *label // Note: is allowed to be same as `out`
){
  #if defined(NDEBUG)
    if (out != label)
        Move_Value(out, label);
  #else
    assert(IS_END(&TG_Thrown_Arg));
    assert(IS_END(&TG_Thrown_Label_Debug));

    // Help avoid accidental uses of thrown output as misunderstood plain
    // outputs, by forcing thrown label access through VAL_THROWN_LABEL()...
    // but still test the release code path half the time.  (Causes different
    // reifications, but outside performance should still work the same.)
    //
    if (SPORADICALLY(2)) {
        Move_Value(&TG_Thrown_Label_Debug, label);
        Init_Unreadable_Blank(out);
    }
    else {
        if (out != label)
            Move_Value(out, label);
    }
  #endif

    Move_Value(&TG_Thrown_Arg, arg);
    return R_THROWN; // for chaining to dispatcher output
}

static inline void CATCH_THROWN(
    RELVAL *arg_out,
    REBVAL *thrown // Note: may be same pointer as arg_out
){
  #if !defined(NDEBUG)
    assert(NOT_END(&TG_Thrown_Arg));
  #endif

    UNUSED(thrown);
    Move_Value(arg_out, &TG_Thrown_Arg);

  #if !defined(NDEBUG)
    SET_END(&TG_Thrown_Arg);
    SET_END(&TG_Thrown_Label_Debug);
  #endif
}


//=////////////////////////////////////////////////////////////////////////=//
//
//  LOW-LEVEL FRAME ACCESSORS
//
//=////////////////////////////////////////////////////////////////////////=//


inline static bool FRM_IS_VALIST(REBFRM *f) {
    return f->feed->vaptr != nullptr;
}

inline static REBARR *FRM_ARRAY(REBFRM *f) {
    assert(IS_END(f->value) or not FRM_IS_VALIST(f));
    return f->feed->array;
}

// !!! Though the evaluator saves its `index`, the index is not meaningful
// in a valist.  Also, if `opt_head` values are used to prefetch before an
// array, those will be lost too.  A true debugging mode would need to
// convert these cases to ordinary arrays before running them, in order
// to accurately present any errors.
//
inline static REBCNT FRM_INDEX(REBFRM *f) {
    if (IS_END(f->value))
        return ARR_LEN(f->feed->array);

    assert(not FRM_IS_VALIST(f));
    return f->feed->index - 1;
}

inline static REBCNT FRM_EXPR_INDEX(REBFRM *f) {
    assert(not FRM_IS_VALIST(f));
    return f->expr_index == END_FLAG
        ? ARR_LEN((f)->feed->array)
        : f->expr_index - 1;
}

inline static REBSTR* FRM_FILE(REBFRM *f) { // https://trello.com/c/K3vntyPx
    if (not f->feed->array or NOT_ARRAY_FLAG(f->feed->array, HAS_FILE_LINE))
        return nullptr;
    return LINK(f->feed->array).file;
}

inline static const char* FRM_FILE_UTF8(REBFRM *f) {
    //
    // !!! Note: Too early in boot at the moment to use Canon(__ANONYMOUS__).
    //
    REBSTR *str = FRM_FILE(f);
    return str ? STR_HEAD(str) : "(anonymous)"; 
}

inline static int FRM_LINE(REBFRM *f) {
    if (not f->feed->array or NOT_ARRAY_FLAG(f->feed->array, HAS_FILE_LINE))
        return 0;
    return MISC(SER(f->feed->array)).line;
}

#define FRM_OUT(f) \
    cast(REBVAL * const, (f)->out) // writable rvalue


// Note about FRM_NUM_ARGS: A native should generally not detect the arity it
// was invoked with, (and it doesn't make sense as most implementations get
// the full list of arguments and refinements).  However, ACTION! dispatch
// has several different argument counts piping through a switch, and often
// "cheats" by using the arity instead of being conditional on which action
// ID ran.  Consider when reviewing the future of ACTION!.
//
#define FRM_NUM_ARGS(f) \
    (cast(REBSER*, (f)->varlist)->content.dynamic.len - 1) // minus rootvar

#define FRM_CELL(f) \
    cast(REBVAL*, &(f)->cell)

#define FRM_PRIOR(f) \
    ((f)->prior + 0) // prevent assignment via this macro

#define FRM_PHASE(f) \
    PAYLOAD(Context, (f)->rootvar).phase

#define FRM_BINDING(f) \
    EXTRA(Binding, (f)->rootvar).node

#define FRM_UNDERLYING(f) \
    ACT_UNDERLYING((f)->original)

#define FRM_DSP_ORIG(f) \
    ((f)->dsp_orig + 0) // prevent assignment via this macro


// ARGS is the parameters and refinements
// 1-based indexing into the arglist (0 slot is for FRAME! value)

#define FRM_ARGS_HEAD(f) \
    ((f)->rootvar + 1)

#ifdef NDEBUG
    #define FRM_ARG(f,n) \
        ((f)->rootvar + (n))
#else
    inline static REBVAL *FRM_ARG(REBFRM *f, REBCNT n) {
        assert(n != 0 and n <= FRM_NUM_ARGS(f));

        REBVAL *var = f->rootvar + n; // 1-indexed
        assert(not IS_RELATIVE(cast(RELVAL*, var)));
        return var;
    }
#endif


// Quick access functions from natives (or compatible functions that name a
// Reb_Frame pointer `frame_`) to get some of the common public fields.
//
#define D_FRAME     frame_
#define D_OUT       FRM_OUT(frame_)         // GC-safe slot for output value
#define D_ARGC      FRM_NUM_ARGS(frame_)    // count of args+refinements/args
#define D_ARG(n)    FRM_ARG(frame_, (n))    // pass 1 for first arg
#define D_CELL      FRM_CELL(frame_)        // scratch GC-safe cell

#define RETURN(v) \
    return Move_Value(D_OUT, (v));

inline static bool Is_Action_Frame(REBFRM *f) {
    if (f->original != nullptr) {
        //
        // Do not count as a function frame unless its gotten to the point
        // of pushing arguments.
        //
        return true;
    }
    return false;
}

// While a function frame is fulfilling its arguments, the `f->param` will
// be pointing to a typeset.  The invariant that is maintained is that
// `f->param` will *not* be a typeset when the function is actually in the
// process of running.  (So no need to set/clear/test another "mode".)
//
inline static bool Is_Action_Frame_Fulfilling(REBFRM *f)
{
    assert(Is_Action_Frame(f));
    return NOT_END(f->param);
}


inline static void Get_Frame_Label_Or_Blank(RELVAL *out, REBFRM *f) {
    assert(Is_Action_Frame(f));
    if (f->opt_label != NULL)
        Init_Word(out, f->opt_label); // invoked via WORD! or PATH!
    else
        Init_Blank(out); // anonymous invocation
}

inline static const char* Frame_Label_Or_Anonymous_UTF8(REBFRM *f) {
    assert(Is_Action_Frame(f));
    if (f->opt_label != NULL)
        return STR_HEAD(f->opt_label);
    return "[anonymous]";
}

inline static void SET_FRAME_VALUE(REBFRM *f, const RELVAL* value) {
    assert(not f->gotten); // is fetched f->value, we'd be invalidating it!
    f->value = value;
}



//=////////////////////////////////////////////////////////////////////////=//
//
//  ARGUMENT AND PARAMETER ACCESS HELPERS
//
//=////////////////////////////////////////////////////////////////////////=//
//
// These accessors are what is behind the INCLUDE_PARAMS_OF_XXX macros that
// are used in natives.  They capture the implicit Reb_Frame* passed to every
// REBNATIVE ('frame_') and read the information out cleanly, like this:
//
//     PARAM(1, foo);
//     REFINE(2, bar);
//
//     if (IS_INTEGER(ARG(foo)) and REF(bar)) { ... }
//
// Though REF can only be used with a REFINE() declaration, ARG can be used
// with either.  By contract, Rebol functions are allowed to mutate their
// arguments and refinements just as if they were locals...guaranteeing only
// their return result as externally visible.  Hence the ARG() cell for a
// refinement provides a GC-safe slot for natives to hold values once they
// have observed what they need from the refinement.
//
// Under the hood `PARAM(1, foo)` and `REFINE(2, bar)` are const values in
// the release build.  Under optimization they disappear completely, so that
// addressing is done directly into the call frame's cached `arg` pointer.
// It is also possible to get the typeset-with-symbol for a particular
// parameter or refinement, e.g. with `PAR(foo)` or `PAR(bar)`.
//
// The PARAM and REFINE macros use token pasting to name the variables they
// are declaring `p_name` instead of just `name`.  This prevents collisions
// with C/C++ identifiers, so PARAM(case) and REFINE(new) would make `p_case`
// and `p_new` instead of just `case` and `new` as the variable names.  (This
// is only visible in the debugger.)
//
// As a further aid, the debug build version of the structures contain the
// actual pointers to the arguments.  It also keeps a copy of a cache of the
// type for the arguments, because the numeric type encoding in the bits of
// the header requires a debug call (or by-hand-binary decoding) to interpret
// Whether a refinement was used or not at time of call is also cached.
//

#ifdef NDEBUG
    #define PARAM(n,name) \
        static const int p_##name = n

    #define REFINE(n,name) \
        static const int p_##name = n

    #define ARG(name) \
        FRM_ARG(frame_, (p_##name))

    #define PAR(name) \
        ACT_PARAM(FRM_PHASE(frame_), (p_##name)) /* a TYPESET! */

    #define REF(name) \
        (not IS_BLANK(ARG(name))) /* should be faster than IS_FALSEY() */
#else
    struct Native_Param {
        int num;
        enum Reb_Kind kind_cache; // for inspecting in watchlist
        REBVAL *arg; // for inspecting in watchlist
    };

    struct Native_Refine {
        int num;
        bool used_cache; // for inspecting in watchlist
        REBVAL *arg; // for inspecting in watchlist
    };

    // Note: Assigning non-const initializers to structs, e.g. `= {var, f()};`
    // is a non-standard extension to C.  So we break out the assignments.

    #define PARAM(n,name) \
        struct Native_Param p_##name; \
        p_##name.num = (n); \
        p_##name.kind_cache = VAL_TYPE(FRM_ARG(frame_, (n))); \
        p_##name.arg = FRM_ARG(frame_, (n)); \

    #define REFINE(n,name) \
        struct Native_Refine p_##name; \
        p_##name.num = (n); \
        p_##name.used_cache = IS_TRUTHY(FRM_ARG(frame_, (n))); \
        p_##name.arg = FRM_ARG(frame_, (n)); \

    #define ARG(name) \
        FRM_ARG(frame_, (p_##name).num)

    #define PAR(name) \
        ACT_PARAM(FRM_PHASE(frame_), (p_##name).num) /* a TYPESET! */

    #define REF(name) \
        ((p_##name).used_cache /* used_cache use stops REF() on PARAM()s */ \
            ? not IS_BLANK(ARG(name)) \
            : not IS_BLANK(ARG(name)))
#endif


// The native entry prelude makes sure that once native code starts running,
// then the frame's stub is flagged to indicate access via a FRAME! should
// not have write access to variables.  That could cause crashes, as raw C
// code is not insulated against having bit patterns for types in cells that
// aren't expected.
//
// !!! Debug injection of bad types into usermode code may cause havoc as
// well, and should be considered a security/permissions issue.  It just won't
// (or shouldn't) crash the evaluator itself.
//
// This is automatically injected by the INCLUDE_PARAMS_OF_XXX macros.  The
// reason this is done with code inlined into the native itself instead of
// based on an IS_NATIVE() test is to avoid the cost of the testing--which
// is itself a bit dodgy to tell a priori if a dispatcher is native or not.
// This way there is no test and only natives pay the cost of flag setting.
//
inline static void Enter_Native(REBFRM *f) {
    SET_SERIES_INFO(f->varlist, HOLD); // may or may not be managed
}


inline static void Begin_Action(REBFRM *f, REBSTR *opt_label)
{
    assert(NOT_EVAL_FLAG(f, RUNNING_ENFIX));
    assert(NOT_FEED_FLAG(f->feed, DEFERRING_ENFIX));

    assert(not f->original);
    f->original = FRM_PHASE(f);

    assert(IS_POINTER_TRASH_DEBUG(f->opt_label)); // only valid w/REB_ACTION
    assert(not opt_label or GET_SERIES_FLAG(opt_label, IS_UTF8_STRING));
    f->opt_label = opt_label;
  #if defined(DEBUG_FRAME_LABELS) // helpful for looking in the debugger
    f->label_utf8 = cast(const char*, Frame_Label_Or_Anonymous_UTF8(f));
  #endif

    f->refine = ORDINARY_ARG;

    assert(NOT_EVAL_FLAG(f, REQUOTE_NULL));
    f->requotes = 0;
}


// Allocate the series of REBVALs inspected by a function when executed (the
// values behind ARG(name), REF(name), D_ARG(3),  etc.)
//
// This only allocates space for the arguments, it does not initialize.
// Eval_Core initializes as it goes, and updates f->param so the GC knows how
// far it has gotten so as not to see garbage.  APPLY has different handling
// when it has to build the frame for the user to write to before running;
// so Eval_Core only checks the arguments, and does not fulfill them.
//
// If the function is a specialization, then the parameter list of that
// specialization will have *fewer* parameters than the full function would.
// For this reason we push the arguments for the "underlying" function.
// Yet if there are specialized values, they must be filled in from the
// exemplar frame.
//
// Rather than "dig" through layers of functions to find the underlying
// function or the specialization's exemplar frame, those properties are
// cached during the creation process.
//
inline static void Push_Action(
    REBFRM *f,
    REBACT *act,
    REBNOD *binding
){
    assert(NOT_EVAL_FLAG(f, FULFILL_ONLY));
    assert(NOT_EVAL_FLAG(f, RUNNING_ENFIX));

    f->param = ACT_PARAMS_HEAD(act); // Specializations hide some params...
    REBCNT num_args = ACT_NUM_PARAMS(act); // ...so see REB_TS_HIDDEN

    // !!! Note: Should pick "smart" size when allocating varlist storage due
    // to potential reuse--but use exact size for *this* action, for now.
    //
    REBSER *s;
    if (not f->varlist) { // usually means first action call in the REBFRM
        s = Alloc_Series_Node(
            SERIES_MASK_CONTEXT
                | SERIES_FLAG_STACK_LIFETIME
                | SERIES_FLAG_FIXED_SIZE // FRAME!s don't expand ATM
        );
        s->info = Endlike_Header(
            FLAG_WIDE_BYTE_OR_0(0) // signals array, also implicit terminator
                | FLAG_LEN_BYTE_OR_255(255) // signals dynamic
        );
        s->link_private.keysource = NOD(f); // maps varlist back to f
        s->misc_private.meta = nullptr; // GC will sees this
        f->varlist = ARR(s);
    }
    else {
        s = SER(f->varlist);
        if (s->content.dynamic.rest >= num_args + 1 + 1) // +roovar, +end
            goto sufficient_allocation;

        //assert(SER_BIAS(s) == 0);
        Free_Unbiased_Series_Data(
            s->content.dynamic.data,
            SER_TOTAL(s)
        );
    }

    if (not Did_Series_Data_Alloc(s, num_args + 1 + 1)) // +rootvar, +end
        fail ("Out of memory in Push_Action()");

    f->rootvar = cast(REBVAL*, s->content.dynamic.data);
    f->rootvar->header.bits =
        NODE_FLAG_NODE | NODE_FLAG_CELL | NODE_FLAG_STACK
        | CELL_FLAG_PROTECTED // cell payload/binding tweaked, not by user
        | FLAG_KIND_BYTE(REB_FRAME);
    TRACK_CELL_IF_DEBUG(f->rootvar, __FILE__, __LINE__);
    PAYLOAD(Context, f->rootvar).varlist = f->varlist;

  sufficient_allocation:

    PAYLOAD(Context, f->rootvar).phase = act; // FRM_PHASE() (can be dummy)
    EXTRA(Binding, f->rootvar).node = binding; // FRM_BINDING()

    s->content.dynamic.len = num_args + 1;
    RELVAL *tail = ARR_TAIL(f->varlist);
    tail->header.bits = NODE_FLAG_STACK | FLAG_KIND_BYTE(REB_0);
    TRACK_CELL_IF_DEBUG(tail, __FILE__, __LINE__);

    // Current invariant for all arrays (including fixed size), last cell in
    // the allocation is an end.
    RELVAL *ultimate = ARR_AT(f->varlist, s->content.dynamic.rest - 1);
    ultimate->header = Endlike_Header(0); // unreadable
    TRACK_CELL_IF_DEBUG(ultimate, __FILE__, __LINE__);

  #if !defined(NDEBUG)
    RELVAL *prep = ultimate - 1;
    for (; prep > tail; --prep) {
        prep->header.bits = FLAG_KIND_BYTE(REB_T_TRASH); // unreadable
        TRACK_CELL_IF_DEBUG(prep, __FILE__, __LINE__);
    }
  #endif

    f->arg = f->rootvar + 1;

    // Each layer of specialization of a function can only add specializations
    // of arguments which have not been specialized already.  For efficiency,
    // the act of specialization merges all the underlying layers of
    // specialization together.  This means only the outermost specialization
    // is needed to fill the specialized slots contributed by later phases.
    //
    // f->special here will either equal f->param (to indicate normal argument
    // fulfillment) or the head of the "exemplar".  To speed this up, the
    // absence of a cached exemplar just means that the "specialty" holds the
    // paramlist... this means no conditional code is needed here.
    //
    f->special = ACT_SPECIALTY_HEAD(act);

    assert(NOT_SERIES_FLAG(f->varlist, MANAGED));
    assert(NOT_SERIES_INFO(f->varlist, INACCESSIBLE));

    // There's a current state for the FEED_FLAG_NO_LOOKAHEAD which invisible
    // actions want to put back as it was when the invisible operation ends.
    // (It gets overwritten during the invisible's own argument gathering).
    // Cache it on the varlist and put it back when an R_INVISIBLE result
    // comes back.
    //
    // !!! Should this go in Begin_Action?
    //
    if (GET_ACTION_FLAG(act, IS_INVISIBLE)) {
        if (GET_FEED_FLAG(f->feed, NO_LOOKAHEAD)) {
            assert(GET_EVAL_FLAG(f, FULFILLING_ARG));
            CLEAR_FEED_FLAG(f->feed, NO_LOOKAHEAD);
            SET_SERIES_INFO(f->varlist, TELEGRAPH_NO_LOOKAHEAD);
        }
    }
}


inline static void Drop_Action(REBFRM *f) {
    assert(NOT_SERIES_FLAG(f->varlist, VARLIST_FRAME_FAILED));

    assert(
        not f->opt_label
        or GET_SERIES_FLAG(f->opt_label, IS_UTF8_STRING)
    );

    if (NOT_EVAL_FLAG(f, FULFILLING_ARG))
        CLEAR_FEED_FLAG(f->feed, BARRIER_HIT);

    CLEAR_EVAL_FLAG(f, RUNNING_ENFIX);
    CLEAR_EVAL_FLAG(f, FULFILL_ONLY);
    CLEAR_EVAL_FLAG(f, REQUOTE_NULL);

    assert(
        GET_SERIES_INFO(f->varlist, INACCESSIBLE)
        or LINK(f->varlist).keysource == NOD(f)
    );

    if (GET_SERIES_INFO(f->varlist, INACCESSIBLE)) {
        //
        // If something like Encloser_Dispatcher() runs, it might steal the
        // variables from a context to give them to the user, leaving behind
        // a non-dynamic node.  Pretty much all the bits in the node are
        // therefore useless.  It served a purpose by being non-null during
        // the call, however, up to this moment.
        //
        if (GET_SERIES_FLAG(f->varlist, MANAGED))
            f->varlist = nullptr; // references exist, let a new one alloc
        else {
            // This node could be reused vs. calling Make_Node() on the next
            // action invocation...but easier for the moment to let it go.
            //
            Free_Node(SER_POOL, f->varlist);
            f->varlist = nullptr;
        }
    }
    else if (GET_SERIES_FLAG(f->varlist, MANAGED)) {
        //
        // The varlist wound up getting referenced in a cell that will outlive
        // this Drop_Action().  The pointer needed to stay working up until
        // now, but the args memory won't be available.  But since we know
        // there were outstanding references to the varlist, we need to
        // convert it into a "stub" that's enough to avoid crashes.
        //
        // ...but we don't free the memory for the args, we just hide it from
        // the stub and get it ready for potential reuse by the next action
        // call.  That's done by making an adjusted copy of the stub, which
        // steals its dynamic memory (by setting the stub not HAS_DYNAMIC).
        //
        f->varlist = CTX_VARLIST(
            Steal_Context_Vars(
                CTX(f->varlist),
                NOD(f->original) // degrade keysource from f
            )
        );
        assert(NOT_SERIES_FLAG(f->varlist, MANAGED));
        LINK(f->varlist).keysource = NOD(f);
    }
    else {
        // We can reuse the varlist and its data allocation, which may be
        // big enough for ensuing calls.  
        //
        // But no series bits we didn't set should be set...and right now,
        // only Enter_Native() sets HOLD.  Clear that.  Also, it's possible
        // for a "telegraphed" no lookahead bit used by an invisible to be
        // left on, so clear it too.
        //
        CLEAR_SERIES_INFO(f->varlist, HOLD);
        CLEAR_SERIES_INFO(f->varlist, TELEGRAPH_NO_LOOKAHEAD);

        assert(0 == (SER(f->varlist)->info.bits & ~( // <- note bitwise not
            SERIES_INFO_0_IS_TRUE // parallels NODE_FLAG_NODE
            | FLAG_WIDE_BYTE_OR_0(0) // don't mask out wide (0 for arrays))
            | FLAG_LEN_BYTE_OR_255(255) // mask out non-dynamic-len (dynamic)
        )));
    }

  #if !defined(NDEBUG)
    if (f->varlist) {
        assert(NOT_SERIES_INFO(f->varlist, INACCESSIBLE));
        assert(NOT_SERIES_FLAG(f->varlist, MANAGED));

        REBVAL *rootvar = cast(REBVAL*, ARR_HEAD(f->varlist));
        assert(IS_FRAME(rootvar));
        assert(PAYLOAD(Context, rootvar).varlist == f->varlist);
        TRASH_POINTER_IF_DEBUG(PAYLOAD(Context, rootvar).phase);
        TRASH_POINTER_IF_DEBUG(EXTRA(Binding, rootvar).node);
    }
  #endif

    f->original = nullptr; // signal an action is no longer running

    TRASH_POINTER_IF_DEBUG(f->opt_label);
  #if defined(DEBUG_FRAME_LABELS)
    TRASH_POINTER_IF_DEBUG(f->label_utf8);
  #endif
}


//
//  Context_For_Frame_May_Manage: C
//
inline static REBCTX *Context_For_Frame_May_Manage(REBFRM *f)
{
    assert(not Is_Action_Frame_Fulfilling(f));
    SET_SERIES_FLAG(f->varlist, MANAGED);
    return CTX(f->varlist);
}


inline static REBACT *VAL_PHASE(REBVAL *frame) {
    assert(IS_FRAME(frame));
    return PAYLOAD(Context, frame).phase;
}
