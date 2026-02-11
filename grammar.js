/**
 * tree-sitter grammar for ABNF (Augmented Backus-Naur Form)
 *
 * Based on:
 *   - RFC 5234: Augmented BNF for Syntax Specifications: ABNF
 *     https://www.rfc-editor.org/rfc/rfc5234
 *   - RFC 7405: Case-Sensitive String Support in ABNF
 *     https://www.rfc-editor.org/rfc/rfc7405
 *
 * The grammar below is a faithful translation of the ABNF self-definition
 * given in RFC 5234 §4, with the `char-val` rule updated per RFC 7405 §2.2.
 *
 * ABNF Definition of ABNF (RFC 5234 §4):
 * ───────────────────────────────────────
 *   rulelist       =  1*( rule / (*c-wsp c-nl) )
 *   rule           =  rulename defined-as elements c-nl
 *   rulename       =  ALPHA *(ALPHA / DIGIT / "-")
 *   defined-as     =  *c-wsp ("=" / "=/") *c-wsp
 *   elements       =  alternation *c-wsp
 *   c-wsp          =  WSP / (c-nl WSP)
 *   c-nl           =  comment / CRLF
 *   comment        =  ";" *(WSP / VCHAR) CRLF
 *   alternation    =  concatenation *(*c-wsp "/" *c-wsp concatenation)
 *   concatenation  =  repetition *(1*c-wsp repetition)
 *   repetition     =  [repeat] element
 *   repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)
 *   element        =  rulename / group / option /
 *                      char-val / num-val / prose-val
 *   group          =  "(" *c-wsp alternation *c-wsp ")"
 *   option         =  "[" *c-wsp alternation *c-wsp "]"
 *   num-val        =  "%" (bin-val / dec-val / hex-val)
 *   bin-val        =  "b" 1*BIT [ 1*("." 1*BIT) / ("-" 1*BIT) ]
 *   dec-val        =  "d" 1*DIGIT [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
 *   hex-val        =  "x" 1*HEXDIG [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
 *   prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
 *
 * Updated char-val (RFC 7405 §2.2):
 * ─────────────────────────────────
 *   char-val                =  case-insensitive-string / case-sensitive-string
 *   case-insensitive-string =  [ "%i" ] quoted-string
 *   case-sensitive-string   =  "%s" quoted-string
 *   quoted-string           =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
 *
 * @file Abnf grammar for tree-sitter
 * @author saltfishpr
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

export default grammar({
  name: "abnf",

  // ──────────────────────────────────────────────────────────────
  // ABNF is whitespace-sensitive.  Whitespace is handled explicitly
  // via c-wsp / c-nl, so we must disable tree-sitter's automatic
  // whitespace skipping.
  // ──────────────────────────────────────────────────────────────
  extras: ($) => [],

  // ──────────────────────────────────────────────────────────────
  // Conflict resolution.
  //
  // `concatenation` and `alternation` both need GLR-style
  // disambiguation because c-wsp can appear both as a separator
  // in concatenation and as trailing whitespace in elements /
  // alternation alternatives.
  // ──────────────────────────────────────────────────────────────
  conflicts: ($) => [[$.concatenation], [$.alternation]],

  rules: {
    // ════════════════════════════════════════════════════════════
    //  Top-level
    // ════════════════════════════════════════════════════════════

    /**
     * rulelist = 1*( rule / (*c-wsp c-nl) )
     *
     * An ABNF file is one or more rule definitions, optionally
     * interspersed with blank / comment-only lines.
     */
    source_file: ($) =>
      repeat1(
        choice(
          $.rule,
          // blank / comment-only line
          prec(1, seq(repeat($._c_wsp), $._c_nl)),
        ),
      ),

    // ════════════════════════════════════════════════════════════
    //  Rule structure
    // ════════════════════════════════════════════════════════════

    /**
     * rule = rulename defined-as elements c-nl
     *
     * "continues if next line starts with white space"
     * (continuation handled via c-wsp → c-nl WSP)
     */
    rule: ($) => seq($.rulename, $.defined_as, $.elements, $._c_nl),

    /**
     * rulename = ALPHA *(ALPHA / DIGIT / "-")
     *
     * Rule names are case-insensitive sequences starting with a
     * letter, followed by letters, digits, or hyphens.
     */
    rulename: ($) => /[A-Za-z][A-Za-z0-9\-]*/,

    /**
     * defined-as = *c-wsp ("=" / "=/") *c-wsp
     *
     * "=" for basic definition, "=/" for incremental alternatives.
     */
    defined_as: ($) =>
      seq(repeat($._c_wsp), choice("=/", "="), repeat($._c_wsp)),

    /**
     * elements = alternation *c-wsp
     */
    elements: ($) => prec.left(1, seq($.alternation, repeat($._c_wsp))),

    // ════════════════════════════════════════════════════════════
    //  Whitespace & comments
    // ════════════════════════════════════════════════════════════

    /**
     * c-wsp = WSP / (c-nl WSP)
     *
     * "Conditional whitespace" — either plain whitespace or a
     * newline (possibly with a comment) followed by whitespace,
     * which indicates a continuation line.
     */
    _c_wsp: ($) => choice($._WSP, seq($._c_nl, $._WSP)),

    /**
     * c-nl = comment / CRLF
     */
    _c_nl: ($) => choice($.comment, $._CRLF),

    /**
     * comment = ";" *(WSP / VCHAR) CRLF
     *
     * We use a single token regex for performance.
     * Matches: semicolon, any printable chars + tabs/spaces, then
     * the line ending.  We accept both CRLF and bare LF for
     * practical use.
     */
    comment: ($) => token(seq(";", /[\x20\x09\x21-\x7E]*/, /\r?\n/)),

    // ════════════════════════════════════════════════════════════
    //  Operators (from highest to lowest precedence)
    // ════════════════════════════════════════════════════════════

    /**
     * alternation = concatenation *(*c-wsp "/" *c-wsp concatenation)
     */
    alternation: ($) =>
      seq(
        $.concatenation,
        repeat(seq(repeat($._c_wsp), "/", repeat($._c_wsp), $.concatenation)),
      ),

    /**
     * concatenation = repetition *(1*c-wsp repetition)
     */
    concatenation: ($) =>
      seq($.repetition, repeat(seq(repeat1($._c_wsp), $.repetition))),

    /**
     * repetition = [repeat] element
     */
    repetition: ($) => seq(optional($.repeat), $.element),

    /**
     * repeat = 1*DIGIT / (*DIGIT "*" *DIGIT)
     *
     * Examples: "3" (exactly 3), "*" (zero or more), "1*" (one or more),
     *           "*4" (at most 4), "2*5" (two to five).
     */
    repeat: ($) =>
      choice(repeat1($.DIGIT), seq(repeat($.DIGIT), "*", repeat($.DIGIT))),

    // ════════════════════════════════════════════════════════════
    //  Elements (atomic building blocks)
    // ════════════════════════════════════════════════════════════

    /**
     * element = rulename / group / option /
     *           char-val / num-val / prose-val
     *
     * We also recognise the 16 core rule names from RFC 5234
     * Appendix B.1 (ALPHA, BIT, …, WSP) as a separate node kind
     * so that downstream consumers can highlight them distinctly.
     */
    element: ($) =>
      choice(
        $.core_rulename,
        $.rulename,
        $.group,
        $.option,
        $.char_val,
        $.num_val,
        $.prose_val,
      ),

    /**
     * group = "(" *c-wsp alternation *c-wsp ")"
     */
    group: ($) =>
      seq("(", repeat($._c_wsp), $.alternation, repeat($._c_wsp), ")"),

    /**
     * option = "[" *c-wsp alternation *c-wsp "]"
     */
    option: ($) =>
      seq("[", repeat($._c_wsp), $.alternation, repeat($._c_wsp), "]"),

    // ════════════════════════════════════════════════════════════
    //  Terminal values — character strings  (RFC 7405)
    // ════════════════════════════════════════════════════════════

    /**
     * char-val = case-insensitive-string / case-sensitive-string
     */
    char_val: ($) => choice($.case_insensitive_string, $.case_sensitive_string),

    /**
     * case-insensitive-string = [ "%i" ] quoted-string
     */
    case_insensitive_string: ($) => seq(optional("%i"), $.quoted_string),

    /**
     * case-sensitive-string = "%s" quoted-string
     */
    case_sensitive_string: ($) => seq("%s", $.quoted_string),

    /**
     * quoted-string = DQUOTE *(%x20-21 / %x23-7E) DQUOTE
     *
     * A double-quoted literal.  Any printable ASCII character plus
     * space is allowed except the double-quote itself.
     */
    quoted_string: ($) => seq('"', /[\x20-\x21\x23-\x7E]*/, '"'),

    // ════════════════════════════════════════════════════════════
    //  Terminal values — numeric
    // ════════════════════════════════════════════════════════════

    /**
     * num-val = "%" (bin-val / dec-val / hex-val)
     */
    num_val: ($) => seq("%", choice($.bin_val, $.dec_val, $.hex_val)),

    /**
     * bin-val = "b" 1*BIT [ 1*("." 1*BIT) / ("-" 1*BIT) ]
     */
    bin_val: ($) => numValBody("b", $.BIT),

    /**
     * dec-val = "d" 1*DIGIT [ 1*("." 1*DIGIT) / ("-" 1*DIGIT) ]
     */
    dec_val: ($) => numValBody("d", $.DIGIT),

    /**
     * hex-val = "x" 1*HEXDIG [ 1*("." 1*HEXDIG) / ("-" 1*HEXDIG) ]
     */
    hex_val: ($) => numValBody("x", $.HEXDIG),

    // ════════════════════════════════════════════════════════════
    //  prose-val
    // ════════════════════════════════════════════════════════════

    /**
     * prose-val = "<" *(%x20-3D / %x3F-7E) ">"
     *
     * Angle-bracketed free-form text — a "last resort" mechanism
     * for describing rules in natural language.
     */
    prose_val: ($) => seq("<", /[\x20-\x3D\x3F-\x7E]*/, ">"),

    // ════════════════════════════════════════════════════════════
    //  Core rule names (RFC 5234, Appendix B.1)
    // ════════════════════════════════════════════════════════════

    /**
     * The 16 "well-known" core rules defined in Appendix B.1 of
     * RFC 5234.  Recognising them as a distinct node type lets
     * syntax highlighters colour them differently from user-defined
     * rule references.
     *
     * Because ABNF rule names are case-insensitive, we match these
     * in any casing.
     */
    core_rulename: ($) =>
      choice(
        /[Aa][Ll][Pp][Hh][Aa]/,
        /[Bb][Ii][Tt]/,
        /[Cc][Hh][Aa][Rr]/,
        /[Cc][Rr][Ll][Ff]/, // must come before CR
        /[Cc][Rr]/,
        /[Cc][Tt][Ll]/,
        /[Dd][Ii][Gg][Ii][Tt]/,
        /[Dd][Qq][Uu][Oo][Tt][Ee]/,
        /[Hh][Ee][Xx][Dd][Ii][Gg]/,
        /[Hh][Tt][Aa][Bb]/,
        /[Ll][Ff]/,
        /[Ll][Ww][Ss][Pp]/,
        /[Oo][Cc][Tt][Ee][Tt]/,
        /[Ss][Pp]/,
        /[Vv][Cc][Hh][Aa][Rr]/,
        /[Ww][Ss][Pp]/,
      ),

    // ════════════════════════════════════════════════════════════
    //  Core terminal symbols (RFC 5234, Appendix B.1)
    // ════════════════════════════════════════════════════════════

    /**
     * These are the atomic character classes used inside the grammar
     * rules above (repeat, bin-val digits, etc.).
     */

    /** ALPHA = %x41-5A / %x61-7A  ; A-Z / a-z */
    _ALPHA: ($) => /[A-Za-z]/,

    /** BIT = "0" / "1" */
    BIT: ($) => /[01]/,

    /** DIGIT = %x30-39  ; 0-9 */
    DIGIT: ($) => /[0-9]/,

    /** HEXDIG = DIGIT / "A"-"F"  (case-insensitive for practical use) */
    HEXDIG: ($) => /[0-9A-Fa-f]/,

    /** CR = %x0D */
    _CR: ($) => /\r/,

    /** LF = %x0A */
    _LF: ($) => /\n/,

    /** CRLF = CR LF  (we also accept bare LF for portability) */
    _CRLF: ($) => /\r?\n/,

    /** SP = %x20 */
    _SP: ($) => " ",

    /** HTAB = %x09 */
    _HTAB: ($) => "\t",

    /** WSP = SP / HTAB */
    _WSP: ($) => choice($._SP, $._HTAB),
  },
});

/// Helper: produces `seq(indicator, 1*digit_rule [ 1*("." 1*digit_rule) / ("-" 1*digit_rule) ])`
/// Shared pattern for bin-val, dec-val, hex-val.
/**
 * @param {RuleOrLiteral} indicator
 * @param {RuleOrLiteral} digitRule
 */
function numValBody(indicator, digitRule) {
  return seq(
    indicator,
    repeat1(digitRule),
    optional(
      choice(
        repeat1(seq(".", repeat1(digitRule))), // concatenation: %x61.62.63
        seq("-", repeat1(digitRule)), // range:         %x30-39
      ),
    ),
  );
}
