; tree-sitter highlight queries for ABNF
; ────────────────────────────────────────

; Comments
(comment) @comment

; Rule name on the LHS of a definition
(rule
  (rulename) @function)

; Rule name on the RHS (references)
(element
  (rulename) @variable)

; Core rule names (ALPHA, DIGIT, CRLF, …)
(core_rulename) @type.builtin

; Operators
(defined_as) @operator
"/" @operator
"*" @operator

; Brackets
"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket

; String literals
(quoted_string) @string
(case_insensitive_string) @string
(case_sensitive_string) @string

; Numeric values
(num_val) @number
(bin_val) @number
(dec_val) @number
(hex_val) @number

; Prose values
(prose_val) @string.special

; Repeat quantifiers
(repeat) @keyword.operator
(DIGIT) @number