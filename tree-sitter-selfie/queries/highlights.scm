"module" @keyword
"fn"     @keyword
"struct" @keyword
"enum"   @keyword
"let"    @keyword
"if"     @keyword
"else"   @keyword

"(" @punctuation.bracket
")" @punctuation.bracket
;; "[" @punctuation.bracket
;; "]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket

;; "::" @punctuation.delimiter
":"  @punctuation.delimiter
"."  @punctuation.delimiter
","  @punctuation.delimiter
";"  @punctuation.delimiter

(type)    @type

(field)                 @property
(variant)               @property

(struct_declaration)  @type.declaration
(enum_declaration)    @type.declaration

(function_declaration) @local.scope
(variable)             @local.reference
(variable)             @variable

(parameter     (name) @variable.parameter)
(call_argument (name) @variable.parameter)

(let_expression (name) @local.definition)

(function_declaration (name) @function)
(call_expression (name)      @function)

;; (char_literal) @string
(string_literal) @string

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(tuple_index)     @constant.builtin
;; (float_literal) @constant.builtin

(comment) @comment
