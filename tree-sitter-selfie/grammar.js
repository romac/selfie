module.exports = grammar({
  name: 'selfie',

  extras: $ => [
    $.comment,
    /[\s\uFEFF\u2060\u200B\u00A0]/
  ],

  rules: {
    program: $ => repeat($._definition),

    _definition: $ => choice(
      $.module_declaration,
      $.function_declaration,
      $.struct_declaration,
      $.enum_declaration
    ),

    module_name: $ => $._upper,

    module_declaration: $ => seq(
      'module',
      $.module_name,
    ),

    function_declaration: $ => seq(
      'fn',
      $.name,
      $.parameter_list,
      ':',
      $.type,
      $.block
    ),

    struct_declaration: $ => seq(
      'struct',
      $.type,
      $.struct_body
    ),

    enum_declaration: $ => seq(
      'enum',
      $.type,
      $.enum_body
    ),

    parameter_list: $ => seq(
      '(',
      optional(commaSep1($.parameter)),
      ')'
    ),

    alias: $ => choice(
      $._under,
      $._lower
    ),

    name: $ => $._lower,

    parameter: $ => seq(
      optional($.alias),
      $.name,
      ':',
      $.type
    ),

    struct_body: $ => seq(
      '{',
      repeat($.struct_field),
      '}'
    ),

    struct_field: $ => seq(
      $.field,
      ':',
      $.type
    ),

    enum_body: $ => seq(
      '{',
      commaSep($.enum_case),
      optional(','),
      '}',
    ),

    enum_case: $ => seq(
      '.',
      $.variant,
      optional(seq(
        '(',
        commaSep1($.type),
        ')'
      )),
    ),

    block: $ => seq(
      '{',
      $.expression,
      '}'
    ),

    variable: $ => prec(2, choice($._under, $._lower)),

    expression: $ => choice(
      $.binary_expression,
      $.call_expression,
      $.tuple_expression,
      $.literal,
      $.match_expression,
      $.let_expression,
      $.if_expression,
      $.variable,
      $.field_access,
      $.tuple_access,
      $.struct_constructor_call,
      $.enum_constructor_call
    ),

    match_expression: $ => seq(
      'match',
      $.expression,
      '{',
      commaSep1($.match_arm),
      optional(','),
      '}'
    ),

    match_arm: $ => seq(
      $.pattern,
      '=>',
      $.expression
    ),

    pattern: $ => choice(
      $.tuple_pattern,
      $.enum_pattern,
      $.variable,
    ),

    tuple_pattern: $ => seq(
      '(',
      commaSep1($.pattern),
      ')'
    ),

    enum_pattern: $ => seq(
      optional($.type),
      '.',
      $.variant,
      optional(seq(
        '(',
        commaSep1($.pattern),
        ')'
      ))
    ),

    let_expression: $ => prec(2, seq(
      'let',
      $.name,
      '=',
      $.expression,
      optional(';'),
      $.expression
    )),

    if_expression: $ => seq(
      'if',
      $.expression,
      $.block,
      optional(seq(
        'else',
        $.block
      ))
    ),

    tuple_expression: $ => seq(
      '(',
      commaSep($.expression),
      ')'
    ),

    call_expression: $ => prec(2, seq(
      $.name,
      '(',
      commaSep($.call_argument),
      ')'
    )),

    call_argument: $ => seq(
      optional(seq($.name, ':')),
      $.expression
    ),

    binary_expression: $ => prec.left(seq(
      $.expression,
      $.binop,
      $.expression
    )),

    field: $ => prec(1, $._lower),

    field_access: $ => prec(1, seq(
      $.expression,
      '.',
      $.field
    )),

    tuple_access: $ => prec(1, seq(
      $.expression,
      '.',
      $.tuple_index
    )),

    tuple_index: $ => /\d+/,

    named_type: $ => $._upper,

    type: $ => choice(
      'Unit',
      'Int64',
      'Bool',
      'String',
      $.named_type,
      $.tuple_type
    ),

    tuple_type: $ => seq(
      '(',
      commaSep1($.type),
      ')'
    ),

    comment: $ => token(seq('//', /.*/)),

    literal: $ => choice(
      $.integer_literal,
      $.string_literal,
      $.boolean_literal
    ),

    integer_literal: $ => /\d+/,

    string_literal: $ => /"[^"]*"/,

    boolean_literal: $ => choice('true', 'false'),

    struct_constructor_call: $ => seq(
      $.type,
      '(',
      commaSep($.call_argument),
      ')'
    ),

    variant: $ => $._lower,

    enum_constructor_call: $ => seq(
      optional($.type),
      '.',
      $.variant
    ),

    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    _upper: $ => /[A-Z][a-zA-Z0-9_]*/,
    _lower: $ => /[a-z][a-zA-Z0-9_]*/,
    _under: $ => /_/,

    unop: $ => choice('-', '!'),
    binop: $ => choice('+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>=', '&&', '||'),
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}
