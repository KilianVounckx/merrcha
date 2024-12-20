const std = @import("std");
const testing = std.testing;

const merrcha = @import("merrcha");

const json = merrcha.combine(.{ ws, element, merrcha.eos }).asStr();

const value = merrcha.oneOf(.{
    object,
    array,
    jstring,
    number,
    jtrue,
    jfalse,
    jnull,
});

const members = merrcha.combine(.{
    member,
    merrcha.combine(.{ comma, member })
        .many(.{ .collect = false })
        .discard(),
});

const elements = merrcha.combine(.{
    element,
    merrcha.combine(.{ comma, element })
        .many(.{ .collect = false })
        .discard(),
});

const array = merrcha.combine(.{ lbracket, elements.opt().discard(), rbracket });
const element = merrcha.ref(valueRef);
const member = merrcha.combine(.{ jstring, colon, element });
const object = merrcha.combine(.{ lcurly, members.opt().discard(), rcurly });

fn valueRef() merrcha.Parser(
    void,
    merrcha.ParserError(ws) || error{
        @"Too few",
        @"Expected 'null'",
    },
) {
    return value;
}

const colon = token(merrcha.ascii.char(':'));
const comma = token(merrcha.ascii.char(','));
const jfalse = token(merrcha.string("false"));
const jnull = token(merrcha.string("null"));
const jstring = token(merrcha.combine(.{
    merrcha.ascii.char('"'),
    chars,
    merrcha.ascii.char('"'),
}));
const jtrue = token(merrcha.string("true"));
const lbracket = token(merrcha.ascii.char('['));
const lcurly = token(merrcha.ascii.char('{'));
const number = token(merrcha.combine(.{ integer, fraction, exponent }));
const rbracket = token(merrcha.ascii.char(']'));
const rcurly = token(merrcha.ascii.char('}'));

fn token(
    comptime parser: anytype,
) merrcha.Parser(void, merrcha.ParserError(parser) || merrcha.ParserError(ws)) {
    return merrcha.combine(.{ parser.discard(), ws });
}

const chars = char.many(.{ .collect = false }).discard();

const char = merrcha.oneOf(.{
    merrcha.ascii.range(' ', '"' - 1).discard(),
    merrcha.ascii.range('"' + 1, '\\' - 1).discard(),
    merrcha.ascii.range('\\' + 1, 255).discard(),
    merrcha.combine(.{
        merrcha.ascii.char('\\'),
        escape,
    }).discard(),
});

const escape = merrcha.oneOf(.{
    merrcha.ascii.char('"'),
    merrcha.ascii.char('\\'),
    merrcha.ascii.char('/'),
    merrcha.ascii.char('b'),
    merrcha.ascii.char('f'),
    merrcha.ascii.char('n'),
    merrcha.ascii.char('r'),
    merrcha.ascii.char('t'),
    merrcha.combine(.{ merrcha.ascii.char('u'), hex, hex, hex, hex }),
});

const hex = merrcha.oneOf(.{
    jdigit,
    merrcha.ascii.range('a', 'f').discard(),
    merrcha.ascii.range('A', 'F').discard(),
});

const integer = merrcha.oneOf(.{
    merrcha.combine(.{ onenine, digits }),
    jdigit,
    merrcha.combine(.{ merrcha.ascii.char('-').discard(), onenine, digits }),
    merrcha.combine(.{ merrcha.ascii.char('-').discard(), jdigit }),
});

const digits = jdigit.many(.{ .collect = false, .min = 1 }).discard();

const jdigit = merrcha.oneOf(.{
    merrcha.ascii.char('0').discard(),
    onenine,
});

const onenine = merrcha.ascii.range('1', '9').discard();

const fraction = merrcha.combine(.{
    merrcha.ascii.char('.').discard(),
    digits,
}).opt().discard();

const exponent = merrcha.combine(.{
    merrcha.oneOf(.{ merrcha.ascii.char('e'), merrcha.ascii.char('E') }).discard(),
    sign,
    digits,
}).opt().discard();

const sign = merrcha.oneOf(.{
    merrcha.ascii.char('-'),
    merrcha.ascii.char('+'),
}).opt().discard();

const ws = merrcha.oneOf(.{
    merrcha.ascii.char(' '),
    merrcha.ascii.char('\r'),
    merrcha.ascii.char('\n'),
    merrcha.ascii.char('\t'),
}).many(.{ .collect = false }).discard();

fn ok(s: []const u8) !void {
    const result = try json.parse(testing.allocator, s);
    try testing.expectEqualStrings(s, result.value);
    try testing.expectEqualStrings("", result.rest);
}

fn err(s: []const u8) !void {
    _ = json.parse(testing.allocator, s) catch return;
    return error.ExpectedError;
}

test "y_trailing_comma_after_empty" {
    try ok(
        \\{"1":[],"2":{},"3":"4"}
    );
}

test "y_array_arraysWithSpaces" {
    try ok(
        \\[[]   ]
    );
}

test "y_array_empty" {
    try ok(
        \\[]
    );
}

test "y_array_empty-string" {
    try ok(
        \\[""]
    );
}

test "y_array_ending_with_newline" {
    try ok(
        \\["a"]
    );
}

test "y_array_false" {
    try ok(
        \\[false]
    );
}

test "y_array_heterogeneous" {
    try ok(
        \\[null, 1, "1", {}]
    );
}

test "y_array_null" {
    try ok(
        \\[null]
    );
}

test "y_array_with_1_and_newline" {
    try ok(
        \\[1
        \\]
    );
}

test "y_array_with_leading_space" {
    try ok(
        \\ [1]
    );
}

test "y_array_with_several_null" {
    try ok(
        \\[1,null,null,null,2]
    );
}

test "y_array_with_trailing_space" {
    try ok("[2] ");
}

test "y_number_0e+1" {
    try ok(
        \\[0e+1]
    );
}

test "y_number_0e1" {
    try ok(
        \\[0e1]
    );
}

test "y_number_after_space" {
    try ok(
        \\[ 4]
    );
}

test "y_number_double_close_to_zero" {
    try ok(
        \\[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]
    );
}

test "y_number_int_with_exp" {
    try ok(
        \\[20e1]
    );
}

test "y_number" {
    try ok(
        \\[123e65]
    );
}

test "y_number_minus_zero" {
    try ok(
        \\[-0]
    );
}

test "y_number_negative_int" {
    try ok(
        \\[-123]
    );
}

test "y_number_negative_one" {
    try ok(
        \\[-1]
    );
}

test "y_number_negative_zero" {
    try ok(
        \\[-0]
    );
}

test "y_number_real_capital_e" {
    try ok(
        \\[1E22]
    );
}

test "y_number_real_capital_e_neg_exp" {
    try ok(
        \\[1E-2]
    );
}

test "y_number_real_capital_e_pos_exp" {
    try ok(
        \\[1E+2]
    );
}

test "y_number_real_exponent" {
    try ok(
        \\[123e45]
    );
}

test "y_number_real_fraction_exponent" {
    try ok(
        \\[123.456e78]
    );
}

test "y_number_real_neg_exp" {
    try ok(
        \\[1e-2]
    );
}

test "y_number_real_pos_exponent" {
    try ok(
        \\[1e+2]
    );
}

test "y_number_simple_int" {
    try ok(
        \\[123]
    );
}

test "y_number_simple_real" {
    try ok(
        \\[123.456789]
    );
}

test "y_object_basic" {
    try ok(
        \\{"asd":"sdf"}
    );
}

test "y_object_duplicated_key_and_value" {
    try ok(
        \\{"a":"b","a":"b"}
    );
}

test "y_object_duplicated_key" {
    try ok(
        \\{"a":"b","a":"c"}
    );
}

test "y_object_empty" {
    try ok(
        \\{}
    );
}

test "y_object_empty_key" {
    try ok(
        \\{"":0}
    );
}

test "y_object_escaped_null_in_key" {
    try ok(
        \\{"foo\u0000bar": 42}
    );
}

test "y_object_extreme_numbers" {
    try ok(
        \\{ "min": -1.0e+28, "max": 1.0e+28 }
    );
}

test "y_object" {
    try ok(
        \\{"asd":"sdf", "dfg":"fgh"}
    );
}

test "y_object_long_strings" {
    try ok(
        \\{"x":[{"id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}], "id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}
    );
}

test "y_object_simple" {
    try ok(
        \\{"a":[]}
    );
}

test "y_object_string_unicode" {
    try ok(
        \\{"title":"\u041f\u043e\u043b\u0442\u043e\u0440\u0430 \u0417\u0435\u043c\u043b\u0435\u043a\u043e\u043f\u0430" }
    );
}

test "y_object_with_newlines" {
    try ok(
        \\{
        \\"a": "b"
        \\}
    );
}

test "y_string_accepted_surrogate_pair" {
    try ok(
        \\["\uD801\udc37"]
    );
}

test "y_string_accepted_surrogate_pairs" {
    try ok(
        \\["\ud83d\ude39\ud83d\udc8d"]
    );
}

test "y_string_allowed_escapes" {
    try ok(
        \\["\"\\\/\b\f\n\r\t"]
    );
}

test "y_string_backslash_and_u_escaped_zero" {
    try ok(
        \\["\\u0000"]
    );
}

test "y_string_backslash_doublequotes" {
    try ok(
        \\["\""]
    );
}

test "y_string_comments" {
    try ok(
        \\["a/*b*/c/*d//e"]
    );
}

test "y_string_double_escape_a" {
    try ok(
        \\["\\a"]
    );
}

test "y_string_double_escape_n" {
    try ok(
        \\["\\n"]
    );
}

test "y_string_escaped_control_character" {
    try ok(
        \\["\u0012"]
    );
}

test "y_string_escaped_noncharacter" {
    try ok(
        \\["\uFFFF"]
    );
}

test "y_string_in_array" {
    try ok(
        \\["asd"]
    );
}

test "y_string_in_array_with_leading_space" {
    try ok(
        \\[ "asd"]
    );
}

test "y_string_last_surrogates_1_and_2" {
    try ok(
        \\["\uDBFF\uDFFF"]
    );
}

test "y_string_nbsp_uescaped" {
    try ok(
        \\["new\u00A0line"]
    );
}

test "y_string_null_escape" {
    try ok(
        \\["\u0000"]
    );
}

test "y_string_pi" {
    try ok(
        \\["π"]
    );
}

test "y_string_simple_ascii" {
    try ok(
        \\["asd "]
    );
}

test "y_string_space" {
    try ok(
        \\" "
    );
}

test "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF" {
    try ok(
        \\["\uD834\uDd1e"]
    );
}

test "y_string_u+2028_line_sep" {
    try ok("[\"\xe2\x80\xa8\"]");
}

test "y_string_u+2029_par_sep" {
    try ok("[\"\xe2\x80\xa9\"]");
}

test "y_string_uescaped_newline" {
    try ok(
        \\["new\u000Aline"]
    );
}

test "y_string_uEscape" {
    try ok(
        \\["\u0061\u30af\u30EA\u30b9"]
    );
}

test "y_string_unescaped_char_delete" {
    try ok("[\"\x7f\"]");
}

test "y_string_unicode_2" {
    try ok(
        \\["⍂㈴⍂"]
    );
}

test "y_string_unicodeEscapedBackslash" {
    try ok(
        \\["\u005C"]
    );
}

test "y_string_unicode_escaped_double_quote" {
    try ok(
        \\["\u0022"]
    );
}

test "y_string_unicode" {
    try ok(
        \\["\uA66D"]
    );
}

test "y_string_unicode_U+10FFFE_nonchar" {
    try ok(
        \\["\uDBFF\uDFFE"]
    );
}

test "y_string_unicode_U+1FFFE_nonchar" {
    try ok(
        \\["\uD83F\uDFFE"]
    );
}

test "y_string_unicode_U+200B_ZERO_WIDTH_SPACE" {
    try ok(
        \\["\u200B"]
    );
}

test "y_string_unicode_U+2064_invisible_plus" {
    try ok(
        \\["\u2064"]
    );
}

test "y_string_unicode_U+FDD0_nonchar" {
    try ok(
        \\["\uFDD0"]
    );
}

test "y_string_unicode_U+FFFE_nonchar" {
    try ok(
        \\["\uFFFE"]
    );
}

test "y_string_with_del_character" {
    try ok("[\"a\x7fa\"]");
}

test "y_structure_lonely_false" {
    try ok(
        \\false
    );
}

test "y_structure_lonely_int" {
    try ok(
        \\42
    );
}

test "y_structure_lonely_negative_real" {
    try ok(
        \\-0.1
    );
}

test "y_structure_lonely_null" {
    try ok(
        \\null
    );
}

test "y_structure_lonely_string" {
    try ok(
        \\"asd"
    );
}

test "y_structure_lonely_true" {
    try ok(
        \\true
    );
}

test "y_structure_string_empty" {
    try ok(
        \\""
    );
}

test "y_structure_trailing_newline" {
    try ok(
        \\["a"]
    );
}

test "y_structure_true_in_array" {
    try ok(
        \\[true]
    );
}

test "y_structure_whitespace_array" {
    try ok(" [] ");
}

test "n_array_1_true_without_comma" {
    try err(
        \\[1 true]
    );
}

test "n_array_colon_instead_of_comma" {
    try err(
        \\["": 1]
    );
}

test "n_array_comma_after_close" {
    try err(
        \\[""],
    );
}

test "n_array_comma_and_number" {
    try err(
        \\[,1]
    );
}

test "n_array_double_comma" {
    try err(
        \\[1,,2]
    );
}

test "n_array_double_extra_comma" {
    try err(
        \\["x",,]
    );
}

test "n_array_extra_close" {
    try err(
        \\["x"]]
    );
}

test "n_array_extra_comma" {
    try err(
        \\["",]
    );
}

test "n_array_incomplete_invalid_value" {
    try err(
        \\[x
    );
}

test "n_array_incomplete" {
    try err(
        \\["x"
    );
}

test "n_array_inner_array_no_comma" {
    try err(
        \\[3[4]]
    );
}

test "n_array_items_separated_by_semicolon" {
    try err(
        \\[1:2]
    );
}

test "n_array_just_comma" {
    try err(
        \\[,]
    );
}

test "n_array_just_minus" {
    try err(
        \\[-]
    );
}

test "n_array_missing_value" {
    try err(
        \\[   , ""]
    );
}

test "n_array_newlines_unclosed" {
    try err(
        \\["a",
        \\4
        \\,1,
    );
}

test "n_array_number_and_comma" {
    try err(
        \\[1,]
    );
}

test "n_array_number_and_several_commas" {
    try err(
        \\[1,,]
    );
}

test "n_array_spaces_vertical_tab_formfeed" {
    try err("[\"\x0aa\"\\f]");
}

test "n_array_star_inside" {
    try err(
        \\[*]
    );
}

test "n_array_unclosed" {
    try err(
        \\[""
    );
}

test "n_array_unclosed_trailing_comma" {
    try err(
        \\[1,
    );
}

test "n_array_unclosed_with_new_lines" {
    try err(
        \\[1,
        \\1
        \\,1
    );
}

test "n_array_unclosed_with_object_inside" {
    try err(
        \\[{}
    );
}

test "n_incomplete_false" {
    try err(
        \\[fals]
    );
}

test "n_incomplete_null" {
    try err(
        \\[nul]
    );
}

test "n_incomplete_true" {
    try err(
        \\[tru]
    );
}

test "n_multidigit_number_then_00" {
    try err("123\x00");
}

test "n_number_0.1.2" {
    try err(
        \\[0.1.2]
    );
}

test "n_number_-01" {
    try err(
        \\[-01]
    );
}

test "n_number_0.3e" {
    try err(
        \\[0.3e]
    );
}

test "n_number_0.3e+" {
    try err(
        \\[0.3e+]
    );
}

test "n_number_0_capital_E" {
    try err(
        \\[0E]
    );
}

test "n_number_0_capital_E+" {
    try err(
        \\[0E+]
    );
}

test "n_number_0.e1" {
    try err(
        \\[0.e1]
    );
}

test "n_number_0e" {
    try err(
        \\[0e]
    );
}

test "n_number_0e+" {
    try err(
        \\[0e+]
    );
}

test "n_number_1_000" {
    try err(
        \\[1 000.0]
    );
}

test "n_number_1.0e-" {
    try err(
        \\[1.0e-]
    );
}

test "n_number_1.0e" {
    try err(
        \\[1.0e]
    );
}

test "n_number_1.0e+" {
    try err(
        \\[1.0e+]
    );
}

test "n_number_-1.0." {
    try err(
        \\[-1.0.]
    );
}

test "n_number_1eE2" {
    try err(
        \\[1eE2]
    );
}

test "n_number_.-1" {
    try err(
        \\[.-1]
    );
}

test "n_number_+1" {
    try err(
        \\[+1]
    );
}

test "n_number_.2e-3" {
    try err(
        \\[.2e-3]
    );
}

test "n_number_2.e-3" {
    try err(
        \\[2.e-3]
    );
}

test "n_number_2.e+3" {
    try err(
        \\[2.e+3]
    );
}

test "n_number_2.e3" {
    try err(
        \\[2.e3]
    );
}

test "n_number_-2." {
    try err(
        \\[-2.]
    );
}

test "n_number_9.e+" {
    try err(
        \\[9.e+]
    );
}

test "n_number_expression" {
    try err(
        \\[1+2]
    );
}

test "n_number_hex_1_digit" {
    try err(
        \\[0x1]
    );
}

test "n_number_hex_2_digits" {
    try err(
        \\[0x42]
    );
}

test "n_number_infinity" {
    try err(
        \\[Infinity]
    );
}

test "n_number_+Inf" {
    try err(
        \\[+Inf]
    );
}

test "n_number_Inf" {
    try err(
        \\[Inf]
    );
}

test "n_number_invalid+-" {
    try err(
        \\[0e+-1]
    );
}

test "n_number_invalid-negative-real" {
    try err(
        \\[-123.123foo]
    );
}

test "n_number_++" {
    try err(
        \\[++1234]
    );
}

test "n_number_minus_infinity" {
    try err(
        \\[-Infinity]
    );
}

test "n_number_minus_sign_with_trailing_garbage" {
    try err(
        \\[-foo]
    );
}

test "n_number_minus_space_1" {
    try err(
        \\[- 1]
    );
}

test "n_number_-NaN" {
    try err(
        \\[-NaN]
    );
}

test "n_number_NaN" {
    try err(
        \\[NaN]
    );
}

test "n_number_neg_int_starting_with_zero" {
    try err(
        \\[-012]
    );
}

test "n_number_neg_real_without_int_part" {
    try err(
        \\[-.123]
    );
}

test "n_number_neg_with_garbage_at_end" {
    try err(
        \\[-1x]
    );
}

test "n_number_real_garbage_after_e" {
    try err(
        \\[1ea]
    );
}

test "n_number_real_without_fractional_part" {
    try err(
        \\[1.]
    );
}

test "n_number_starting_with_dot" {
    try err(
        \\[.123]
    );
}

test "n_number_U+FF11_fullwidth_digit_one" {
    try err(
        \\[ï¼]
    );
}

test "n_number_with_alpha_char" {
    try err(
        \\[1.8011670033376514H-308]
    );
}

test "n_number_with_alpha" {
    try err(
        \\[1.2a-3]
    );
}

test "n_number_with_leading_zero" {
    try err(
        \\[012]
    );
}

test "n_object_bad_value" {
    try err(
        \\["x", truth]
    );
}

test "n_object_bracket_key" {
    try err(
        \\{[: "x"}
    );
}

test "n_object_comma_instead_of_colon" {
    try err(
        \\{"x", null}
    );
}

test "n_object_double_colon" {
    try err(
        \\{"x"::"b"}
    );
}

test "n_object_emoji" {
    try err(
        \\{ð¨ð­}
    );
}

test "n_object_garbage_at_end" {
    try err(
        \\{"a":"a" 123}
    );
}

test "n_object_key_with_single_quotes" {
    try err(
        \\{key: 'value'}
    );
}

test "n_object_lone_continuation_byte_in_key_and_trailing_comma" {
    try err(
        \\{"¹":"0",}
    );
}

test "n_object_missing_colon" {
    try err(
        \\{"a" b}
    );
}

test "n_object_missing_key" {
    try err(
        \\{:"b"}
    );
}

test "n_object_missing_semicolon" {
    try err(
        \\{"a" "b"}
    );
}

test "n_object_missing_value" {
    try err(
        \\{"a":
    );
}

test "n_object_no-colon" {
    try err(
        \\{"a"
    );
}

test "n_object_non_string_key_but_huge_number_instead" {
    try err(
        \\{9999E9999:1}
    );
}

test "n_object_non_string_key" {
    try err(
        \\{1:1}
    );
}

test "n_object_repeated_null_null" {
    try err(
        \\{null:null,null:null}
    );
}

test "n_object_several_trailing_commas" {
    try err(
        \\{"id":0,,,,}
    );
}

test "n_object_single_quote" {
    try err(
        \\{'a':0}
    );
}

test "n_object_trailing_comma" {
    try err(
        \\{"id":0,}
    );
}

test "n_object_trailing_comment" {
    try err(
        \\{"a":"b"}/**/
    );
}

test "n_object_trailing_comment_open" {
    try err(
        \\{"a":"b"}/**//
    );
}

test "n_object_trailing_comment_slash_open_incomplete" {
    try err(
        \\{"a":"b"}/
    );
}

test "n_object_trailing_comment_slash_open" {
    try err(
        \\{"a":"b"}//
    );
}

test "n_object_two_commas_in_a_row" {
    try err(
        \\{"a":"b",,"c":"d"}
    );
}

test "n_object_unquoted_key" {
    try err(
        \\{a: "b"}
    );
}

test "n_object_unterminated-value" {
    try err(
        \\{"a":"a
    );
}

test "n_object_with_single_string" {
    try err(
        \\{ "foo" : "bar", "a" }
    );
}

test "n_object_with_trailing_garbage" {
    try err(
        \\{"a":"b"}#
    );
}

test "n_single_space" {
    try err(" ");
}

test "n_string_1_surrogate_then_escape" {
    try err(
        \\["\uD800\"]
    );
}

test "n_string_1_surrogate_then_escape_u1" {
    try err(
        \\["\uD800\u1"]
    );
}

test "n_string_1_surrogate_then_escape_u1x" {
    try err(
        \\["\uD800\u1x"]
    );
}

test "n_string_1_surrogate_then_escape_u" {
    try err(
        \\["\uD800\u"]
    );
}

test "n_string_accentuated_char_no_quotes" {
    try err(
        \\[Ã©]
    );
}

test "n_string_backslash_00" {
    try err("[\"\x00\"]");
}

test "n_string_escaped_backslash_bad" {
    try err(
        \\["\\\"]
    );
}

test "n_string_escaped_ctrl_char_tab" {
    try err("\x5b\x22\x5c\x09\x22\x5d");
}

test "n_string_escaped_emoji" {
    try err("[\"\x5c\xc3\xb0\xc2\x9f\xc2\x8c\xc2\x80\"]");
}

test "n_string_escape_x" {
    try err(
        \\["\x00"]
    );
}

test "n_string_incomplete_escaped_character" {
    try err(
        \\["\u00A"]
    );
}

test "n_string_incomplete_escape" {
    try err(
        \\["\"]
    );
}

test "n_string_incomplete_surrogate_escape_invalid" {
    try err(
        \\["\uD800\uD800\x"]
    );
}

test "n_string_incomplete_surrogate" {
    try err(
        \\["\uD834\uDd"]
    );
}

test "n_string_invalid_backslash_esc" {
    try err(
        \\["\a"]
    );
}

test "n_string_invalid_unicode_escape" {
    try err(
        \\["\uqqqq"]
    );
}

test "n_string_leading_uescaped_thinspace" {
    try err(
        \\[\u0020"asd"]
    );
}

test "n_string_no_quotes_with_bad_escape" {
    try err(
        \\[\n]
    );
}

test "n_string_single_doublequote" {
    try err(
        \\"
    );
}

test "n_string_single_quote" {
    try err(
        \\['single quote']
    );
}

test "n_string_single_string_no_double_quotes" {
    try err(
        \\abc
    );
}

test "n_string_start_escape_unclosed" {
    try err(
        \\["\
    );
}

test "n_string_unescaped_crtl_char" {
    try err("[\"a\x00a\"]");
}

test "n_string_unescaped_newline" {
    try err(
        \\["new
        \\line"]
    );
}

test "n_string_unescaped_tab" {
    try err("[\"\t\"]");
}

test "n_string_unicode_CapitalU" {
    try err(
        \\"\UA66D"
    );
}

test "n_string_with_trailing_garbage" {
    try err(
        \\""x
    );
}

test "n_structure_100000_opening_arrays" {
    return error.SkipZigTest;
    // try err( "[" ** 100000);
}

test "n_structure_angle_bracket_." {
    try err(
        \\<.>
    );
}

test "n_structure_angle_bracket_null" {
    try err(
        \\[<null>]
    );
}

test "n_structure_array_trailing_garbage" {
    try err(
        \\[1]x
    );
}

test "n_structure_array_with_extra_array_close" {
    try err(
        \\[1]]
    );
}

test "n_structure_array_with_unclosed_string" {
    try err(
        \\["asd]
    );
}

test "n_structure_ascii-unicode-identifier" {
    try err(
        \\aÃ¥
    );
}

test "n_structure_capitalized_True" {
    try err(
        \\[True]
    );
}

test "n_structure_close_unopened_array" {
    try err(
        \\1]
    );
}

test "n_structure_comma_instead_of_closing_brace" {
    try err(
        \\{"x": true,
    );
}

test "n_structure_double_array" {
    try err(
        \\[][]
    );
}

test "n_structure_end_array" {
    try err(
        \\]
    );
}

test "n_structure_lone-open-bracket" {
    try err(
        \\[
    );
}

test "n_structure_no_data" {
    try err(
        \\
    );
}

test "n_structure_null-byte-outside-string" {
    try err("[\x00]");
}

test "n_structure_number_with_trailing_garbage" {
    try err(
        \\2@
    );
}

test "n_structure_object_followed_by_closing_object" {
    try err(
        \\{}}
    );
}

test "n_structure_object_unclosed_no_value" {
    try err(
        \\{"":
    );
}

test "n_structure_object_with_comment" {
    try err(
        \\{"a":/*comment*/"b"}
    );
}

test "n_structure_object_with_trailing_garbage" {
    try err(
        \\{"a": true} "x"
    );
}

test "n_structure_open_array_apostrophe" {
    try err(
        \\['
    );
}

test "n_structure_open_array_comma" {
    try err(
        \\[,
    );
}

test "n_structure_open_array_object" {
    return error.SkipZigTest;
    // try err( "[{\"\":" ** 50000);
}

test "n_structure_open_array_open_object" {
    try err(
        \\[{
    );
}

test "n_structure_open_array_open_string" {
    try err(
        \\["a
    );
}

test "n_structure_open_array_string" {
    try err(
        \\["a"
    );
}

test "n_structure_open_object_close_array" {
    try err(
        \\{]
    );
}

test "n_structure_open_object_comma" {
    try err(
        \\{,
    );
}

test "n_structure_open_object" {
    try err(
        \\{
    );
}

test "n_structure_open_object_open_array" {
    try err(
        \\{[
    );
}

test "n_structure_open_object_open_string" {
    try err(
        \\{"a
    );
}

test "n_structure_open_object_string_with_apostrophes" {
    try err(
        \\{'a'
    );
}

test "n_structure_open_open" {
    try err(
        \\["\{["\{["\{["\{
    );
}

test "n_structure_single_eacute" {
    try err(
        \\é
    );
}

test "n_structure_single_star" {
    try err(
        \\*
    );
}

test "n_structure_trailing_#" {
    try err(
        \\{"a":"b"}#{}
    );
}

test "n_structure_U+2060_word_joined" {
    try err(
        \\[â ]
    );
}

test "n_structure_uescaped_LF_before_string" {
    try err(
        \\[\u000A""]
    );
}

test "n_structure_unclosed_array" {
    try err(
        \\[1
    );
}

test "n_structure_unclosed_array_partial_null" {
    try err(
        \\[ false, nul
    );
}

test "n_structure_unclosed_array_unfinished_false" {
    try err(
        \\[ true, fals
    );
}

test "n_structure_unclosed_array_unfinished_true" {
    try err(
        \\[ false, tru
    );
}

test "n_structure_unclosed_object" {
    try err(
        \\{"asd":"asd"
    );
}

test "n_structure_unicode-identifier" {
    try err(
        \\Ã¥
    );
}

test "n_structure_whitespace_formfeed" {
    try err("[\x0c]");
}

test "n_structure_whitespace_U+2060_word_joiner" {
    try err(
        \\[â ]
    );
}

test "i_number_double_huge_neg_exp" {
    try ok(
        \\[123.456e-789]
    );
}

test "i_number_huge_exp" {
    try ok(
        \\[0.4e00669999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999969999999006]
    );
}

test "i_number_neg_int_huge_exp" {
    try ok(
        \\[-1e+9999]
    );
}

test "i_number_pos_double_huge_exp" {
    try ok(
        \\[1.5e+9999]
    );
}

test "i_number_real_neg_overflow" {
    try ok(
        \\[-123123e100000]
    );
}

test "i_number_real_pos_overflow" {
    try ok(
        \\[123123e100000]
    );
}

test "i_number_real_underflow" {
    try ok(
        \\[123e-10000000]
    );
}

test "i_number_too_big_neg_int" {
    try ok(
        \\[-123123123123123123123123123123]
    );
}

test "i_number_too_big_pos_int" {
    try ok(
        \\[100000000000000000000]
    );
}

test "i_number_very_big_negative_int" {
    try ok(
        \\[-237462374673276894279832749832423479823246327846]
    );
}

test "i_object_key_lone_2nd_surrogate" {
    try ok(
        \\{"\uDFAA":0}
    );
}

test "i_string_1st_surrogate_but_2nd_missing" {
    try ok(
        \\["\uDADA"]
    );
}

test "i_string_1st_valid_surrogate_2nd_invalid" {
    try ok(
        \\["\uD888\u1234"]
    );
}

test "i_string_incomplete_surrogate_and_escape_valid" {
    try ok(
        \\["\uD800\n"]
    );
}

test "i_string_incomplete_surrogate_pair" {
    try ok(
        \\["\uDd1ea"]
    );
}

test "i_string_incomplete_surrogates_escape_valid" {
    try ok(
        \\["\uD800\uD800\n"]
    );
}

test "i_string_invalid_lonely_surrogate" {
    try ok(
        \\["\ud800"]
    );
}

test "i_string_invalid_surrogate" {
    try ok(
        \\["\ud800abc"]
    );
}

test "i_string_inverted_surrogates_U+1D11E" {
    try ok(
        \\["\uDd1e\uD834"]
    );
}

test "i_string_iso_latin_1" {
    try ok(
        \\["é"]
    );
}

test "i_string_lone_second_surrogate" {
    try ok(
        \\["\uDFAA"]
    );
}

test "i_string_not_in_unicode_range" {
    try ok(
        \\["ô¿¿¿"]
    );
}

test "i_string_overlong_sequence_2_bytes" {
    try ok(
        \\["À¯"]
    );
}

test "i_string_overlong_sequence_6_bytes" {
    try ok(
        \\["ü¿¿¿¿"]
    );
}

test "i_string_overlong_sequence_6_bytes_null" {
    try ok(
        \\["ü"]
    );
}

test "i_structure_500_nested_arrays" {
    try ok(("[" ** 500) ++ ("]" ** 500));
}
