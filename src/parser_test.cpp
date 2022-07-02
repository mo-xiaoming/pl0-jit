#include "lexer.hpp"
#include "lexer_symbols.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

#include <gtest/gtest-param-test.h>
#include <gtest/gtest.h>
#include <variant>

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, EmptyFile) {
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"()"));

  auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_empty_file_t>(&result));
}

namespace {
struct test_data_t {
  std::string_view source;
  std::string_view expected;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, test_data_t v) {
    return os << utils::str::to_str("source:\n", v.source, "\n\n", "expected:\n", v.expected);
  }
};
// NOLINTNEXTLINE(hicpp-avoid-c-arrays, modernize-avoid-c-arrays, cppcoreguidelines-avoid-c-arrays)
constexpr test_data_t test_data[] = {
    // statements
    {R"(const a = 3;.)", "const a=3\n"},
    {R"(const b = 4, c = 5;.)", "const b=4\nconst c=5\n"},
    {R"(var e, f;.)", "var e\nvar f\n"},
    {R"(var g;.)", "var g\n"},
    {R"(var g;
?g.)",
     "var g\n?g\n"},
    {R"(call f.)", "call f\n"},
    {R"(var a;
begin
  ?a
end.)",
     R"(var a
begin
?a
end
)"},
    {R"(var a, b;
begin
  ?a;
  ?b
end.)",
     R"(var a
var b
begin
?a
?b
end
)"},
    {R"(var a, b;
begin
  begin
    ?b
  end
end.)",
     R"(var a
var b
begin
begin
?b
end
end
)"},
    {R"(var a, b;
begin
  ?a;
  begin
    ?b
  end
end.)",
     R"(var a
var b
begin
?a
begin
?b
end
end
)"},
    {R"(var a, b;
begin
  begin
    ?b
  end;
  ?a
end.)",
     R"(var a
var b
begin
begin
?b
end
?a
end
)"},
    {R"(var a, b, c;
begin
  begin
    ?b
  end;

  ?a;

  begin
    ?c
  end
end.)",
     R"(var a
var b
var c
begin
begin
?b
end
?a
begin
?c
end
end
)"},
    {R"(var a, b; if a # b then !a.)", "var a\nvar b\nif a#b then !a\n"},
    {R"(if odd 3 then !4.)", "if odd 3 then !4\n"},
    // expressions
};
} // namespace
struct ParserTestSuite : public testing::TestWithParam<test_data_t> {};

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
INSTANTIATE_TEST_SUITE_P(BasicData, ParserTestSuite, testing::ValuesIn(test_data));

// NOLINTNEXTLINE(cppcoreguidelines*, hicpp-special-member-functions)
TEST_P(ParserTestSuite, Basic) {
  auto const [source, expected] = GetParam();

  auto const tokens = std::get<lexer::tokens_t>(lex_string(source));
  auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_ok_t>(&result));
  EXPECT_EQ(utils::str::to_str(parser), expected);
}
#if 0
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, Expression) {
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"(
var x, a, b, c, abc;

!3
!x
!+42
!-42
!+x
!-x
!3+4
!3*4
!-3/4
!-3-4
!(3)
!-(3)
!(-4)
!-(-4)
!(3-4)
!(-3-4)

!3+4*5
!3*4+5
!3+4-5
!3+(4-5)
!(3+(4-5))
!(3+4)-5
!3+4*5/6-7
!3+4*5/(6-7)
!(3+4)*5/6-7
!(3+a)/4+7*(b-42)
!(3+a)/(4+7*(b-42))

abc:=3+a-b*c
.
  )"));

  auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_ok_t>(&result));
  std::cout << parser << '\n';
}
#endif
