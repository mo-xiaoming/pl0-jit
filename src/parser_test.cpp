#include "lexer.hpp"
#include "lexer_symbols.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

#include <gtest/gtest.h>
#include <variant>

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, EmptyFile) {
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"()"));

  auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_empty_file_t>(&result));
}

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, OnlyPeriod) {}

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables, cppcoreguidelines-owning-memory)
TEST(ParserTestSuite, Const) {
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"(
const a = 3;
const b = 4, c = 5;
var e, f;
var g;
?a
?c
call b
.
  )"));

  auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_ok_t>(&result));
  std::cout << parser << '\n';
}

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
