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
?a
.
  )"));

  [[maybe_unused]] auto parser = parser::parser_t(tokens);
  auto const result = parser.parse();
  EXPECT_TRUE(!!std::get_if<parser::parse_error_ok_t>(&result));
  parser.print();
}
