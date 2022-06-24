#include "lexer.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

#include <gtest/gtest.h>

TEST(ParserTestSuite, Scarfolding) {
  constexpr auto src = std::string_view(".");
  auto const ret = lex_string(src);
  auto const* const tokens = std::get_if<lexer::tokens_t>(&ret);
  ASSERT_NE(tokens, nullptr);
  EXPECT_EQ(tokens->size(), 1);

  auto parser = parser::parser_t(*tokens);
  EXPECT_EQ(parser.eval(), 0);
}
