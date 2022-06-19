#include "annotation.hpp"
#include "lexer.hpp"

#include <gtest/gtest.h>

#include <filesystem>

namespace {
std::string const& get_mock_source_path() {
  static auto const mock_source_path = std::string("hopefully_this_does_not_exist");
  return mock_source_path;
}

lexer::lex_result_t lex_string(std::string_view content) {
  auto const cursor = lexer::internal::source_cursor_t(lexer::internal::source_content_t(content.data()),
                                                       lexer::internal::source_size_t(content.size()),
                                                       lexer::internal::source_position_t(0U));
  return lexer::internal::lex(cursor);
}
} // namespace

// NOLINTNEXTLINE
TEST(LexerTestSuite, NonExistSourceFileShouldReturnFileUnreadableError) {
  auto const& mock_source_path = get_mock_source_path();

  auto err = std::error_code();
  auto const exists = std::filesystem::exists(mock_source_path, err);
  ASSERT_TRUE(!exists && !err) << "exists: " << exists << ", err: " << err.message();

  auto const ret = lexer::lex_source_file(mock_source_path);
  auto const* const error = std::get_if<lexer::lex_error_file_unreadable_t>(&ret);
  ASSERT_NE(error, nullptr);
  ASSERT_EQ(error->source_path, mock_source_path);
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, IncompleteBecomes) {
  constexpr auto src = std::string_view(R"(a : b;)");
  auto const ret = lex_string(src);
  auto const* const error = std::get_if<lexer::lex_unexpected_char_t>(&ret);
  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->expected, ":=");
  EXPECT_EQ(error->annotation, (annotation_t{.start = src.data() + 2, .length = 2U}));
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, UnrecognizedChar) {
  constexpr auto src = std::string_view(R"(a@ : b;)");
  auto const ret = lex_string(src);
  auto const* const error = std::get_if<lexer::lex_unknown_char_t>(&ret);
  ASSERT_NE(error, nullptr);
  EXPECT_EQ(error->annotation, (annotation_t{.start = src.data() + 1, .length = 1U}));
}

// NOLINTNEXTLINE
TEST(LexerTestSuite, Normal) {
  constexpr auto src = std::string_view(R"(var x, squ;

procedure square;
const f := 3;
begin
  while squ > 0 do if odd b then y := x + 3 / 4;
  while x <= y do if a>=b then w:=2*w-7;
  while x = y do if x#b then ?a;
  if x < y then b := (3 + 4) / 42;
  if b then call square;
end;

!a
.
)");

  auto const ret = lex_string(src);

  using enum lexer::symbol_t;
  constexpr auto tokens_oracle = std::array{
      lexer::token_t{.symbol = var, .annotation = {.start = src.data() + 0U, .length = 3U}},             // var
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 4U, .length = 1U}},           // x
      lexer::token_t{.symbol = comma, .annotation = {.start = src.data() + 5U, .length = 1U}},           // ,
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 7U, .length = 3U}},           // squ
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 10U, .length = 1U}},      // ;
      lexer::token_t{.symbol = proc, .annotation = {.start = src.data() + 13U, .length = 9U}},           // procedure
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 23U, .length = 6U}},          // square
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 29U, .length = 1U}},      // ;
      lexer::token_t{.symbol = const_, .annotation = {.start = src.data() + 31U, .length = 5U}},         // const
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 37U, .length = 1U}},          // f
      lexer::token_t{.symbol = becomes, .annotation = {.start = src.data() + 39U, .length = 2U}},        // :=
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 42U, .length = 1U}},         // 3
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 43U, .length = 1U}},      // ;
      lexer::token_t{.symbol = begin, .annotation = {.start = src.data() + 45U, .length = 5U}},          // begin
      lexer::token_t{.symbol = while_, .annotation = {.start = src.data() + 53U, .length = 5U}},         // while
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 59U, .length = 3U}},          // x
      lexer::token_t{.symbol = greater, .annotation = {.start = src.data() + 63U, .length = 1U}},        // <=
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 65U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.start = src.data() + 67U, .length = 2U}},            // do
      lexer::token_t{.symbol = if_, .annotation = {.start = src.data() + 70U, .length = 2U}},            // if
      lexer::token_t{.symbol = odd, .annotation = {.start = src.data() + 73U, .length = 3U}},            // odd
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 77U, .length = 1U}},          // b
      lexer::token_t{.symbol = then, .annotation = {.start = src.data() + 79U, .length = 4U}},           // then
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 84U, .length = 1U}},          // y
      lexer::token_t{.symbol = becomes, .annotation = {.start = src.data() + 86U, .length = 2U}},        // :=
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 89U, .length = 1U}},          // x
      lexer::token_t{.symbol = plus, .annotation = {.start = src.data() + 91U, .length = 1U}},           // +
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 93U, .length = 1U}},         // 3
      lexer::token_t{.symbol = divide, .annotation = {.start = src.data() + 95U, .length = 1U}},         // /
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 97U, .length = 1U}},         // 4
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 98U, .length = 1U}},      // ;
      lexer::token_t{.symbol = while_, .annotation = {.start = src.data() + 102U, .length = 5U}},        // while
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 108U, .length = 1U}},         // x
      lexer::token_t{.symbol = less_equal, .annotation = {.start = src.data() + 110U, .length = 2U}},    // <=
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 113U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.start = src.data() + 115U, .length = 2U}},           // do
      lexer::token_t{.symbol = if_, .annotation = {.start = src.data() + 118U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 121U, .length = 1U}},         // a
      lexer::token_t{.symbol = greater_equal, .annotation = {.start = src.data() + 122U, .length = 2U}}, // >=
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 124U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.start = src.data() + 126U, .length = 4U}},          // then
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 131U, .length = 1U}},         // w
      lexer::token_t{.symbol = becomes, .annotation = {.start = src.data() + 132U, .length = 2U}},       // :=
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 134U, .length = 1U}},        // 2
      lexer::token_t{.symbol = times, .annotation = {.start = src.data() + 135U, .length = 1U}},         // *
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 136U, .length = 1U}},         // w
      lexer::token_t{.symbol = minus, .annotation = {.start = src.data() + 137U, .length = 1U}},         // -
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 138U, .length = 1U}},        // 7
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 139U, .length = 1U}},     // ;
      lexer::token_t{.symbol = while_, .annotation = {.start = src.data() + 143U, .length = 5U}},        // while
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 149U, .length = 1U}},         // x
      lexer::token_t{.symbol = equal, .annotation = {.start = src.data() + 151U, .length = 1U}},         // =
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 153U, .length = 1U}},         // y
      lexer::token_t{.symbol = do_, .annotation = {.start = src.data() + 155U, .length = 2U}},           // do
      lexer::token_t{.symbol = if_, .annotation = {.start = src.data() + 158U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 161U, .length = 1U}},         // x
      lexer::token_t{.symbol = not_equal, .annotation = {.start = src.data() + 162U, .length = 1U}},     // #
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 163U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.start = src.data() + 165U, .length = 4U}},          // then
      lexer::token_t{.symbol = in, .annotation = {.start = src.data() + 170U, .length = 1U}},            // ?
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 171U, .length = 1U}},         // a
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 172U, .length = 1U}},     // ;
      lexer::token_t{.symbol = if_, .annotation = {.start = src.data() + 176U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 179U, .length = 1U}},         // x
      lexer::token_t{.symbol = less, .annotation = {.start = src.data() + 181U, .length = 1U}},          // <
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 183U, .length = 1U}},         // y
      lexer::token_t{.symbol = then, .annotation = {.start = src.data() + 185U, .length = 4U}},          // then
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 190U, .length = 1U}},         // b
      lexer::token_t{.symbol = becomes, .annotation = {.start = src.data() + 192U, .length = 2U}},       // :=
      lexer::token_t{.symbol = lparen, .annotation = {.start = src.data() + 195U, .length = 1U}},        // (
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 196U, .length = 1U}},        // 3
      lexer::token_t{.symbol = plus, .annotation = {.start = src.data() + 198U, .length = 1U}},          // +
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 200U, .length = 1U}},        // 4
      lexer::token_t{.symbol = rparen, .annotation = {.start = src.data() + 201U, .length = 1U}},        // )
      lexer::token_t{.symbol = divide, .annotation = {.start = src.data() + 203U, .length = 1U}},        // /
      lexer::token_t{.symbol = number, .annotation = {.start = src.data() + 205U, .length = 2U}},        // 42
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 207U, .length = 1U}},     // ;
      lexer::token_t{.symbol = if_, .annotation = {.start = src.data() + 211U, .length = 2U}},           // if
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 214U, .length = 1U}},         // b
      lexer::token_t{.symbol = then, .annotation = {.start = src.data() + 216U, .length = 4U}},          // then
      lexer::token_t{.symbol = call, .annotation = {.start = src.data() + 221U, .length = 4U}},          // call
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 226U, .length = 6U}},         // square
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 232U, .length = 1U}},     // ;
      lexer::token_t{.symbol = end, .annotation = {.start = src.data() + 234U, .length = 3U}},           // end
      lexer::token_t{.symbol = semicolon, .annotation = {.start = src.data() + 237U, .length = 1U}},     // ;
      lexer::token_t{.symbol = out, .annotation = {.start = src.data() + 240U, .length = 1U}},           // !
      lexer::token_t{.symbol = ident, .annotation = {.start = src.data() + 241U, .length = 1U}},         // a
      lexer::token_t{.symbol = period, .annotation = {.start = src.data() + 243U, .length = 1U}},        // .
  };

  auto const* const tokens = std::get_if<lexer::tokens_t>(&ret);
  ASSERT_NE(tokens, nullptr);
  EXPECT_EQ(tokens->size(), tokens_oracle.size());
  for (std::size_t i = 0; i != std::min(tokens_oracle.size(), tokens->size()); ++i) {
    ASSERT_EQ((*tokens)[i], tokens_oracle[i]) << "at loop " << i;
  }
}
