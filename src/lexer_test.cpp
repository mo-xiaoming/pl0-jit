#include "lexer.hpp"

#include <gtest/gtest.h>

#include <filesystem>

#if 0
namespace lexer {
void PrintTo(tokens_t const& tokens, std::ostream* os) {
  for (tokens_t::size_type i = 0U; i != tokens.size(); ++i) {
    *os << std::setw(4) << i << ' ' << tokens[i] << '\n';
  }
}
std::ostream& operator<<(std::ostream& os, tokens_t const& tokens) {
  for (tokens_t::size_type i = 0U; i != tokens.size(); ++i) {
    os << std::setw(4) << i << ' ' << tokens[i] << '\n';
  }
  return os;
}
} // namespace lexer
#endif
#if 0
template <auto N> std::ostream& operator<<(std::ostream& os, std::array<lexer::symbol_t, N> const& tokens) {
  for (typename std::decay_t<decltype(tokens)>::size_type i = 0U; i != tokens.size(); ++i) {
    os << std::setw(4) << i << ' ' << tokens[i] << '\n';
  }
  return os;
}
#endif
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
TEST(LexerTestSuite, Normal) {
  auto const ret = lex_string(R"(var x, squ;

procedure square;
const f := 3
begin
  while squ > 0 do if odd b then y := x + 3 / 4;
  while x <= y do if a>=b then w:=2*w-7;
  while x = y do if x # b then ?a;
  if x < y then b := (3 + 4) / 42;
  if b then call square;
end;

!a
.
)");

  constexpr auto tokens_oracle = std::array{
      lexer::symbol_t::var,           // var
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::comma,         // ,
      lexer::symbol_t::ident,         // squ
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::proc,          // procedure
      lexer::symbol_t::ident,         // square
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::const_,        // const
      lexer::symbol_t::ident,         // f
      lexer::symbol_t::becomes,       // :=
      lexer::symbol_t::number,        // 3
      lexer::symbol_t::begin,         // begin
      lexer::symbol_t::while_,        // while
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::greater,       // <=
      lexer::symbol_t::number,        // y
      lexer::symbol_t::do_,           // do
      lexer::symbol_t::if_,           // if
      lexer::symbol_t::odd,           // odd
      lexer::symbol_t::ident,         // b
      lexer::symbol_t::then,          // then
      lexer::symbol_t::ident,         // y
      lexer::symbol_t::becomes,       // :=
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::plus,          // +
      lexer::symbol_t::number,        // 3
      lexer::symbol_t::divide,        // /
      lexer::symbol_t::number,        // 4
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::while_,        // while
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::less_equal,    // <=
      lexer::symbol_t::ident,         // y
      lexer::symbol_t::do_,           // do
      lexer::symbol_t::if_,           // if
      lexer::symbol_t::ident,         // a
      lexer::symbol_t::greater_equal, // >=
      lexer::symbol_t::ident,         // b
      lexer::symbol_t::then,          // then
      lexer::symbol_t::ident,         // w
      lexer::symbol_t::becomes,       // :=
      lexer::symbol_t::number,        // 2
      lexer::symbol_t::times,         // *
      lexer::symbol_t::ident,         // w
      lexer::symbol_t::minus,         // -
      lexer::symbol_t::number,        // 7
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::while_,        // while
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::equal,         // =
      lexer::symbol_t::ident,         // y
      lexer::symbol_t::do_,           // do
      lexer::symbol_t::if_,           // if
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::not_equal,     // #
      lexer::symbol_t::ident,         // b
      lexer::symbol_t::then,          // then
      lexer::symbol_t::in,            // ?
      lexer::symbol_t::ident,         // a
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::if_,           // if
      lexer::symbol_t::ident,         // x
      lexer::symbol_t::less,          // <
      lexer::symbol_t::ident,         // y
      lexer::symbol_t::then,          // then
      lexer::symbol_t::ident,         // b
      lexer::symbol_t::becomes,       // :=
      lexer::symbol_t::lparen,        // (
      lexer::symbol_t::number,        // 3
      lexer::symbol_t::plus,          // +
      lexer::symbol_t::number,        // 4
      lexer::symbol_t::rparen,        // )
      lexer::symbol_t::divide,        // /
      lexer::symbol_t::number,        // 42
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::if_,           // if
      lexer::symbol_t::ident,         // b
      lexer::symbol_t::then,          // then
      lexer::symbol_t::call,          // call
      lexer::symbol_t::ident,         // square
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::end,           // end
      lexer::symbol_t::semicolon,     // ;
      lexer::symbol_t::out,           // !
      lexer::symbol_t::ident,         // a
      lexer::symbol_t::period         // .
  };

  auto const* const tokens = std::get_if<lexer::tokens_t>(&ret);
  ASSERT_NE(tokens, nullptr);
  EXPECT_EQ(tokens->size(), tokens_oracle.size());
  for (std::size_t i = 0; i != std::min(tokens_oracle.size(), tokens->size()); ++i) {
    EXPECT_EQ((*tokens)[i].symbol, tokens_oracle[i]) << "at loop " << i;
  }
}
