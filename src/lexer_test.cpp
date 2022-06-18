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
TEST(LexerTestSuite, Normal) {
  auto const ret = lex_string(R"(var x, squ;

procedure square;
begin
   squ:= x * x
end;

begin
  x := 5
  call square
end.
)");

  constexpr auto tokens_oracle = std::array{
      lexer::symbol_t::var,       // var
      lexer::symbol_t::ident,     // x
      lexer::symbol_t::comma,     // ,
      lexer::symbol_t::ident,     // squ
      lexer::symbol_t::semicolon, // ;
      lexer::symbol_t::proc,      // procedure
      lexer::symbol_t::ident,     // square
      lexer::symbol_t::semicolon, // ;
      lexer::symbol_t::begin,     // begin
      lexer::symbol_t::ident,     // squ
      lexer::symbol_t::becomes,   // :=
      lexer::symbol_t::ident,     // x
      lexer::symbol_t::times,     // *
      lexer::symbol_t::ident,     // x
      lexer::symbol_t::end,       // end
      lexer::symbol_t::semicolon, // ;
      lexer::symbol_t::begin,     // begin
      lexer::symbol_t::ident,     // x
      lexer::symbol_t::becomes,   // :=
      lexer::symbol_t::number,    // 5
      lexer::symbol_t::call,      // call
      lexer::symbol_t::ident,     // square
      lexer::symbol_t::end,       // end
      lexer::symbol_t::period     // .
  };

  auto const* const tokens = std::get_if<lexer::tokens_t>(&ret);
  ASSERT_NE(tokens, nullptr);
  ASSERT_EQ(tokens->size(), tokens_oracle.size());
  for (std::size_t i = 0; i != tokens_oracle.size(); ++i) {
    EXPECT_EQ((*tokens)[i].symbol, tokens_oracle[i]) << "at loop " << i;
  }
}
