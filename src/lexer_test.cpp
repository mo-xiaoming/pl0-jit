#include "annotation.hpp"
#include "lexer.hpp"

#include <boost/algorithm/searching/boyer_moore.hpp>
#include <boost/array.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/optional/optional_io.hpp>
#include <boost/system/is_error_code_enum.hpp>

#include <boost/utility/string_view_fwd.hpp>
#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators.hpp>

namespace {
[[nodiscard]] boost::optional<boost::filesystem::path> get_unique_path() {
  auto e = boost::system::error_code();
  if (auto const tmp_dir = boost::filesystem::temp_directory_path(e); !e) {
    return tmp_dir / boost::filesystem::unique_path();
  }
  return boost::none;
}

constexpr auto mock_source_path = boost::string_view();

[[nodiscard]] lexer::lex_result_t lex(boost::string_view content) {
  return lexer::detail::lex(mock_source_path,
                            [content](auto) -> lexer::detail::read_result_t {
                              return std::pair(content.data(), content.size());
                            });
}

[[nodiscard]] annotation_t create_test_token(boost::string_view content,
                                             boost::string_view symbol_str) {
  auto const pos = content.find(symbol_str);
  REQUIRE(pos != boost::string_view::npos);
  return {.start = &content[pos], .length = symbol_str.size()};
}
} // namespace

// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
TEST_CASE("non-exist file reports on file not openable", "[lexer]") {
  auto const tmp_path = get_unique_path();
  CHECK(tmp_path.has_value());

  auto const path = tmp_path->string();

  auto const ret = lexer::lex(path);
  auto const* const error =
      boost::variant2::get_if<lexer::lex_error_file_unreadable_t>(&ret);
  REQUIRE(error != nullptr);
  REQUIRE(error->path == path);
}

TEST_CASE("empty file reports on expecting period", "[lexer]") {
  boost::string_view const content = GENERATE("", "\n \t  \n");

  CAPTURE(content);

  auto const ret = lex(content);

  auto const* const error =
      boost::variant2::get_if<lexer::lex_error_missing_ending_period_t>(&ret);
  REQUIRE(error != nullptr);
  REQUIRE(error->path == mock_source_path);
}

TEST_CASE("file with one period returns period", "[lexer]") {
  boost::string_view const content =
      GENERATE(".", "  .", "\n \t \v  \n.", " \n   . \n");

  CAPTURE(content);

  auto const ret = lex(content);

  auto const* const tokens = boost::variant2::get_if<lexer::tokens_t>(&ret);
  REQUIRE(tokens != nullptr);
  REQUIRE(tokens->size() == 1U);
  REQUIRE((*tokens)[0].symbol == lexer::symbol_t::period);

  auto const annotation_oracle = create_test_token(content, ".");
  CAPTURE(annotation_oracle);
  REQUIRE((*tokens)[0].annotation == annotation_oracle);
}