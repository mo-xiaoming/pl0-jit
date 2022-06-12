#include "lexer.hpp"

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/optional/optional_io.hpp>
#include <boost/system/is_error_code_enum.hpp>

#include <doctest/doctest.h>

namespace {
[[nodiscard]] boost::optional<boost::filesystem::path> get_unique_path() {
  auto e = boost::system::error_code();
  if (auto const tmp_dir = boost::filesystem::temp_directory_path(e); !e) {
    return tmp_dir / boost::filesystem::unique_path();
  }
  return boost::none;
}

constexpr auto mock_source_path = boost::string_view();
} // namespace

TEST_CASE("non-exist file reports on file not openable") {
  auto const tmp_path = get_unique_path();
  CHECK(tmp_path.has_value());

  auto const path = tmp_path->string();

  auto const ret = lexer::lex(path);
  auto const* const error =
      boost::variant2::get_if<lexer::lex_error_file_unreadable_t>(&ret);
  CHECK_NE(error, nullptr);
  CHECK_EQ(error->path, path);
}

TEST_CASE("empty file reports on expecting period") {
  constexpr auto content = boost::string_view();

  auto const ret = lexer::detail::lex(
      mock_source_path, [content](auto) -> lexer::detail::read_result_t {
        return std::pair(content.data(), content.size());
      });
  auto const* const error =
      boost::variant2::get_if<lexer::lex_error_missing_ending_period_t>(&ret);
  CHECK_NE(error, nullptr);
  CHECK_EQ(error->path, mock_source_path);
}

#if 0
TEST_CASE("file with one period returns period") {
  constexpr auto content = boost::string_view(".");

  auto const ret = lexer::detail::lex(
      mock_source_path, [content](auto) -> lexer::detail::read_result_t {
        return std::pair(content.data(), content.size());
      });
  auto const* const error =
      boost::variant2::get_if<lexer::lex_error_missing_ending_period_t>(&ret);
  CHECK_NE(error, nullptr);
  CHECK_EQ(error->path, mock_source_path);
}
#endif