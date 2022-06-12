#include "lexer.hpp"

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
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
} // namespace

TEST_CASE("non-exist file reports on file not openable") {
  auto const tmp_path = get_unique_path();
  CHECK(tmp_path.has_value());

  auto const e = lexer::lex(tmp_path.value().string());
  REQUIRE(
      boost::variant2::holds_alternative<lexer::lex_error_file_unreadable_t>(
          e));
}

TEST_CASE("empty file reports on expecting period") {}
