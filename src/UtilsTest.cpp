#include "Utils.hpp"

#include <doctest/doctest.h>

TEST_CASE("splitLines") {
  SUBCASE("empty input yields one empty line") {
    const auto lines = splitLines("");
    CHECK_UNARY(lines.empty());
  }
  SUBCASE("one new line yields one empty line") {
    const auto lines = splitLines("\n");
    CHECK_EQ(lines.size(), 1);
    CHECK_EQ(lines[0], "");
  }
  SUBCASE("two consecutive new lines yield two empty lines") {
    const auto lines = splitLines("\n\n");
    CHECK_EQ(lines.size(), 2);
    CHECK_EQ(lines[0], "");
    CHECK_EQ(lines[1], "");
  }
  SUBCASE("string w/o EOL yields one line") {
    const auto lines = splitLines("abc");
    CHECK_EQ(lines.size(), 1);
    CHECK_EQ(lines[0], "abc");
  }
  SUBCASE("string w/ EOL yields one line") {
    const auto lines = splitLines("abc\n");
    CHECK_EQ(lines.size(), 1);
    CHECK_EQ(lines[0], "abc");
  }
  SUBCASE("string prefixed with new line yields one empty line and one none empty line") {
    const auto lines = splitLines("\nabc");
    CHECK_EQ(lines.size(), 2);
    CHECK_EQ(lines[0], "");
    CHECK_EQ(lines[1], "abc");
  }
  SUBCASE("string between two EOF yields one empty line and one none empty line") {
    const auto lines = splitLines("\nabc\n");
    CHECK_EQ(lines.size(), 2);
    CHECK_EQ(lines[0], "");
    CHECK_EQ(lines[1], "abc");
  }
  SUBCASE("EOF between two strings yield two lines") {
    const auto lines = splitLines("abc\ndef");
    CHECK_EQ(lines.size(), 2);
    CHECK_EQ(lines[0], "abc");
    CHECK_EQ(lines[1], "def");
  }
  SUBCASE("two strings both have EOL yield two lines") {
    const auto lines = splitLines("abc\ndef\n");
    CHECK_EQ(lines.size(), 2);
    CHECK_EQ(lines[0], "abc");
    CHECK_EQ(lines[1], "def");
  }
  SUBCASE("normal cases") {
    const auto lines = splitLines("abc\n\ndef\nghi\n");
    CHECK_EQ(lines.size(), 4);
    CHECK_EQ(lines[0], "abc");
    CHECK_EQ(lines[1], "");
    CHECK_EQ(lines[2], "def");
    CHECK_EQ(lines[3], "ghi");
  }
}

