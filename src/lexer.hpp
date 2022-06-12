#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include "annotation.hpp"

#include <boost/algorithm/cxx11/find_if_not.hpp>
#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/type_traits/declval.hpp>
#include <boost/utility/string_view.hpp>
#include <boost/variant2/variant.hpp>
#include <mio/mmap.hpp>

#include <system_error>
#include <vector>

namespace lexer {
enum class symbol_t {
  ident,
  number,
  lparen,
  rparen,
  times,
  slash,
  plus,
  minus,
  eql,
  neq,
  lss,
  leq,
  gtr,
  geq,
  callsym,
  insym,
  outsym,
  beginsym,
  semicolon,
  endsym,
  ifsym,
  whilesym,
  becomes,
  thensym,
  dosym,
  constsym,
  comma,
  varsym,
  procsym,
  period,
  oddsym
};

namespace detail {
using read_result_t = boost::optional<std::pair<char const*, std::size_t>>;

[[nodiscard]] inline read_result_t read(boost::string_view path) noexcept {
  auto error = std::error_code();
  if (auto const input = mio::make_mmap_source(path, error); !error) {
    return std::pair(input.data(), input.size());
  }
  return boost::none;
}

template <typename T>
concept reader_concept = requires(T reader) {
  { reader(boost::string_view()) } -> std::same_as<detail::read_result_t>;
};
} // namespace detail

struct token_t {
  annotation_t annotation;
  symbol_t symbol;
};

struct lex_error_file_unreadable_t {
  boost::string_view path;
};
struct lex_error_missing_ending_period_t {
  boost::string_view path;
};
using tokens_t = std::vector<token_t>;
using lex_result_t =
    boost::variant2::variant<tokens_t, lex_error_file_unreadable_t,
                             lex_error_missing_ending_period_t>;

namespace detail {
[[nodiscard]] inline std::size_t
skip_whitespaces(char const* content, std::size_t size, std::size_t cur_pos) {
  auto const* const it = boost::algorithm::find_if_not(
      content + cur_pos, content + size,
      [](unsigned char c) { return std::isspace(c); });
  return static_cast<std::size_t>(std::distance(content, it));
}

template <reader_concept T>
[[nodiscard]] lex_result_t lex(boost::string_view path, T&& reader) {
  auto const possible_ret = std::forward<T>(reader)(path);
  if (!possible_ret.has_value()) {
    return lex_error_file_unreadable_t{path};
  }

  auto const [content, size] = *possible_ret;
  auto cur_pos = skip_whitespaces(content, size, 0);

  // empty file?
  if (cur_pos == size) {
    return lex_error_missing_ending_period_t{path};
  }

  auto tokens = tokens_t();
  if (content[cur_pos] == '.') {
    tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::period);
    ++cur_pos;
  }

  cur_pos = skip_whitespaces(content, size, cur_pos);
  if (cur_pos == size) {
    return tokens;
  }
  return {};
}
} // namespace detail

[[nodiscard]] inline lex_result_t lex(std::string const& path) {
  return detail::lex(path, detail::read);
}
} // namespace lexer

#endif
