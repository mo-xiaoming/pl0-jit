#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/type_traits/declval.hpp>
#include <boost/utility/string_view.hpp>
#include <boost/variant2/variant.hpp>
#include <mio/mmap.hpp>

#include <system_error>

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

struct lex_error_file_unreadable_t {
  boost::string_view path;
};
struct lex_error_missing_ending_period_t {
  boost::string_view path;
};
using lex_error_t = boost::variant2::variant<lex_error_file_unreadable_t,
                                             lex_error_missing_ending_period_t>;

namespace detail {
template <reader_concept T>
[[nodiscard]] lex_error_t lex(boost::string_view path, T&& reader) {
  auto const possible_ret = std::forward<T>(reader)(path);
  if (!possible_ret.has_value()) {
    return lex_error_file_unreadable_t{path};
  }

  auto const& [content, size] = *possible_ret;
  if (size == 0) {
    return lex_error_missing_ending_period_t{path};
  }
  return {};
}
} // namespace detail

[[nodiscard]] inline lex_error_t lex(std::string const& path) {
  return detail::lex(path, detail::read);
}
} // namespace lexer

#endif
