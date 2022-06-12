#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/type_traits/declval.hpp>
#include <boost/utility/string_ref.hpp>
#include <boost/variant2/variant.hpp>
#include <mio/mmap.hpp>

#include <system_error>

namespace lexer {
namespace detail {
using read_result_t = boost::optional<std::pair<char const*, std::size_t>>;

[[nodiscard]] inline read_result_t read(boost::string_ref path) noexcept {
  auto error = std::error_code();
  if (auto const input = mio::make_mmap_source(path, error); !error) {
    return std::pair(input.data(), input.size());
  }
  return boost::none;
}

template <typename T>
concept reader_concept = requires(T reader) {
  { reader(boost::string_ref()) } -> std::same_as<detail::read_result_t>;
};
} // namespace detail

struct lex_error_file_unreadable_t {
  boost::string_ref path;
};
struct lex_error_missing_ending_period_t {
  boost::string_ref path;
};
using lex_error_t = boost::variant2::variant<lex_error_file_unreadable_t,
                                             lex_error_missing_ending_period_t>;

namespace detail {
template <reader_concept T>
[[nodiscard]] lex_error_t lex(boost::string_ref path, T&& reader) {
  auto const possible_ret = std::forward<T>(reader)(path);
  if (!possible_ret.has_value()) {
    return lex_error_file_unreadable_t{path};
  }

  // auto const& [content, size] = possible_ret.value();
  return lex_error_missing_ending_period_t{path};
}
} // namespace detail

[[nodiscard]] inline lex_error_t lex(std::string const& path) {
  return detail::lex(path, detail::read);
}
} // namespace lexer

#endif
