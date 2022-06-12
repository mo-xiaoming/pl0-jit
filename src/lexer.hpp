#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include "annotation.hpp"
#include "utils.hpp"

#include <boost/algorithm/cxx11/find_if_not.hpp>
#include <boost/algorithm/cxx14/equal.hpp>
#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/type_traits/declval.hpp>
#include <boost/utility/string_view.hpp>
#include <boost/variant2/variant.hpp>
#include <fmt/core.h>
#include <mio/mmap.hpp>

#include <cstddef>
#include <system_error>
#include <vector>

namespace lexer {
enum class symbol_t {
  ident,
  semicolon,
  comma,
  number,
  lparen,
  rparen,
  times,
  divide,
  plus,
  minus,
  equal,
  not_equal,
  less,
  less_equal,
  greater,
  greater_equal,
  call,
  odd,
  in,
  out,
  begin,
  end,
  if_,
  while_,
  assign,
  then,
  do_,
  const_,
  var,
  proc,
  period,
};

[[nodiscard]] constexpr boost::string_view
stringify_symbol(symbol_t sym) noexcept {
  using enum symbol_t;
  // NOLINTNEXTLINE
  constexpr boost::string_view strings[] = {
      // NOLINTNEXTLINE
      [static_cast<std::size_t>(ident)] = "identifier",
      [static_cast<std::size_t>(semicolon)] = ";",
      [static_cast<std::size_t>(comma)] = ",",
      [static_cast<std::size_t>(number)] = "number",
      [static_cast<std::size_t>(lparen)] = "(",
      [static_cast<std::size_t>(rparen)] = ")",
      [static_cast<std::size_t>(times)] = "*",
      [static_cast<std::size_t>(divide)] = "/",
      [static_cast<std::size_t>(plus)] = "+",
      [static_cast<std::size_t>(minus)] = "-",
      [static_cast<std::size_t>(equal)] = "=",
      [static_cast<std::size_t>(not_equal)] = "#",
      [static_cast<std::size_t>(less)] = "<",
      [static_cast<std::size_t>(less_equal)] = "<=",
      [static_cast<std::size_t>(greater)] = ">",
      [static_cast<std::size_t>(greater_equal)] = ">=",
      [static_cast<std::size_t>(call)] = "call",
      [static_cast<std::size_t>(odd)] = "odd",
      [static_cast<std::size_t>(in)] = "?",
      [static_cast<std::size_t>(out)] = "!",
      [static_cast<std::size_t>(begin)] = "begin",
      [static_cast<std::size_t>(end)] = "end",
      [static_cast<std::size_t>(if_)] = "if",
      [static_cast<std::size_t>(while_)] = "while",
      [static_cast<std::size_t>(assign)] = ":=",
      [static_cast<std::size_t>(then)] = "then",
      [static_cast<std::size_t>(do_)] = "do",
      [static_cast<std::size_t>(const_)] = "const",
      [static_cast<std::size_t>(var)] = "var",
      [static_cast<std::size_t>(proc)] = "procedure",
      [static_cast<std::size_t>(period)] = ".",
  };
  return strings[static_cast<std::size_t>(sym)];
}

inline std::ostream& operator<<(std::ostream& os, symbol_t sym) {
  return os << stringify_symbol(sym);
}

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
      content + cur_pos, content + size, utils::isspace_s);
  return static_cast<std::size_t>(std::distance(content, it));
}

[[nodiscard]] inline boost::optional<char>
peek_next_char(char const* content, std::size_t size, std::size_t cur_pos) {
  if (cur_pos == size) {
    return boost::none;
  }
  return content[cur_pos];
}

[[nodiscard]] inline boost::string_view
get_number(char const* content, std::size_t size, std::size_t cur_pos) {
  auto const* const it = boost::algorithm::find_if_not(
      content + cur_pos, content + size, utils::isdigit_s);
  return {content + cur_pos,
          static_cast<std::size_t>(it - (content + cur_pos))};
}

[[nodiscard]] inline bool peek_keyword(char const* content, std::size_t size,
                                       std::size_t cur_pos,
                                       boost::string_view expected) {
  if (size - cur_pos < expected.size()) {
    return false;
  }
  if (!boost::algorithm::equal(content + cur_pos, content + size,
                               expected.cbegin(), expected.cend())) {
    return false;
  }
  if (cur_pos + expected.size() == size) {
    return true;
  }
  return !utils::isident_s(content[cur_pos + expected.size()]);
}

[[nodiscard]] inline boost::string_view
get_identifier(char const* content, std::size_t size, std::size_t cur_pos) {
  auto const* const it = boost::algorithm::find_if_not(
      content + cur_pos, content + size, utils::isident_s);
  return {content + cur_pos,
          static_cast<std::size_t>(it - (content + cur_pos))};
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

  while (auto const pc = peek_next_char(content, size, cur_pos)) {
    if (!pc.has_value()) {
      break;
    }
    char const c = *pc;
    switch (c) {
    case ';':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1},
                          symbol_t::semicolon);
      break;
    case ',':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::comma);
      break;
    case '(':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::lparen);
      break;
    case ')':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::rparen);
      break;
    case '*':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::times);
      break;
    case '/':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::divide);
      break;
    case '+':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::plus);
      break;
    case '-':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::minus);
      break;
    case '=':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::equal);
      break;
    case '#':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1},
                          symbol_t::not_equal);
      break;
    case '<': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        tokens.emplace_back(annotation_t{&content[cur_pos], 2},
                            symbol_t::less_equal);
        ++cur_pos;
      } else {
        tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::less);
      }
      break;
    }
    case '>': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        tokens.emplace_back(annotation_t{&content[cur_pos], 2},
                            symbol_t::greater_equal);
        ++cur_pos;
      } else {
        tokens.emplace_back(annotation_t{&content[cur_pos], 1},
                            symbol_t::greater);
      }
      break;
    }
    case ':': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        tokens.emplace_back(annotation_t{&content[cur_pos], 2},
                            symbol_t::assign);
        ++cur_pos;
      } else {
        fmt::print("unexpected char '{}', should it be ':='?\n", c);
      }
      break;
    }
    case '?':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::in);
      break;
    case '!':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::out);
      break;
    case '.':
      tokens.emplace_back(annotation_t{&content[cur_pos], 1}, symbol_t::period);
      break;
    case '0' ... '9': {
      auto const num = get_number(content, size, cur_pos);
      tokens.emplace_back(annotation_t{&content[cur_pos], num.size()},
                          symbol_t::number);
      cur_pos += num.size() - 1U;
      break;
    }
    case 'c': {
      if (peek_keyword(content, size, cur_pos, "call")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 4}, symbol_t::call);
        cur_pos += 3;
      } else if (peek_keyword(content, size, cur_pos, "const")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 5}, symbol_t::call);
        cur_pos += 4;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'o': {
      if (peek_keyword(content, size, cur_pos, "odd")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 3}, symbol_t::odd);
        cur_pos += 2;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'b': {
      if (peek_keyword(content, size, cur_pos, "begin")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 5},
                            symbol_t::begin);
        cur_pos += 4;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'e': {
      if (peek_keyword(content, size, cur_pos, "end")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 3}, symbol_t::end);
        cur_pos += 2;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'i': {
      if (peek_keyword(content, size, cur_pos, "if")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 2}, symbol_t::if_);
        cur_pos += 1;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'w': {
      if (peek_keyword(content, size, cur_pos, "while")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 5},
                            symbol_t::while_);
        cur_pos += 4;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 't': {
      if (peek_keyword(content, size, cur_pos, "then")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 4}, symbol_t::then);
        cur_pos += 3;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'd': {
      if (peek_keyword(content, size, cur_pos, "d")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 2}, symbol_t::do_);
        cur_pos += 1;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'v': {
      if (peek_keyword(content, size, cur_pos, "var")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 3}, symbol_t::var);
        cur_pos += 2;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    case 'p': {
      if (peek_keyword(content, size, cur_pos, "procedure")) {
        tokens.emplace_back(annotation_t{&content[cur_pos], 9}, symbol_t::proc);
        cur_pos += 8;
      } else {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
      }
      break;
    }
    default:
      if (utils::isspace_s(c)) {
        break;
      }
      if (utils::isalpha_s(c)) {
        auto const ident = get_identifier(content, size, cur_pos);
        tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()},
                            symbol_t::ident);
        cur_pos += ident.size() - 1U;
        break;
      }
      fmt::print("unknown char '{}'\n", c);
      break;
    }
    ++cur_pos;
  }
  return tokens;
}
} // namespace detail

[[nodiscard]] inline lex_result_t lex(std::string const& path) {
  return detail::lex(path, detail::read);
}
} // namespace lexer

#endif
