#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include "annotation.hpp"
#include "utils.hpp"

#include <boost/algorithm/cxx11/find_if_not.hpp>
#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/utility/string_view.hpp>
#include <boost/variant2/variant.hpp>
#include <fmt/core.h>
#include <mio/mmap.hpp>

#include <cstddef>
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
  becomes,
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
      [static_cast<std::size_t>(becomes)] = ":=",
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
    boost::variant2::variant<tokens_t, lex_error_file_unreadable_t>;

namespace detail {
#if 0
[[nodiscard]] inline std::size_t
skip_whitespaces(char const* content, std::size_t size, std::size_t cur_pos) {
  auto const* const it = boost::algorithm::find_if_not(
      content + cur_pos, content + size, utils::isspace_s);
  return static_cast<std::size_t>(std::distance(content, it));
}
#endif

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

[[nodiscard]] inline boost::optional<symbol_t>
try_match_keyword(boost::string_view ident) {
  static constexpr auto keywords = std::array{
      std::pair(boost::string_view("call"), symbol_t::call),
      std::pair(boost::string_view("odd"), symbol_t::odd),
      std::pair(boost::string_view("begin"), symbol_t::begin),
      std::pair(boost::string_view("end"), symbol_t::end),
      std::pair(boost::string_view("if"), symbol_t::if_),
      std::pair(boost::string_view("while"), symbol_t::while_),
      std::pair(boost::string_view("then"), symbol_t::then),
      std::pair(boost::string_view("do"), symbol_t::do_),
      std::pair(boost::string_view("const"), symbol_t::const_),
      std::pair(boost::string_view("var"), symbol_t::var),
      std::pair(boost::string_view("procedure"), symbol_t::proc),
  };
  if (auto const* const it =
          std::find_if(keywords.cbegin(), keywords.cend(),
                       [ident](auto const& p) { return p.first == ident; });
      it != keywords.cend()) {
    return it->second;
  }
  return boost::none;
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

  auto tokens = tokens_t();

  auto const add_single_char_token =
      [&tokens, content = content](std::size_t cur_pos,
                                   auto sym) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], 1}, sym);
    return 1U;
  };
  auto const add_two_chars_token =
      [&tokens, content = content](std::size_t cur_pos,
                                   auto sym) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], 2}, sym);
    return 2U;
  };
  auto const add_number_token =
      [&tokens, content = content](std::size_t cur_pos,
                                   boost::string_view num) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], num.size()},
                        symbol_t::number);
    return num.size();
  };
  auto const add_ident_or_keyword_token =
      [&tokens, content = content](std::size_t cur_pos,
                                   boost::string_view ident) -> std::size_t {
    auto const sym = [ident] {
      if (auto const possible_keyword = try_match_keyword(ident);
          possible_keyword) {
        return *possible_keyword;
      }
      return symbol_t::ident;
    }();
    tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()}, sym);
    return ident.size();
  };

  std::size_t cur_pos = 0;
  while (auto const pc = peek_next_char(content, size, cur_pos)) {
    if (!pc.has_value()) {
      break;
    }
    char const c = *pc;
    switch (c) {
    case ';':
      cur_pos += add_single_char_token(cur_pos, symbol_t::semicolon);
      break;
    case ',':
      cur_pos += add_single_char_token(cur_pos, symbol_t::comma);
      break;
    case '(':
      cur_pos += add_single_char_token(cur_pos, symbol_t::lparen);
      break;
    case ')':
      cur_pos += add_single_char_token(cur_pos, symbol_t::rparen);
      break;
    case '*':
      cur_pos += add_single_char_token(cur_pos, symbol_t::times);
      break;
    case '/':
      cur_pos += add_single_char_token(cur_pos, symbol_t::divide);
      break;
    case '+':
      cur_pos += add_single_char_token(cur_pos, symbol_t::plus);
      break;
    case '-':
      cur_pos += add_single_char_token(cur_pos, symbol_t::minus);
      break;
    case '=':
      cur_pos += add_single_char_token(cur_pos, symbol_t::equal);
      break;
    case '#':
      cur_pos += add_single_char_token(cur_pos, symbol_t::not_equal);
      break;
    case '?':
      cur_pos += add_single_char_token(cur_pos, symbol_t::in);
      break;
    case '!':
      cur_pos += add_single_char_token(cur_pos, symbol_t::out);
      break;
    case '.':
      cur_pos += add_single_char_token(cur_pos, symbol_t::period);
      break;
    case '<': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        cur_pos += add_two_chars_token(cur_pos, symbol_t::less_equal);
      } else {
        cur_pos += add_single_char_token(cur_pos, symbol_t::less);
      }
      break;
    }
    case '>': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        cur_pos += add_two_chars_token(cur_pos, symbol_t::greater_equal);
      } else {
        cur_pos += add_single_char_token(cur_pos, symbol_t::greater);
      }
      break;
    }
    case ':': {
      if (auto const nc = peek_next_char(content, size, cur_pos + 1);
          nc.has_value() && *nc == '=') {
        cur_pos += add_two_chars_token(cur_pos, symbol_t::becomes);
      } else {
        fmt::print("unexpected char '{}', should it be ':='?\n", c);
        __builtin_unreachable();
      }
      break;
    }
    case '0' ... '9': {
      auto const num = get_number(content, size, cur_pos);
      cur_pos += add_number_token(cur_pos, num);
      break;
    }
    case 'a' ... 'z':
    case 'A' ... 'Z': {
      auto const ident = get_identifier(content, size, cur_pos);
      cur_pos += add_ident_or_keyword_token(cur_pos, ident);
      break;
    }
    default:
      if (!utils::isspace_s(c)) {
        fmt::print("unknown char '{}'\n", c);
      }
      ++cur_pos;
      break;
    }
  }
  return tokens;
}
} // namespace detail

[[nodiscard]] inline lex_result_t lex(std::string const& path) {
  return detail::lex(path, detail::read);
}
} // namespace lexer

#endif
