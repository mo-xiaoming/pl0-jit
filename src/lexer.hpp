#ifndef PL0_LEXER_HPP__
#define PL0_LEXER_HPP__

#include "annotation.hpp"
#include "utils.hpp"

#include <boost/algorithm/cxx11/find_if_not.hpp>
#include <boost/none.hpp>
#include <boost/optional/optional.hpp>
#include <boost/unordered/unordered_map.hpp>
#include <boost/utility/string_view.hpp>
#include <boost/variant2/variant.hpp>
#include <fmt/core.h>
#include <mio/mmap.hpp>

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

[[nodiscard]] constexpr boost::string_view stringify_symbol(symbol_t sym) noexcept {
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

inline std::ostream& operator<<(std::ostream& os, symbol_t sym) { return os << stringify_symbol(sym); }

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
using lex_result_t = boost::variant2::variant<tokens_t, lex_error_file_unreadable_t>;

namespace detail {
struct cursor_t {
  char const* content;
  std::size_t size;
  std::size_t cur_pos;
};

[[nodiscard]] inline boost::optional<char> peek_nth_char(cursor_t const& cursor, std::size_t nth) {
  if (cursor.cur_pos + nth >= cursor.size) {
    return boost::none;
  }
  return cursor.content[cursor.cur_pos + nth];
}

[[nodiscard]] inline boost::optional<char> peek_cur_char(cursor_t const& cursor) { return peek_nth_char(cursor, 0); }

// TODO(mx): need strong type
[[nodiscard]] inline bool try_match_nth_char(cursor_t const& cursor, std::size_t nth, char expected) {
  if (auto const pc = peek_nth_char(cursor, nth); pc) {
    return *pc == expected;
  }
  return false;
}

[[nodiscard]] inline boost::string_view get_number(cursor_t const& cursor) {
  auto const* const it =
      boost::algorithm::find_if_not(cursor.content + cursor.cur_pos, cursor.content + cursor.size, utils::isdigit_s);
  return {cursor.content + cursor.cur_pos, static_cast<std::size_t>(it - (cursor.content + cursor.cur_pos))};
}

[[nodiscard]] inline boost::optional<symbol_t> try_match_keyword(boost::string_view ident) {
  static auto const keywords = boost::unordered_map<boost::string_view, symbol_t>{
      {"call", symbol_t::call},    {"odd", symbol_t::odd},      {"begin", symbol_t::begin},    {"end", symbol_t::end},
      {"if", symbol_t::if_},       {"while", symbol_t::while_}, {"then", symbol_t::then},      {"do", symbol_t::do_},
      {"const", symbol_t::const_}, {"var", symbol_t::var},      {"procedure", symbol_t::proc},
  };
  if (auto const it = keywords.find(ident); it != keywords.cend()) {
    return it->second;
  }
  return boost::none;
}

[[nodiscard]] inline boost::string_view get_identifier(cursor_t const& cursor) {
  auto const* const it =
      boost::algorithm::find_if_not(cursor.content + cursor.cur_pos, cursor.content + cursor.size, utils::isident_s);
  return {cursor.content + cursor.cur_pos, static_cast<std::size_t>(it - (cursor.content + cursor.cur_pos))};
}

template <reader_concept T> [[nodiscard]] lex_result_t lex(boost::string_view path, T&& reader) {
  auto const possible_ret = std::forward<T>(reader)(path);
  if (!possible_ret.has_value()) {
    return lex_error_file_unreadable_t{path};
  }

  auto const [content, size] = *possible_ret;

  auto tokens = tokens_t();

  auto const add_single_char_token = [&tokens, content = content](std::size_t cur_pos, auto sym) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], 1}, sym);
    return 1U;
  };
  auto const add_two_chars_token = [&tokens, content = content](std::size_t cur_pos, auto sym) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], 2}, sym);
    return 2U;
  };
  auto const add_number_token = [&tokens, content = content](std::size_t cur_pos,
                                                             boost::string_view num) -> std::size_t {
    tokens.emplace_back(annotation_t{&content[cur_pos], num.size()}, symbol_t::number);
    return num.size();
  };
  auto const add_ident_or_keyword_token = [&tokens, content = content](std::size_t cur_pos,
                                                                       boost::string_view ident) -> std::size_t {
    auto const sym = [ident] {
      if (auto const possible_keyword = try_match_keyword(ident); possible_keyword) {
        return *possible_keyword;
      }
      return symbol_t::ident;
    }();
    tokens.emplace_back(annotation_t{&content[cur_pos], ident.size()}, sym);
    return ident.size();
  };

  static auto const single_char_to_token_map = boost::unordered_map<char, symbol_t>{
      {';', symbol_t::semicolon}, {',', symbol_t::comma},     {'(', symbol_t::lparen}, {')', symbol_t::rparen},
      {'*', symbol_t::times},     {'/', symbol_t::divide},    {'+', symbol_t::plus},   {'-', symbol_t::minus},
      {'=', symbol_t::equal},     {'#', symbol_t::not_equal}, {'?', symbol_t::in},     {'!', symbol_t::out},
      {'.', symbol_t::period},
  };

  cursor_t cursor{.content = content, .size = size, .cur_pos = 0};
  while (auto const pc = peek_cur_char(cursor)) {
    if (!pc.has_value()) {
      break;
    }
    char const c = *pc;
    if (auto const it = single_char_to_token_map.find(c); it != single_char_to_token_map.cend()) {
      cursor.cur_pos += add_single_char_token(cursor.cur_pos, it->second);
    } else if (c == '<') {
      if (try_match_nth_char(cursor, 1, '=')) {
        cursor.cur_pos += add_two_chars_token(cursor.cur_pos, symbol_t::less_equal);
      } else {
        cursor.cur_pos += add_single_char_token(cursor.cur_pos, symbol_t::less);
      }
    } else if (c == '>') {
      if (try_match_nth_char(cursor, 1, '=')) {
        cursor.cur_pos += add_two_chars_token(cursor.cur_pos, symbol_t::greater_equal);
      } else {
        cursor.cur_pos += add_single_char_token(cursor.cur_pos, symbol_t::greater);
      }
      break;
    } else if (c == ':') {
      if (try_match_nth_char(cursor, 1, '=')) {
        cursor.cur_pos += add_two_chars_token(cursor.cur_pos, symbol_t::becomes);
      } else {
        fmt::print("unexpected char '{}', should it be ':='?\n", c);
        __builtin_unreachable();
      }
    } else if (utils::isdigit_s(c)) {
      auto const num = get_number(cursor);
      cursor.cur_pos += add_number_token(cursor.cur_pos, num);
    } else if (utils::isalpha_s(c)) {
      auto const ident = get_identifier(cursor);
      cursor.cur_pos += add_ident_or_keyword_token(cursor.cur_pos, ident);
    } else {
      if (!utils::isspace_s(c)) {
        fmt::print("unknown char '{}'\n", c);
      }
      ++cursor.cur_pos;
    }
  }
  return tokens;
}
} // namespace detail

[[nodiscard]] inline lex_result_t lex(std::string const& path) { return detail::lex(path, detail::read); }
} // namespace lexer

#endif
