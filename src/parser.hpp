#ifndef PL0_PARSER_HPP__
#define PL0_PARSER_HPP__

/*
program = block "." .

block =
    ["const" ident "=" num {"," ident "=" num} ";"]
    ["var" ident {"," ident} ";"]
    {"procedure" ident ";" block ";"} statement .

statement =
    ident ":=" expression
    | "call" ident
    | "?" ident
    | "!" expression
    | "begin" statement {";" statement } "end"
    | "if" condition "then" statement
    | "while" condition "do" statement .

condition =
    "odd" expression
    | expression ("="|"#"|"<"|"<="|">"|">=") expression .

expression = ["+"|"-"] term {("+"|"-") term} .

term = factor {("*"|"/") factor} .

factor =
    ident
    | number
    | "(" expression ")" .
*/

#include "lexer_symbols.hpp"
#include "utils/strings.hpp"

#include <charconv>
#include <initializer_list>
#include <iostream>
#include <optional>
#include <variant>

namespace parser {
template <typename T>
concept loggable = requires(const T& v) {
  std::cerr << v;
};

namespace internal {
void error(const loggable auto& s) {
  std::cerr << s << '\n';
  std::exit(1);
}
void error(char const* s) {
  std::cerr << s << '\n';
  std::exit(1);
}
} // namespace internal

struct block_t {};

struct statement_t {};

struct expression_t {};

struct condition_t {};

struct term_t {};
using terms_t = std::vector<term_t>;

struct ident_t {
  explicit ident_t(lexer::token_t const& token) : m_token(token) {}

private:
  lexer::token_t m_token;
};

struct number_t {
  friend std::optional<number_t> make_a_number(lexer::token_t const& token);

private:
  explicit number_t(int value) : m_value(value) {}
  int m_value;
};

std::optional<number_t> make_a_number(lexer::token_t const& token) {
  int value = 0;
  auto [ptr, ec] = std::from_chars(token.annotation.start, token.annotation.start + token.annotation.length, value);
  if (ec == std::errc()) {
    return number_t(value);
  }
  if (ec == std::errc::invalid_argument) {
    internal::error(utils::str::to_str(token.annotation.to_sv(), " is an invalid number"));
  } else if (ec == std::errc::result_out_of_range) {
    internal::error(utils::str::to_str(token.annotation.to_sv(), " is out of range"));
  }
  return std::nullopt;
}

using everything_t = std::variant<block_t, statement_t, expression_t, condition_t, ident_t, number_t, terms_t>;

struct root_t {
  everything_t everything;
};

struct parser_t {
  explicit parser_t(lexer::tokens_t tokens) noexcept : m_tokens(std::move(tokens)) {}

  void eval() { return block(); }

private:
  lexer::tokens_t m_tokens;
  std::size_t m_cur_token_idx = 0;

  void next() noexcept { ++m_cur_token_idx; }

  [[nodiscard]] constexpr std::optional<lexer::token_t> cur_token() const noexcept {
    if (m_cur_token_idx == m_tokens.size()) {
      return std::nullopt;
    }
    return m_tokens[m_cur_token_idx];
  }

  [[nodiscard]] bool try_with(lexer::symbol_t s) {
    auto const ct = cur_token();
    if (ct && ct->symbol == s) {
      next();
      return true;
    }
    return false;
  }

  std::optional<lexer::symbol_t> try_with_any_of(std::initializer_list<lexer::symbol_t> ss) {
    auto const p = [this](lexer::symbol_t s) {
      const auto ct = cur_token();
      return ct && ct->symbol == s;
    };
    if (auto const* const it = std::find_if(ss.begin(), ss.end(), p); it != ss.end()) {
      next();
      return *it;
    }
    return std::nullopt;
  }

  void must_be(lexer::symbol_t s) {
    if (!try_with(s)) {
      auto const ct = cur_token();
      if (ct.has_value()) {
        internal::error(utils::str::to_str("expected '", s, "', got '", *ct, " at somewhere"));
      } else {
        internal::error(utils::str::to_str("expected '", s, "', incomplete file"));
      }
      std::exit(1);
    }
  }

  std::optional<lexer::symbol_t> must_be_any_of(std::initializer_list<lexer::symbol_t> ss) {
    if (auto const ret = try_with_any_of(ss); ret) {
      return ret;
    }
    internal::error("untry_withed symbol");
    return std::nullopt;
  }

  everything_t factor() {
    if (try_with(lexer::symbol_t::ident)) {
      return ident_t(*cur_token());
    }
    if (try_with(lexer::symbol_t::number)) {
      return *make_a_number(*cur_token());
    }
    if (try_with(lexer::symbol_t::lparen)) {
      auto const ret = expression();
      must_be(lexer::symbol_t::rparen);
      return ret;
    }
    auto const ct = cur_token();
    if (ct.has_value()) {
      internal::error(utils::str::to_str("expected identifier, number or lparent, got ", ct->symbol, " instead"));
    } else {
      internal::error(utils::str::to_str("expected identifier, number or lparent, got nothing"));
    }
  }

  everything_t term() {
    std::vector<everything_t> ret({factor()});
    ret.reserve(4);
    while (try_with_any_of({lexer::symbol_t::times, lexer::symbol_t::divide})) {
      next();
      ret.push_back(factor());
    }
    return ret;
  }

  std::vector<everything_t> expression() {
    auto const try_with_plus_or_minus = [this] {
      return try_with_any_of({lexer::symbol_t::plus, lexer::symbol_t::minus});
    };

    if (try_with_plus_or_minus()) {
      next();
    }
    std::vector<everything_t> ret({term()});
    ret.reserve(4);
    while (try_with_plus_or_minus()) {
      next();
      ret.push_back(term());
    }
    return ret;
  }

  void condition() {
    if (try_with(lexer::symbol_t::odd)) {
      expression();
    } else {
      expression();
      if (must_be_any_of({lexer::symbol_t::equal, lexer::symbol_t::not_equal, lexer::symbol_t::less,
                          lexer::symbol_t::less_equal, lexer::symbol_t::greater_equal, lexer::symbol_t::greater})) {
        next();
        expression();
      }
    }
  }

  void statement() {
    if (try_with(lexer::symbol_t::ident)) {
      must_be(lexer::symbol_t::becomes);
      expression();
    } else if (try_with(lexer::symbol_t::call)) {
      must_be(lexer::symbol_t::ident);
    } else if (try_with(lexer::symbol_t::in)) {
      must_be(lexer::symbol_t::ident);
    } else if (try_with(lexer::symbol_t::out)) {
      expression();
    } else if (try_with(lexer::symbol_t::begin)) {
      do {
        statement();
      } while (try_with(lexer::symbol_t::semicolon));
      must_be(lexer::symbol_t::end);
    } else if (try_with(lexer::symbol_t::if_)) {
      condition();
      must_be(lexer::symbol_t::then);
      statement();
    } else if (try_with(lexer::symbol_t::while_)) {
      condition();
      must_be(lexer::symbol_t::do_);
      statement();
    }
  }

  void block() {
    if (try_with(lexer::symbol_t::const_)) {
      do {
        must_be(lexer::symbol_t::ident);
        must_be(lexer::symbol_t::equal);
        must_be(lexer::symbol_t::number);
      } while (try_with(lexer::symbol_t::comma));
      must_be(lexer::symbol_t::semicolon);
    }
    if (try_with(lexer::symbol_t::var)) {
      do {
        must_be(lexer::symbol_t::ident);
      } while (try_with(lexer::symbol_t::comma));
      must_be(lexer::symbol_t::semicolon);
    }
    while (try_with(lexer::symbol_t::proc)) {
      must_be(lexer::symbol_t::ident);
      must_be(lexer::symbol_t::comma);
      block();
      must_be(lexer::symbol_t::comma);
    }
    statement();
    must_be(lexer::symbol_t::period);
  }
};
} // namespace parser
#endif
