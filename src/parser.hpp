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

#include <iostream>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace parser {
struct parse_error_ok_t {};
struct parse_error_empty_file_t {};

using parse_error_t = std::variant<parse_error_ok_t, parse_error_empty_file_t>;

namespace internal {
template <typename T, typename... Ts> void error(T&& v, Ts&&... vs) {
  std::cerr << utils::str::to_str(std::forward<T>(v), std::forward<Ts>(vs)...) << '\n';
  std::exit(1);
}

struct statement_t {
  virtual ~statement_t() = default;

  virtual void print() const = 0;
};

struct in_t : statement_t {
  explicit in_t(lexer::token_t const& ident) : m_ident(ident) {}

  void print() const override { std::cout << utils::str::to_str("?", m_ident); }

private:
  lexer::token_t m_ident;
};

[[nodiscard]] inline constexpr std::string_view sv(lexer::token_t const& token) noexcept {
  return {token.annotation.start, token.annotation.start + token.annotation.length};
}

struct const_t {
  const_t(lexer::token_t const& ident, lexer::token_t const& num) : m_ident(ident), m_num(num) {}

  void print() const { std::cout << utils::str::to_str("const ", sv(m_ident), " = ", sv(m_num)) << '\n'; }

private:
  lexer::token_t m_ident;
  lexer::token_t m_num;
};

struct program_t {
  std::vector<const_t> consts;
  std::vector<std::unique_ptr<statement_t>> statements;
};

inline void print(program_t const& program) {
  for (auto const& c : program.consts) {
    c.print();
  }
  for (auto const& s : program.statements) {
    s->print();
  }
}
} // namespace internal

struct parser_t {
  explicit parser_t(lexer::tokens_t tokens) noexcept : m_tokens(std::move(tokens)) {}

  parse_error_t parse() {
    if (m_tokens.empty()) {
      return parse_error_empty_file_t{};
    }

    parse_top_block();

    return parse_error_ok_t{};
  };

  void print() const { internal::print(m_program); }

private:
  lexer::tokens_t::size_type m_cur_pos = 0;

  void next() noexcept { ++m_cur_pos; }

  [[nodiscard]] std::optional<lexer::token_t> cur_token() const noexcept {
    if (m_cur_pos == m_tokens.size()) {
      return std::nullopt;
    }
    return m_tokens[m_cur_pos];
  }

  [[nodiscard]] bool try_with(lexer::symbol_t s) {
    auto const ct = cur_token();
    return ct && ct->symbol == s;
  }

  void must_be(lexer::symbol_t s) {
    if (!try_with(s)) {
      auto const ct = cur_token();
      if (ct.has_value()) {
        internal::error(utils::str::to_str("expected '", s, "', got '", *ct, " at somewhere"));
      } else {
        internal::error(utils::str::to_str("expected '", s, "', incomplete file"));
      }
    }
  }

  void parse_program() {
    parse_top_block();
    must_be(lexer::symbol_t::period);
  }

  void parse_top_block() {
    parse_top_level_consts();
    parse_top_level_statements();
  }

  void parse_top_level_consts() {
    if (!try_with(lexer::symbol_t::const_)) {
      return;
    }

    while (true) {
      next();
      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();

      next();
      must_be(lexer::symbol_t::equal);

      next();
      must_be(lexer::symbol_t::number);
      auto const num = *cur_token();

      m_program.consts.emplace_back(id, num);

      next();
      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        if (!try_with(lexer::symbol_t::const_)) {
          return;
        }
      } else {
        must_be(lexer::symbol_t::comma);
      }
    }
  }

  void parse_top_level_statements() {}

  lexer::tokens_t m_tokens;
  internal::program_t m_program;
};
} // namespace parser
#endif
