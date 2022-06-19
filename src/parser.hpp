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

#include <initializer_list>
#include <iostream>
#include <optional>

template <typename T>
concept loggable = requires(const T& v) {
  std::cerr << v;
};

namespace internal {
void error(const loggable auto& s) { std::cerr << s; }

void next_sym() {}
} // namespace internal

struct parser_t {
private:
  std::optional<lexer::symbol_t> expect(lexer::symbol_t s) {
    if (_sym == s) {
      internal::next_sym();
      return s;
    }
    return std::nullopt;
  }

  std::optional<lexer::symbol_t> expect_any_of(std::initializer_list<lexer::symbol_t> ss) {
    if (auto const* it = std::find(ss.begin(), ss.end(), _sym); it != ss.end()) {
      internal::next_sym();
      return *it;
    }
    return std::nullopt;
  }

  std::optional<lexer::symbol_t> must_be(lexer::symbol_t s) {
    if (expect(s)) {
      return s;
    }
    internal::error("unexpected symbol");
    return std::nullopt;
  }

  std::optional<lexer::symbol_t> must_be_any_of(std::initializer_list<lexer::symbol_t> ss) {
    if (auto const ret = expect_any_of(ss); ret) {
      return ret;
    }
    internal::error("unexpected symbol");
    return std::nullopt;
  }

  void factor() {
    if (expect(lexer::symbol_t::ident)) {
      ;
    } else if (expect(lexer::symbol_t::number)) {
      ;
    } else if (expect(lexer::symbol_t::lparen)) {
      expression();
      must_be(lexer::symbol_t::rparen);
    } else {
      internal::error("factor: syntax error");
      internal::next_sym();
    }
  }

  void term() {
    factor();
    while (expect_any_of({lexer::symbol_t::times, lexer::symbol_t::divide})) {
      internal::next_sym();
      factor();
    }
  }

  void expression() {
    auto const expect_plus_or_minus = [this] { return expect_any_of({lexer::symbol_t::plus, lexer::symbol_t::minus}); };

    if (expect_plus_or_minus()) {
      internal::next_sym();
    }
    term();
    while (expect_plus_or_minus()) {
      internal::next_sym();
      term();
    }
  }

  void condition() {
    if (expect(lexer::symbol_t::odd)) {
      expression();
    } else {
      expression();
      if (must_be_any_of({lexer::symbol_t::equal, lexer::symbol_t::not_equal, lexer::symbol_t::less,
                          lexer::symbol_t::less_equal, lexer::symbol_t::greater_equal, lexer::symbol_t::greater})) {
        internal::next_sym();
        expression();
      }
    }
  }

  void statement() {
    if (expect(lexer::symbol_t::ident)) {
      if (must_be(lexer::symbol_t::becomes)) {
        expression();
      }
    } else if (expect(lexer::symbol_t::call)) {
      must_be(lexer::symbol_t::ident);
    } else if (expect(lexer::symbol_t::in)) {
      must_be(lexer::symbol_t::ident);
    } else if (expect(lexer::symbol_t::out)) {
      expression();
    } else if (expect(lexer::symbol_t::begin)) {
      do {
        statement();
      } while (expect(lexer::symbol_t::semicolon));
      must_be(lexer::symbol_t::end);
    } else if (expect(lexer::symbol_t::if_)) {
      condition();
      must_be(lexer::symbol_t::then);
      statement();
    } else if (expect(lexer::symbol_t::while_)) {
      condition();
      must_be(lexer::symbol_t::do_);
      statement();
    }
  }

  void block() {
    if (expect(lexer::symbol_t::const_)) {
      do {
        must_be(lexer::symbol_t::ident);
        must_be(lexer::symbol_t::equal);
        must_be(lexer::symbol_t::number);
      } while (expect(lexer::symbol_t::comma));
      must_be(lexer::symbol_t::semicolon);
    }
    if (expect(lexer::symbol_t::var)) {
      do {
        must_be(lexer::symbol_t::ident);
      } while (expect(lexer::symbol_t::comma));
      must_be(lexer::symbol_t::semicolon);
    }
    while (expect(lexer::symbol_t::proc)) {
      must_be(lexer::symbol_t::ident);
      must_be(lexer::symbol_t::comma);
      block();
      must_be(lexer::symbol_t::comma);
    }
    statement();
    must_be(lexer::symbol_t::period);
  }
  lexer::symbol_t _sym;
};

#endif
