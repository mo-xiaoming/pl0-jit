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

#include <cstdlib>
#include <initializer_list>
#include <optional>

#include <spdlog/spdlog.h>

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

template <typename T>
concept loggable = requires(const T &v) {
  spdlog::info(v);
};

namespace detail {
void error(const loggable auto &s) {
  spdlog::critical(s);
  std::exit(EXIT_FAILURE);
}
inline void next_sym() {}
} // namespace detail

struct parser_t {

private:
  std::optional<symbol_t> expect(symbol_t s) {
    if (_sym == s) {
      detail::next_sym();
      return s;
    }
    return std::nullopt;
  }

  std::optional<symbol_t> expect_any_of(std::initializer_list<symbol_t> ss) {
    if (const auto *it = std::find(ss.begin(), ss.end(), _sym);
        it != ss.end()) {
      detail::next_sym();
      return *it;
    }
    return std::nullopt;
  }

  std::optional<symbol_t> must_be(symbol_t s) {
    if (expect(s)) {
      return s;
    }
    detail::error("unexpected symbol");
    return std::nullopt;
  }

  std::optional<symbol_t> must_be_any_of(std::initializer_list<symbol_t> ss) {
    if (const auto ret = expect_any_of(ss); ret) {
      return ret;
    }
    detail::error("unexpected symbol");
    return std::nullopt;
  }

  void factor() {
    if (expect(symbol_t::ident)) {
      ;
    } else if (expect(symbol_t::number)) {
      ;
    } else if (expect(symbol_t::lparen)) {
      expression();
      must_be(symbol_t::rparen);
    } else {
      detail::error("factor: syntax error");
      detail::next_sym();
    }
  }

  void term() {
    factor();
    while (expect_any_of({symbol_t::times, symbol_t::slash})) {
      detail::next_sym();
      factor();
    }
  }

  void expression() {
    const auto expect_plus_or_minus = [this] {
      return expect_any_of({symbol_t::plus, symbol_t::minus});
    };

    if (expect_plus_or_minus()) {
      detail::next_sym();
    }
    term();
    while (expect_plus_or_minus()) {
      detail::next_sym();
      term();
    }
  }

  void condition() {
    if (expect(symbol_t::oddsym)) {
      expression();
    } else {
      expression();
      if (must_be_any_of({symbol_t::eql, symbol_t::neq, symbol_t::lss,
                          symbol_t::leq, symbol_t::geq, symbol_t::gtr})) {
        detail::next_sym();
        expression();
      }
    }
  }

  void statement() {
    if (expect(symbol_t::ident)) {
      if (must_be(symbol_t::becomes)) {
        expression();
      }
    } else if (expect(symbol_t::callsym)) {
      must_be(symbol_t::ident);
    } else if (expect(symbol_t::insym)) {
      must_be(symbol_t::ident);
    } else if (expect(symbol_t::outsym)) {
      expression();
    } else if (expect(symbol_t::beginsym)) {
      do {
        statement();
      } while (expect(symbol_t::semicolon));
      must_be(symbol_t::endsym);
    } else if (expect(symbol_t::ifsym)) {
      condition();
      must_be(symbol_t::thensym);
      statement();
    } else if (expect(symbol_t::whilesym)) {
      condition();
      must_be(symbol_t::dosym);
      statement();
    }
  }

  void block() {
    if (expect(symbol_t::constsym)) {
      do {
        must_be(symbol_t::ident);
        must_be(symbol_t::eql);
        must_be(symbol_t::number);
      } while (expect(symbol_t::comma));
      must_be(symbol_t::semicolon);
    }
    if (expect(symbol_t::varsym)) {
      do {
        must_be(symbol_t::ident);
      } while (expect(symbol_t::comma));
      must_be(symbol_t::semicolon);
    }
    while (expect(symbol_t::procsym)) {
      must_be(symbol_t::ident);
      must_be(symbol_t::comma);
      block();
      must_be(symbol_t::comma);
    }
    statement();
    must_be(symbol_t::period);
  }
  symbol_t _sym;
};