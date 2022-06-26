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
#include <memory>
#include <optional>
#include <stack>
#include <variant>

namespace parser {
template <typename T>
concept loggable = requires(const T& v) {
  std::cerr << v;
};

namespace internal {
struct symbol_name_t {
  lexer::token_t token;
};

void error(const loggable auto& s) {
  std::cerr << s << '\n';
  std::exit(1);
}
void error(char const* s) {
  std::cerr << s << '\n';
  std::exit(1);
}
} // namespace internal

struct expr_ast_t {
  virtual ~expr_ast_t() = default;
};

struct number_ast_t : expr_ast_t {
  explicit number_ast_t(lexer::token_t const& token) : m_token(token) {}

private:
  lexer::token_t m_token;
};

struct ident_ast_t : expr_ast_t {
  explicit ident_ast_t(lexer::token_t const& token) : m_token(token) {}

private:
  lexer::token_t m_token;
};

struct binary_expr_op_t : expr_ast_t {
  binary_expr_op_t(lexer::token_t const& op_token, std::unique_ptr<expr_ast_t>&& lhs, std::unique_ptr<expr_ast_t>&& rhs)
      : m_op_token(op_token), m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {}

private:
  lexer::token_t m_op_token;
  std::unique_ptr<expr_ast_t> m_lhs;
  std::unique_ptr<expr_ast_t> m_rhs;
};

template <std::integral NumberType = int> std::optional<NumberType> make_a_number(lexer::token_t const& token) {
  NumberType value = 0;
  auto [ptr, ec] = std::from_chars(token.annotation.start, token.annotation.start + token.annotation.length, value);
  if (ec == std::errc()) {
    return value;
  }
  if (ec == std::errc::invalid_argument) {
    internal::error(utils::str::to_str(token.annotation.to_sv(), " is an invalid number"));
  } else if (ec == std::errc::result_out_of_range) {
    internal::error(utils::str::to_str(token.annotation.to_sv(), " is out of range"));
  }
  return std::nullopt;
}
#if 0
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
    internal::error("unknown symbol");
    return std::nullopt;
  }

  std::unique_ptr<expr_ast_t> factor() {
    if (try_with(lexer::symbol_t::ident)) {
      return std::make_unique<ident_ast_t>(*cur_token());
    }
    if (try_with(lexer::symbol_t::number)) {
      return std::make_unique<number_ast_t>(*cur_token());
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

  std::unique_ptr<expr_ast_t> term() {
    std::vector<expr_node_t> ret({factor()});
    ret.reserve(4);
    while (try_with_any_of({lexer::symbol_t::times, lexer::symbol_t::divide})) {
      next();
      ret.push_back(factor());
    }
    return ret;
  }

  std::vector<expr_node_t> expression() {
    auto const try_with_plus_or_minus = [this] {
      return try_with_any_of({lexer::symbol_t::plus, lexer::symbol_t::minus});
    };

    if (try_with_plus_or_minus()) {
      next();
    }
    std::vector<expr_node_t> ret({term()});
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
#endif

struct const_t {
  lexer::token_t token;
  int value;
};

struct var_t {
  lexer::token_t token;
};

struct procedure_t {
  lexer::token_t token;
  std::vector<const_t> consts;
  std::vector<var_t> vars;
  procedure_t* parent = nullptr;
  std::vector<procedure_t> sub_procedures;
};

struct expression_t {};

struct odd_condition_t {
  std::unique_ptr<expression_t> expression;
};

struct bin_op_condition_t {
  lexer::symbol_t op;
  std::unique_ptr<expression_t> lhs;
  std::unique_ptr<expression_t> rhs;
};

using condition_t = std::variant<odd_condition_t, binary_expr_op_t>;

struct becomes_t {
  lexer::token_t token;
  std::unique_ptr<expression_t> expression;
};

struct call_t {
  lexer::token_t token;
};

struct in_t {
  lexer::token_t token;
};

struct out_t {
  std::unique_ptr<expression_t> expression;
};

struct if_t {
  condition_t condition;
  std::vector<statement_t> statements;
};

using statement_t = std::variant<call_t, in_t, out_t, becomes_t>;

struct program_t {
  std::vector<const_t> consts;
  std::vector<var_t> vars;
  std::vector<procedure_t> procedures;
  std::vector<statement_t> statements;
};

struct parser_t {
  explicit parser_t(lexer::tokens_t tokens) noexcept : m_tokens(std::move(tokens)) {}

  void parse() {
    handle_block();
    must_be(lexer::symbol_t::period);
  }

private:
  void next() noexcept { ++m_cur_pos; }

  [[nodiscard]] std::optional<lexer::token_t> cur_token() const noexcept {
    if (m_cur_pos == m_tokens.size()) {
      return std::nullopt;
    }
    return m_tokens[m_cur_pos];
  }

  [[nodiscard]] bool try_with(lexer::symbol_t s) {
    auto const ct = cur_token();
    if (ct && ct->symbol == s) {
      next();
      return true;
    }
    return false;
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

  void handle_block() {
    if (try_with(lexer::symbol_t::const_)) {
      handle_consts();
    } else if (try_with(lexer::symbol_t::var)) {
      handle_vars();
    } else if (try_with(lexer::symbol_t::proc)) {
      handle_procedures();
    } else {
      handle_statements();
    }
  }

  void handle_consts() {
    while (true) {
      next();

      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();

      next();
      must_be(lexer::symbol_t::equal);

      next();
      must_be(lexer::symbol_t::number);

      auto const num = make_a_number(*cur_token());

      if (m_cur_scope == nullptr) {
        m_program.consts.emplace_back(id, num);
      } else {
        m_cur_scope->consts.emplace_back(id, num);
      }

      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        break;
      }
      must_be(lexer::symbol_t::comma);
    }
  }

  void handle_vars() {
    while (true) {
      next();

      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();

      if (m_cur_scope == nullptr) {
        m_program.vars.emplace_back(id);
      } else {
        m_cur_scope->vars.emplace_back(id);
      }

      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        break;
      }
      must_be(lexer::symbol_t::comma);
    }
  }

  void handle_procedures() {
    while (true) {
      if (!try_with(lexer::symbol_t::proc)) {
        break;
      }
      next();

      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();

      must_be(lexer::symbol_t::comma);

      if (m_cur_scope == nullptr) {
        m_program.procedures.emplace_back(id);
        m_cur_scope = &m_program.procedures.back();
      } else {
        m_cur_scope->sub_procedures.emplace_back(id);
        m_cur_scope->sub_procedures.back().parent = m_cur_scope;
        m_cur_scope = &m_cur_scope->sub_procedures.back();
      }

      handle_block();

      must_be(lexer::symbol_t::comma);
    }
  }

  std::vector<statement_t> handle_statements() {
    std::vector<statement_t> ret;
    if (try_with(lexer::symbol_t::call)) {
      next();
      ret.emplace_back(call_t{*cur_token()});
    }
    if (try_with(lexer::symbol_t::in)) {
      next();
      ret.emplace_back(in_t{*cur_token()});
    }
    if (try_with(lexer::symbol_t::out)) {
      next();
      ret.emplace_back(out_t{handle_expression()});
    }
    if (try_with(lexer::symbol_t::ident)) {
      auto const id = *cur_token();
      next();
      ret.emplace_back(becomes_t{.token = id, .expression = handle_expression()});
    }
    if (try_with(lexer::symbol_t::if_)) {
      next();
      auto const condition = handle_condition();
      must_be(lexer::symbol_t::then);
      auto const statements = handle_statements();
      ret.emplace_back()
    }
    return ret;
  }

  procedure_t* m_cur_scope = nullptr;
  program_t m_program;
  lexer::tokens_t m_tokens;
  lexer::tokens_t::size_type m_cur_pos = 0;
};
} // namespace parser
#endif
