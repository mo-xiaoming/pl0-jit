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

#include <cassert>
#include <iostream>
#include <map>
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

template <typename T, typename... Ts> [[nodiscard]] std::string info_str(T&& v, Ts&&... vs) {
  return utils::str::to_str(std::forward<T>(v), std::forward<Ts>(vs)...);
}

struct statement_t {
  statement_t() = default;
  statement_t(statement_t const&) = default;
  statement_t(statement_t&&) noexcept = default;
  statement_t& operator=(statement_t const&) & = default;
  statement_t& operator=(statement_t&&) & noexcept = default;
  virtual ~statement_t() = default;

  [[nodiscard]] virtual std::string to_string() const = 0;
};

[[nodiscard]] inline constexpr std::string_view sv(lexer::token_t const& token) noexcept {
  return {token.annotation.start, token.annotation.start + token.annotation.length};
}

struct expression_t {
  expression_t() = default;
  expression_t(expression_t const&) = default;
  expression_t(expression_t&&) noexcept = default;
  expression_t& operator=(expression_t const&) & = default;
  expression_t& operator=(expression_t&&) & noexcept = default;
  virtual ~expression_t() = default;

  [[nodiscard]] virtual std::string to_string() const = 0;
};

struct expression_primary_t : expression_t {};

struct expression_binary_op_t : expression_t {
  expression_binary_op_t(lexer::token_t const& op, std::unique_ptr<const expression_t>&& lhs,
                         std::unique_ptr<const expression_t>&& rhs)
      : m_op(op), m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {}

  [[nodiscard]] std::string to_string() const override {
    return info_str('(', sv(m_op), ' ', m_lhs->to_string(), ' ', m_rhs->to_string(), ')');
  }

private:
  lexer::token_t m_op;
  std::unique_ptr<const expression_t> m_lhs;
  std::unique_ptr<const expression_t> m_rhs;
};

struct number_t : expression_primary_t {
  explicit number_t(lexer::token_t const& value) : m_value(value) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_value)); }

private:
  lexer::token_t m_value;
};

struct ident_t : expression_primary_t {
  explicit ident_t(lexer::token_t const& name) : m_name(name) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_name)); }

private:
  lexer::token_t m_name;
};

struct call_t : statement_t {
  explicit call_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const override { return info_str("call ", sv(m_ident)); }

private:
  lexer::token_t m_ident;
};

struct in_t : statement_t {
  explicit in_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const override { return info_str("?", sv(m_ident)); }

private:
  lexer::token_t m_ident;
};

struct out_t : statement_t {
  explicit out_t(std::unique_ptr<const expression_t>&& expression) : m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str("!", m_expression->to_string()); }

private:
  std::unique_ptr<const expression_t> m_expression;
};

struct becomes_t : statement_t {
  becomes_t(lexer::token_t const& name, std::unique_ptr<const expression_t>&& expression)
      : m_name(name), m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_name), ":=", m_expression->to_string()); }

private:
  lexer::token_t m_name;
  std::unique_ptr<const expression_t> m_expression;
};

struct const_t {
  const_t(lexer::token_t const& ident, lexer::token_t const& num) : m_ident(ident), m_num(num) {}

  [[nodiscard]] std::string to_string() const { return info_str("const ", sv(m_ident), " = ", sv(m_num)); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

private:
  lexer::token_t m_ident;
  lexer::token_t m_num;
};

struct var_t {
  explicit var_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const { return info_str("var ", sv(m_ident)); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

private:
  lexer::token_t m_ident;
};

struct program_t {
  std::vector<const_t> consts;
  std::vector<var_t> vars;
  std::vector<std::unique_ptr<const statement_t>> statements;

  friend std::ostream& operator<<(std::ostream& os, program_t const& program) {
    for (auto const& c : program.consts) {
      os << c.to_string() << '\n';
    }
    for (auto const& v : program.vars) {
      os << v.to_string() << '\n';
    }
    for (auto const& s : program.statements) {
      os << s->to_string() << '\n';
    }
    return os;
  }
};
} // namespace internal

struct parser_t {
  explicit parser_t(lexer::tokens_t tokens) noexcept : m_tokens(std::move(tokens)) {}

  parse_error_t parse() {
    if (m_tokens.empty()) {
      return parse_error_empty_file_t{};
    }

    parse_program();

    return parse_error_ok_t{};
  };

  friend std::ostream& operator<<(std::ostream& os, parser_t const& parser) { return os << parser.m_program; }

private:
  lexer::tokens_t::size_type m_cur_pos = 0;

  void next() noexcept { ++m_cur_pos; }

  [[nodiscard]] std::optional<const lexer::token_t> cur_token() const noexcept {
    if (m_cur_pos == m_tokens.size()) {
      return std::nullopt;
    }
    return m_tokens[m_cur_pos];
  }

  [[nodiscard]] bool try_with(lexer::symbol_t s) const noexcept {
    auto const ct = cur_token();
    return ct && ct->symbol == s;
  }

  [[nodiscard]] std::optional<const lexer::symbol_t> try_with_any_of(std::initializer_list<lexer::symbol_t> ss) {
    auto const p = [this](lexer::symbol_t s) {
      const auto ct = cur_token();
      return ct && ct->symbol == s;
    };
    if (auto const* const it = std::find_if(ss.begin(), ss.end(), p); it != ss.end()) {
      return *it;
    }
    return std::nullopt;
  }

  void must_be(lexer::symbol_t s) const noexcept {
    if (!try_with(s)) {
      auto const ct = cur_token();
      if (ct.has_value()) {
        internal::error(utils::str::to_str("expected '", s, "', got '", internal::sv(*ct), " at somewhere"));
      } else {
        internal::error(utils::str::to_str("expected '", s, "', incomplete file"));
      }
    }
  }

  struct environment_t {
    environment_t* parent;
    std::vector<internal::const_t>* consts;
    std::vector<internal::var_t>* vars;
  };

  void parse_program() {
    environment_t const top_env = {
        .parent = nullptr,
        .consts = &m_program.consts,
        .vars = &m_program.vars,
    };

    parse_top_block(top_env);
    must_be(lexer::symbol_t::period);
  }

  void parse_top_block(environment_t const& env) {
    parse_consts(env);
    parse_vars(env);
    parse_statements(env);
  }

  static std::optional<lexer::token_t> lookup_name(environment_t const& env, lexer::token_t const& name) {
    auto const find_name = [&name](auto const& container) -> std::optional<lexer::token_t> {
      if (auto const it =
              std::find_if(container.cbegin(), container.cend(),
                           [&name](auto const& e) { return internal::sv(name) == internal::sv(e.token()); });
          it != container.cend()) {
        return it->token();
      }
      return std::nullopt;
    };

    for (auto const* cur_env = &env; cur_env != nullptr; cur_env = env.parent) {
      if (auto const t = find_name(*cur_env->consts); t.has_value()) {
        return *t;
      }
      if (auto const t = find_name(*cur_env->vars); t.has_value()) {
        return *t;
      }
    }
    return std::nullopt;
  }

  void parse_consts(environment_t const& env) {
    if (!try_with(lexer::symbol_t::const_)) {
      return;
    }

    while (true) {
      next();
      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
        internal::error(id, " has been defined at ", *prev_define);
      }

      next();
      must_be(lexer::symbol_t::equal);

      next();
      must_be(lexer::symbol_t::number);
      auto const num = *cur_token();

      env.consts->emplace_back(id, num);

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

  void parse_vars(environment_t const& env) {
    if (!try_with(lexer::symbol_t::var)) {
      return;
    }

    while (true) {
      next();
      must_be(lexer::symbol_t::ident);
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
        internal::error(id, " has been defined at ", *prev_define);
      }

      env.vars->emplace_back(id);

      next();
      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        if (!try_with(lexer::symbol_t::var)) {
          return;
        }
      } else {
        must_be(lexer::symbol_t::comma);
      }
    }
  }

  internal::in_t parse_in(environment_t const& env) {
    next();

    must_be(lexer::symbol_t::ident);
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      internal::error(id, " has not been defined");
    }

    next();

    return internal::in_t{id};
  }

  internal::out_t parse_out(environment_t const& env) {
    next();

    return internal::out_t{parse_expression(env)};
  }

  internal::call_t parse_call(environment_t const& /*env*/) {
    next();

    must_be(lexer::symbol_t::ident);
    auto const id = *cur_token();
// TODO(mx): name should be defined in the scope. Do it in codegen phase?
#if 0
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      internal::error(id, " has not been defined");
    }
#endif

    next();

    return internal::call_t{id};
  }

  internal::becomes_t parse_becomes(environment_t const& env) {
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      internal::error(id, " has not been defined");
    }
    next();

    must_be(lexer::symbol_t::becomes);
    next();

    auto expr = parse_expression(env);

    return internal::becomes_t{id, std::move(expr)};
  }

  void parse_statements(environment_t const& env) {
    while (!try_with(lexer::symbol_t::period)) {
      if (try_with(lexer::symbol_t::in)) {
        m_program.statements.push_back(std::make_unique<const internal::in_t>(parse_in(env)));
      }
      if (try_with(lexer::symbol_t::call)) {
        m_program.statements.push_back(std::make_unique<const internal::call_t>(parse_call(env)));
      }
      if (try_with(lexer::symbol_t::out)) {
        m_program.statements.push_back(std::make_unique<const internal::out_t>(parse_out(env)));
      }
      if (try_with(lexer::symbol_t::ident)) {
        m_program.statements.push_back(std::make_unique<const internal::becomes_t>(parse_becomes(env)));
      }
    }
  }

  std::unique_ptr<const internal::expression_t> parse_expression(environment_t const& env) {
    std::vector<std::unique_ptr<const internal::expression_t>> expressions{};
    expressions.push_back(parse_expression_primary(env));
    std::vector<lexer::token_t> ops{};
    return parse_expression_precedence_climbing(env, expressions, ops, 0);
  }

  std::unique_ptr<const internal::expression_t> parse_expression_primary(environment_t const& env) {
    if (try_with(lexer::symbol_t::minus)) {
      next();
      auto ret = std::make_unique<const internal::expression_binary_op_t>(
          lexer::token_t{.symbol = lexer::symbol_t::times, .annotation = {.start = "*", .length = 1}},
          std::make_unique<const internal::number_t>(
              lexer::token_t{.symbol = lexer::symbol_t::number, .annotation = {.start = "-1", .length = 2}}),
          parse_expression_without_leading_sign(env));
      return ret;
    }
    if (try_with(lexer::symbol_t::plus)) {
      next();
    }
    return parse_expression_without_leading_sign(env);
  }

  std::unique_ptr<const internal::expression_t> parse_expression_without_leading_sign(environment_t const& env) {
    if (!try_with_any_of({lexer::symbol_t::number, lexer::symbol_t::ident, lexer::symbol_t::lparen}).has_value()) {
      if (cur_token().has_value()) {
        internal::error("expected '(', number or identifier, got ", internal::sv(*cur_token()));
      } else {
        internal::error("expected '(', number or identifier, got EOF");
      }
    }

    if (try_with(lexer::symbol_t::number)) {
      auto ret = std::make_unique<const internal::number_t>(*cur_token());
      next();
      return ret;
    }
    if (try_with(lexer::symbol_t::ident)) {
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
        internal::error(id, " has not been defined");
      }
      auto ret = std::make_unique<const internal::ident_t>(id);
      next();
      return ret;
    }
    if (try_with(lexer::symbol_t::lparen)) {
      next();
      auto ret = parse_expression(env);
      must_be(lexer::symbol_t::rparen);
      next();
      return ret;
    }
    __builtin_unreachable();
  }

  std::unique_ptr<const internal::expression_t>
  parse_expression_precedence_climbing(environment_t const& env,
                                       std::vector<std::unique_ptr<const internal::expression_t>>& expressions,
                                       std::vector<lexer::token_t>& ops, int precedence) {
    static std::map<lexer::symbol_t, int> const precedences = {
        {lexer::symbol_t::plus, 1},
        {lexer::symbol_t::minus, 1},
        {lexer::symbol_t::times, 2},
        {lexer::symbol_t::divide, 2},
    };

    while (cur_token().has_value()) {
      auto const it = precedences.find(cur_token()->symbol);
      if (it == precedences.cend()) {
        break;
      }

      if (precedence < it->second) {
        ops.push_back(*cur_token());
        next();
        expressions.push_back(parse_expression_without_leading_sign(env));
        precedence = it->second;
      } else if (precedence == it->second) {
        auto const op = ops.back();
        ops.back() = *cur_token();
        next();
        auto rhs = std::move(expressions.back());
        expressions.pop_back();
        auto lhs = std::move(expressions.back());
        expressions.back() =
            std::make_unique<const internal::expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
        expressions.push_back(parse_expression_without_leading_sign(env));
      } else {
        parse_expression_reduce_all(expressions, ops);
        precedence = it->second;
        ops.push_back(*cur_token());
        next();
        expressions.push_back(parse_expression_without_leading_sign(env));
      }
    }
    parse_expression_reduce_all(expressions, ops);
    return std::move(expressions[0]);
  }

  static void parse_expression_reduce_all(std::vector<std::unique_ptr<const internal::expression_t>>& expressions,
                                          std::vector<lexer::token_t>& ops) {
    // NOLINTNEXTLINE(hicpp-no-array-decay, cppcoreguidelines-pro-bounds-array-to-pointer-decay)
    assert(ops.size() + 1 == expressions.size());
    while (!ops.empty()) {
      auto rhs = std::move(expressions.back());
      expressions.pop_back();
      auto lhs = std::move(expressions.back());
      auto const op = ops.back();
      ops.pop_back();
      expressions.back() = std::make_unique<const internal::expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
    }
  }

  lexer::tokens_t m_tokens;
  internal::program_t m_program;
};
} // namespace parser
#endif
