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
namespace internal {
[[nodiscard]] inline constexpr std::string_view sv(lexer::token_t const& token) noexcept {
  return to_sv(token.annotation);
}
} // namespace internal

struct parse_error_ok_t {};
struct parse_error_empty_file_t {};
struct parse_error_early_eof_t {
  std::vector<lexer::symbol_t> expected;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_early_eof_t const& pe) {
    os << "expected ";
    std::string sep;
    for (auto const i : pe.expected) {
      os << sep << i;
      sep = ", ";
    }
    return os << ", but got eof";
  }
};
struct parse_error_unexpected_t {
  std::vector<lexer::symbol_t> expected;
  lexer::token_t got;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_unexpected_t const& pe) {
    os << "expected ";
    std::string sep;
    for (auto const i : pe.expected) {
      os << sep << i;
      sep = ", ";
    }
    os << ", but got " << internal::sv(pe.got) << '\n';
    return os << annotation_to_error_string(pe.got.annotation) << '\n';
  }
};
struct parse_error_name_redefined_t {
  lexer::token_t pre_defined;
  lexer::token_t cur_defined;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_name_redefined_t const& pe) {
    return os << internal::sv(pe.cur_defined) << " previously defined at\n"
              << annotation_to_error_string(pe.pre_defined.annotation) << "redefined at\n"
              << annotation_to_error_string(pe.cur_defined.annotation);
  }
};
struct parse_error_name_undefined_t {
  lexer::token_t name;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_name_undefined_t const& pe) {
    return os << internal::sv(pe.name) << " is undefined" << '\n'
              << annotation_to_error_string(pe.name.annotation) << '\n';
  }
};
using parse_error_t =
    std::variant<parse_error_ok_t, parse_error_empty_file_t, parse_error_early_eof_t, parse_error_unexpected_t,
                 parse_error_name_redefined_t, parse_error_name_undefined_t>;

template <class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;
[[maybe_unused]] inline std::ostream& operator<<(std::ostream& os, parse_error_t const& pe) {
  return std::visit(
      overloaded{
          [&os](parse_error_ok_t const&) -> std::ostream& { return os << "parse_error_ok_t"; },
          [&os](parse_error_empty_file_t const&) -> std::ostream& { return os << "parse_error_empty_file_t"; },
          [&os](parse_error_early_eof_t const& e) -> std::ostream& { return os << "parse_error_early_eof_t: " << e; },
          [&os](parse_error_unexpected_t const& e) -> std::ostream& { return os << "parse_error_unexpected_t: " << e; },
          [&os](parse_error_name_redefined_t const& e) -> std::ostream& {
            return os << "parse_error_name_redefined_t: " << e;
          },
          [&os](parse_error_name_undefined_t const& e) -> std::ostream& {
            return os << "parse_error_name_undefined_t: " << e;
          },
      },
      pe);
}

[[nodiscard]] inline bool has_parse_error(parse_error_t const& error) noexcept {
  return !std::holds_alternative<parse_error_ok_t>(error);
}

namespace internal {
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

struct condition_t {
  condition_t() = default;
  condition_t(condition_t const&) = default;
  condition_t(condition_t&&) noexcept = default;
  condition_t& operator=(condition_t const&) & = default;
  condition_t& operator=(condition_t&&) & noexcept = default;
  virtual ~condition_t() = default;

  [[nodiscard]] virtual std::string to_string() const = 0;
};

struct odd_condition_t : condition_t {
  explicit odd_condition_t(std::unique_ptr<const expression_t>&& expression) : m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str("odd ", m_expression->to_string()); }

private:
  std::unique_ptr<const expression_t> m_expression;
};

struct cmp_condition_t : condition_t {
  explicit cmp_condition_t(lexer::token_t const& op, std::unique_ptr<const expression_t>&& lhs,
                           std::unique_ptr<const expression_t>&& rhs)
      : m_op(op), m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {}

  [[nodiscard]] std::string to_string() const override {
    return info_str(m_lhs->to_string(), sv(m_op), m_rhs->to_string());
  }

private:
  lexer::token_t m_op;
  std::unique_ptr<const expression_t> m_lhs;
  std::unique_ptr<const expression_t> m_rhs;
};

struct if_then_t : statement_t {
  if_then_t(std::unique_ptr<const condition_t>&& condition, std::unique_ptr<const statement_t>&& statement)
      : m_condition(std::move(condition)), m_statement(std::move(statement)) {}

  [[nodiscard]] std::string to_string() const override {
    return info_str("if ", m_condition->to_string(), " then ", m_statement->to_string());
  }

private:
  std::unique_ptr<const condition_t> m_condition;
  std::unique_ptr<const statement_t> m_statement;
};

struct while_do_t : statement_t {
  while_do_t(std::unique_ptr<const condition_t>&& condition, std::unique_ptr<const statement_t>&& statement)
      : m_condition(std::move(condition)), m_statement(std::move(statement)) {}

  [[nodiscard]] std::string to_string() const override {
    return info_str("while ", m_condition->to_string(), " do ", m_statement->to_string());
  }

private:
  std::unique_ptr<const condition_t> m_condition;
  std::unique_ptr<const statement_t> m_statement;
};

struct begin_end_t : statement_t {
  explicit begin_end_t(std::vector<std::unique_ptr<const statement_t>>&& statements)
      : m_statements(std::move(statements)) {}

  [[nodiscard]] std::string to_string() const override {
    std::ostringstream oss;
    oss << "begin\n";
    for (auto const& s : m_statements) {
      oss << s->to_string() << '\n';
    }
    oss << "end";
    return std::move(oss).str();
  }

private:
  std::vector<std::unique_ptr<const statement_t>> m_statements;
};

struct const_t {
  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  const_t(lexer::token_t const& ident, lexer::token_t const& num) : m_ident(ident), m_num(num) {}

  [[nodiscard]] std::string to_string() const { return info_str("const ", sv(m_ident), "=", sv(m_num)); }

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

struct environment_t;
struct procedure_t {
  procedure_t(lexer::token_t const& ident, environment_t&& env)
      : m_ident(ident), m_env(std::make_unique<environment_t>(std::move(env))) {}

  [[nodiscard]] std::string to_string() const { return info_str("procedure ", sv(m_ident), ';', *m_env, ';'); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

private:
  lexer::token_t m_ident;
  std::unique_ptr<environment_t> m_env;
};

struct environment_t {
  environment_t const* parent = nullptr;
  lexer::token_t ident;
  std::vector<const_t> consts;
  std::vector<var_t> vars;
  std::vector<procedure_t> procedures;
  std::vector<std::unique_ptr<const statement_t>> statements;

  friend std::ostream& operator<<(std::ostream& os, environment_t const& program) {
    for (auto const& c : program.consts) {
      os << c.to_string() << '\n';
    }
    for (auto const& v : program.vars) {
      os << v.to_string() << '\n';
    }
    for (auto const& p : program.procedures) {
      os << p.to_string() << '\n';
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

  [[nodiscard]] parse_error_t parse() {
    if (m_tokens.empty()) {
      return parse_error_empty_file_t{};
    }

    return parse_program();
  };

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parser_t const& parser) {
    return os << parser.m_top_env;
  }

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

  [[nodiscard]] parse_error_t must_be_any_of(std::initializer_list<lexer::symbol_t> ss) {
    if (try_with_any_of(ss)) {
      return parse_error_ok_t{};
    }

    auto const ct = cur_token();
    if (ct.has_value()) {
      return parse_error_unexpected_t{.expected = {ss}, .got = *ct};
    }
    return parse_error_early_eof_t{.expected = {ss}};
  }

  [[nodiscard]] parse_error_t must_be(lexer::symbol_t s) const noexcept {
    if (try_with(s)) {
      return parse_error_ok_t{};
    }

    auto const ct = cur_token();
    if (ct.has_value()) {
      return parse_error_unexpected_t{.expected = {s}, .got = *ct};
    }
    return parse_error_early_eof_t{.expected = {s}};
  }

  [[nodiscard]] parse_error_t parse_program() {
    if (auto pe = parse_block(m_top_env); has_parse_error(pe)) {
      return pe;
    }
    return must_be(lexer::symbol_t::period);
  }

  [[nodiscard]] parse_error_t parse_block(internal::environment_t& env) {
    if (auto pe = parse_consts(env); has_parse_error(pe)) {
      return pe;
    }
    if (auto pe = parse_vars(env); has_parse_error(pe)) {
      return pe;
    }
    if (auto pe = parse_procedures(env); has_parse_error(pe)) {
      return pe;
    }
    auto ret = parse_statements(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    env.statements = std::move(std::get<std::vector<std::unique_ptr<const internal::statement_t>>>(ret));
    return parse_error_ok_t{};
  }

  [[nodiscard]] static std::optional<lexer::token_t> lookup_name(internal::environment_t const& env,
                                                                 lexer::token_t const& name) {
    auto const find_name = [&name](auto const& container) -> std::optional<lexer::token_t> {
      if (auto const it =
              std::find_if(container.cbegin(), container.cend(),
                           [&name](auto const& e) { return internal::sv(name) == internal::sv(e.token()); });
          it != container.cend()) {
        return it->token();
      }
      return std::nullopt;
    };

    for (auto const* cur_env = &env; cur_env != nullptr; cur_env = cur_env->parent) {
      if (auto const t = find_name(cur_env->consts); t.has_value()) {
        return *t;
      }
      if (auto const t = find_name(cur_env->vars); t.has_value()) {
        return *t;
      }
      if (cur_env->parent != nullptr && cur_env->ident == name) {
        return cur_env->ident;
      }
      if (auto const it =
              std::find_if(cur_env->procedures.cbegin(), cur_env->procedures.cend(),
                           [&name](auto const& p) { return internal::sv(p.token()) == internal::sv(name); });
          it != cur_env->procedures.cend()) {
        return it->token();
      }
    }
    return std::nullopt;
  }

  [[nodiscard]] parse_error_t parse_consts(internal::environment_t& env) {
    if (!try_with(lexer::symbol_t::const_)) {
      return parse_error_ok_t{};
    }

    while (true) {
      next();
      if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
        return pe;
      }
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
        return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
      }

      next();
      if (auto pe = must_be(lexer::symbol_t::equal); has_parse_error(pe)) {
        return pe;
      }

      next();
      if (auto pe = must_be(lexer::symbol_t::number); has_parse_error(pe)) {
        return pe;
      }
      auto const num = *cur_token();

      env.consts.emplace_back(id, num);

      next();
      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        if (!try_with(lexer::symbol_t::const_)) {
          return parse_error_ok_t{};
        }
      } else {
        if (auto pe = must_be(lexer::symbol_t::comma); has_parse_error(pe)) {
          return pe;
        }
      }
    }

    return parse_error_ok_t{};
  }

  [[nodiscard]] parse_error_t parse_vars(internal::environment_t& env) {
    if (!try_with(lexer::symbol_t::var)) {
      return parse_error_ok_t{};
    }

    while (true) {
      next();
      if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
        return pe;
      }
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
        return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
      }

      env.vars.emplace_back(id);

      next();
      if (try_with(lexer::symbol_t::semicolon)) {
        next();
        if (!try_with(lexer::symbol_t::var)) {
          return parse_error_ok_t{};
        }
      } else {
        if (auto pe = must_be(lexer::symbol_t::comma); has_parse_error(pe)) {
          return pe;
        }
      }
    }

    return parse_error_ok_t{};
  }

  [[nodiscard]] parse_error_t parse_procedures(internal::environment_t& env) {
    while (try_with(lexer::symbol_t::proc)) {
      next();
      if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
        return pe;
      }
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); prev_define.has_value()) {
        return parse_error_name_redefined_t{.pre_defined = *prev_define, .cur_defined = id};
      }
      next();

      if (auto pe = must_be(lexer::symbol_t::semicolon); has_parse_error(pe)) {
        return pe;
      }
      next();

      internal::environment_t block_env{};
      block_env.parent = &env;
      block_env.ident = id;
      if (auto pe = parse_block(block_env); has_parse_error(pe)) {
        return pe;
      }

      if (auto pe = must_be(lexer::symbol_t::semicolon); has_parse_error(pe)) {
        return pe;
      }
      next();

      env.procedures.emplace_back(id, std::move(block_env));
    }
    return parse_error_ok_t{};
  }

  [[nodiscard]] std::variant<internal::in_t, parse_error_t> parse_in(internal::environment_t const& env) {
    next();

    if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
      return pe;
    }
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      return parse_error_name_undefined_t{.name = id};
    }

    next();

    return internal::in_t{id};
  }

  [[nodiscard]] std::variant<internal::out_t, parse_error_t> parse_out(internal::environment_t const& env) {
    next();

    auto ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return internal::out_t{std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret))};
  }

  [[nodiscard]] std::variant<internal::call_t, parse_error_t> parse_call(internal::environment_t const& /*env*/) {
    next();

    if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
      return pe;
    }
    auto const id = *cur_token();
// TODO(mx): name should be defined in the scope. Do it in codegen phase?
#if 0
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      return parse_error_name_undefined_t{.name=id};
    }
#endif

    next();

    return internal::call_t{id};
  }

  [[nodiscard]] std::variant<internal::becomes_t, parse_error_t> parse_becomes(internal::environment_t const& env) {
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      return parse_error_name_undefined_t{.name = id};
    }
    next();

    if (auto pe = must_be(lexer::symbol_t::becomes); has_parse_error(pe)) {
      return pe;
    }
    next();

    auto expr = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&expr); pe != nullptr) {
      return *pe;
    }

    return internal::becomes_t{id, std::move(std::get<std::unique_ptr<const internal::expression_t>>(expr))};
  }

  [[nodiscard]] std::variant<internal::begin_end_t, parse_error_t> parse_begin_end(internal::environment_t const& env) {
    std::vector<std::unique_ptr<const internal::statement_t>> statements;
    std::vector<std::unique_ptr<const internal::statement_t>> sub_statements;
    do {
      next();
      auto ret = parse_statements(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      sub_statements = std::move(std::get<std::vector<std::unique_ptr<const internal::statement_t>>>(ret));
      statements.insert(statements.end(), std::make_move_iterator(sub_statements.begin()),
                        std::make_move_iterator(sub_statements.end()));
    } while (try_with(lexer::symbol_t::semicolon));
    if (auto pe = must_be(lexer::symbol_t::end); has_parse_error(pe)) {
      return pe;
    }
    next();

    return internal::begin_end_t{std::move(statements)};
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::condition_t>, parse_error_t>
  parse_condition(internal::environment_t const& env) {
    next();
    if (try_with(lexer::symbol_t::odd)) {
      next();
      auto ret = parse_expression(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::odd_condition_t>(
          std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret)));
    }
    auto ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    auto lhs = std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret));

    if (auto pe = must_be_any_of({lexer::symbol_t::equal, lexer::symbol_t::not_equal, lexer::symbol_t::less_equal,
                                  lexer::symbol_t::less, lexer::symbol_t::greater_equal, lexer::symbol_t::greater});
        has_parse_error(pe)) {
      return pe;
    }
    auto const op = *cur_token();
    next();

    ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    auto rhs = std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret));

    return std::make_unique<const internal::cmp_condition_t>(op, std::move(lhs), std::move(rhs));
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::statement_t>, parse_error_t>
  parse_statement(internal::environment_t const& env) {
    if (try_with(lexer::symbol_t::in)) {
      auto ret = parse_in(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::in_t>(std::move(std::get<internal::in_t>(ret)));
    }
    if (try_with(lexer::symbol_t::call)) {
      auto ret = parse_call(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::call_t>(std::move(std::get<internal::call_t>(ret)));
    }
    if (try_with(lexer::symbol_t::out)) {
      auto ret = parse_out(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::out_t>(std::move(std::get<internal::out_t>(ret)));
    }
    if (try_with(lexer::symbol_t::ident)) {
      auto ret = parse_becomes(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::becomes_t>(std::move(std::get<internal::becomes_t>(ret)));
    }
    if (try_with(lexer::symbol_t::begin)) {
      auto ret = parse_begin_end(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::begin_end_t>(std::move(std::get<internal::begin_end_t>(ret)));
    }
    if (try_with(lexer::symbol_t::if_)) {
      auto ret = parse_if_then(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::if_then_t>(std::move(std::get<internal::if_then_t>(ret)));
    }
    if (try_with(lexer::symbol_t::while_)) {
      auto ret = parse_while_do(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::while_do_t>(std::move(std::get<internal::while_do_t>(ret)));
    }
    __builtin_unreachable();
  }

  [[nodiscard]] std::variant<internal::if_then_t, parse_error_t> parse_if_then(internal::environment_t const& env) {
    auto condition = parse_condition(env);
    if (auto const* pe = std::get_if<parse_error_t>(&condition); pe != nullptr) {
      return *pe;
    }

    if (auto const pe = must_be(lexer::symbol_t::then); has_parse_error(pe)) {
      return pe;
    }
    next();

    auto statement = parse_statement(env);
    if (auto const* pe = std::get_if<parse_error_t>(&statement); pe != nullptr) {
      return *pe;
    }

    return internal::if_then_t{std::move(std::get<std::unique_ptr<const internal::condition_t>>(condition)),
                               std::move(std::get<std::unique_ptr<const internal::statement_t>>(statement))};
  }

  [[nodiscard]] std::variant<internal::while_do_t, parse_error_t> parse_while_do(internal::environment_t const& env) {
    auto condition = parse_condition(env);
    if (auto const* pe = std::get_if<parse_error_t>(&condition); pe != nullptr) {
      return *pe;
    }

    if (auto const pe = must_be(lexer::symbol_t::do_); has_parse_error(pe)) {
      return pe;
    }
    next();

    auto statement = parse_statement(env);
    if (auto const* pe = std::get_if<parse_error_t>(&statement); pe != nullptr) {
      return *pe;
    }

    return internal::while_do_t{std::move(std::get<std::unique_ptr<const internal::condition_t>>(condition)),
                                std::move(std::get<std::unique_ptr<const internal::statement_t>>(statement))};
  }

  [[nodiscard]] std::variant<std::vector<std::unique_ptr<const internal::statement_t>>, parse_error_t>
  parse_statements(internal::environment_t const& env) {
    std::vector<std::unique_ptr<const internal::statement_t>> statements;
    while (true) {
      if (!try_with_any_of({lexer::symbol_t::in, lexer::symbol_t::call, lexer::symbol_t::out, lexer::symbol_t::ident,
                            lexer::symbol_t::begin, lexer::symbol_t::if_, lexer::symbol_t::while_})
               .has_value()) {
        break;
      }
      auto ret = parse_statement(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      statements.push_back(std::move(std::get<std::unique_ptr<const internal::statement_t>>(ret)));
    }
    return statements;
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::expression_t>, parse_error_t>
  parse_expression(internal::environment_t const& env) {
    std::vector<std::unique_ptr<const internal::expression_t>> expressions{};
    auto ret = parse_expression_primary(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    expressions.push_back(std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret)));
    std::vector<lexer::token_t> ops{};
    return parse_expression_precedence_climbing(env, expressions, ops, 0);
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::expression_t>, parse_error_t>
  parse_expression_primary(internal::environment_t const& env) {
    if (try_with(lexer::symbol_t::minus)) {
      next();
      auto ret = parse_expression_without_leading_sign(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      return std::make_unique<const internal::expression_binary_op_t>(
          lexer::token_t{.symbol = lexer::symbol_t::times, .annotation = {.source = "*", .start = 0U, .length = 1U}},
          std::make_unique<const internal::number_t>(lexer::token_t{
              .symbol = lexer::symbol_t::number, .annotation = {.source = "-1", .start = 0U, .length = 2}}),
          std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret)));
    }
    if (try_with(lexer::symbol_t::plus)) {
      next();
    }
    return parse_expression_without_leading_sign(env);
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::expression_t>, parse_error_t>
  parse_expression_without_leading_sign(internal::environment_t const& env) {
    if (auto pe = must_be_any_of({lexer::symbol_t::number, lexer::symbol_t::ident, lexer::symbol_t::lparen});
        has_parse_error(pe)) {
      return pe;
    }

    if (try_with(lexer::symbol_t::number)) {
      auto ret = std::make_unique<const internal::number_t>(*cur_token());
      next();
      return ret;
    }
    if (try_with(lexer::symbol_t::ident)) {
      auto const id = *cur_token();
      if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
        return parse_error_name_undefined_t{.name = id};
      }
      auto ret = std::make_unique<const internal::ident_t>(id);
      next();
      return ret;
    }
    if (try_with(lexer::symbol_t::lparen)) {
      next();
      auto ret = parse_expression(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      if (auto pe = must_be(lexer::symbol_t::rparen); has_parse_error(pe)) {
        return pe;
      }
      next();
      return std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret));
    }
    __builtin_unreachable();
  }

  [[nodiscard]] std::variant<std::unique_ptr<const internal::expression_t>, parse_error_t>
  parse_expression_precedence_climbing(internal::environment_t const& env,
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
      } else {
        parse_expression_reduce_all(expressions, ops);
        precedence = it->second;
        ops.push_back(*cur_token());
        next();
      }
      auto ret = parse_expression_without_leading_sign(env);
      if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
        return *pe;
      }
      expressions.push_back(std::move(std::get<std::unique_ptr<const internal::expression_t>>(ret)));
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
  internal::environment_t m_top_env;
};
} // namespace parser
#endif
