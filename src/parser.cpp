#include "parser.hpp"

#include <cassert>
#include <map>

namespace parser {
std::ostream& operator<<(std::ostream& os, parse_error_t const& pe) {
  return std::visit(
      [&os]<typename T>(T const& e) -> std::ostream& {
        if constexpr (std::is_same_v<parse_error_ok_t, T>) {
          return os << "parse_error_ok_t";
        } else if constexpr (std::is_same_v<parse_error_empty_file_t, T>) {
          return os << "parse_error_empty_file_t";
        } else if constexpr (std::is_same_v<parse_error_early_eof_t, T>) {
          return os << "parse_error_early_eof_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_unexpected_t, T>) {
          return os << "parse_error_unexpected_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_name_redefined_t, T>) {
          return os << "parse_error_name_redefined_t: " << e;
        } else if constexpr (std::is_same_v<parse_error_name_undefined_t, T>) {
          return os << "parse_error_name_undefined_t: " << e;
        } else {
          __builtin_unreachable();
        }
      },
      pe);
}

std::variant<ast_t, parse_error_t> parser_t::parse() {
  if (m_tokens.empty()) {
    return parse_error_empty_file_t{};
  }

  if (auto pe = parse_program(); has_parse_error(pe)) {
    return pe;
  }
  return std::move(m_top_env);
};

std::optional<const lexer::token_t> parser_t::cur_token() const noexcept {
  if (m_cur_pos == m_tokens.size()) {
    return std::nullopt;
  }
  return m_tokens[m_cur_pos];
}

bool parser_t::try_with(lexer::symbol_t s) const noexcept {
  auto const ct = cur_token();
  return ct && ct->symbol == s;
}

std::optional<const lexer::symbol_t> parser_t::try_with_any_of(std::initializer_list<lexer::symbol_t> ss) {
  auto const p = [this](lexer::symbol_t s) {
    const auto ct = cur_token();
    return ct && ct->symbol == s;
  };
  if (auto const* const it = std::find_if(ss.begin(), ss.end(), p); it != ss.end()) {
    return *it;
  }
  return std::nullopt;
}

parse_error_t parser_t::must_be_any_of(std::initializer_list<lexer::symbol_t> ss) {
  if (try_with_any_of(ss)) {
    return parse_error_ok_t{};
  }

  auto const ct = cur_token();
  if (ct.has_value()) {
    return parse_error_unexpected_t{.expected = {ss}, .got = *ct};
  }
  return parse_error_early_eof_t{.expected = {ss}};
}

parse_error_t parser_t::must_be(lexer::symbol_t s) const noexcept {
  if (try_with(s)) {
    return parse_error_ok_t{};
  }

  auto const ct = cur_token();
  if (ct.has_value()) {
    return parse_error_unexpected_t{.expected = {s}, .got = *ct};
  }
  return parse_error_early_eof_t{.expected = {s}};
}

std::optional<lexer::token_t> parser_t::lookup_name(environment_t const& env, lexer::token_t const& name) {
  auto const find_name = [&name](auto const& container) -> std::optional<lexer::token_t> {
    if (auto const it = std::find_if(container.cbegin(), container.cend(),
                                     [&name](auto const& e) { return sv(name) == sv(e.token()); });
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
    if (auto const it = std::find_if(cur_env->procedures.cbegin(), cur_env->procedures.cend(),
                                     [&name](auto const& p) { return sv(p.token()) == sv(name); });
        it != cur_env->procedures.cend()) {
      return it->token();
    }
  }
  return std::nullopt;
}

parse_error_t parser_t::parse_program() {
  if (auto pe = parse_block(m_top_env); has_parse_error(pe)) {
    return pe;
  }
  return must_be(lexer::symbol_t::period);
}

parse_error_t parser_t::parse_block(environment_t& env) {
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
  env.statements = std::move(std::get<std::vector<std::unique_ptr<const statement_t>>>(ret));
  return parse_error_ok_t{};
}

parse_error_t parser_t::parse_consts(environment_t& env) {
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

parse_error_t parser_t::parse_vars(environment_t& env) {
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

parse_error_t parser_t::parse_procedures(environment_t& env) {
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

    environment_t block_env{};
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

parser_t::result_t<in_t> parser_t::parse_in(environment_t const& env) {
  next();

  if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
    return pe;
  }
  auto const id = *cur_token();
  if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
    return parse_error_name_undefined_t{.name = id};
  }

  next();

  return in_t{id};
}

parser_t::result_t<out_t> parser_t::parse_out(environment_t const& env) {
  next();

  auto ret = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  return out_t{std::move(std::get<std::unique_ptr<const expression_t>>(ret))};
}

parser_t::result_t<call_t> parser_t::parse_call(environment_t const& /*env*/) {
  next();

  if (auto pe = must_be(lexer::symbol_t::ident); has_parse_error(pe)) {
    return pe;
  }
  auto const id = *cur_token();

  next();

  return call_t{id};
}

parser_t::result_t<becomes_t> parser_t::parse_becomes(environment_t const& env) {
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

  return becomes_t{id, std::move(std::get<std::unique_ptr<const expression_t>>(expr))};
}

parser_t::result_t<begin_end_t> parser_t::parse_begin_end(environment_t const& env) {
  std::vector<std::unique_ptr<const statement_t>> statements;
  std::vector<std::unique_ptr<const statement_t>> sub_statements;
  do {
    next();
    auto ret = parse_statements(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    sub_statements = std::move(std::get<std::vector<std::unique_ptr<const statement_t>>>(ret));
    statements.insert(statements.end(), std::make_move_iterator(sub_statements.begin()),
                      std::make_move_iterator(sub_statements.end()));
  } while (try_with(lexer::symbol_t::semicolon));
  if (auto pe = must_be(lexer::symbol_t::end); has_parse_error(pe)) {
    return pe;
  }
  next();

  return begin_end_t{std::move(statements)};
}

parser_t::ptr_result_t<condition_t> parser_t::parse_condition(environment_t const& env) {
  next();
  if (try_with(lexer::symbol_t::odd)) {
    next();
    auto ret = parse_expression(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const odd_condition_t>(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  auto ret = parse_expression(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  auto lhs = std::move(std::get<std::unique_ptr<const expression_t>>(ret));

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
  auto rhs = std::move(std::get<std::unique_ptr<const expression_t>>(ret));

  return std::make_unique<const cmp_condition_t>(op, std::move(lhs), std::move(rhs));
}

parser_t::ptr_result_t<statement_t> parser_t::parse_statement(environment_t const& env) {
  if (try_with(lexer::symbol_t::in)) {
    auto ret = parse_in(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const in_t>(std::move(std::get<in_t>(ret)));
  }
  if (try_with(lexer::symbol_t::call)) {
    auto ret = parse_call(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const call_t>(std::move(std::get<call_t>(ret)));
  }
  if (try_with(lexer::symbol_t::out)) {
    auto ret = parse_out(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const out_t>(std::move(std::get<out_t>(ret)));
  }
  if (try_with(lexer::symbol_t::ident)) {
    auto ret = parse_becomes(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const becomes_t>(std::move(std::get<becomes_t>(ret)));
  }
  if (try_with(lexer::symbol_t::begin)) {
    auto ret = parse_begin_end(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const begin_end_t>(std::move(std::get<begin_end_t>(ret)));
  }
  if (try_with(lexer::symbol_t::if_)) {
    auto ret = parse_if_then(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const if_then_t>(std::move(std::get<if_then_t>(ret)));
  }
  if (try_with(lexer::symbol_t::while_)) {
    auto ret = parse_while_do(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const while_do_t>(std::move(std::get<while_do_t>(ret)));
  }
  __builtin_unreachable();
}

parser_t::result_t<if_then_t> parser_t::parse_if_then(environment_t const& env) {
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

  return if_then_t{std::move(std::get<std::unique_ptr<const condition_t>>(condition)),
                   std::move(std::get<std::unique_ptr<const statement_t>>(statement))};
}

parser_t::result_t<while_do_t> parser_t::parse_while_do(environment_t const& env) {
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

  return while_do_t{std::move(std::get<std::unique_ptr<const condition_t>>(condition)),
                    std::move(std::get<std::unique_ptr<const statement_t>>(statement))};
}

parser_t::ptr_vec_result_t<statement_t> parser_t::parse_statements(environment_t const& env) {
  std::vector<std::unique_ptr<const statement_t>> statements;
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
    statements.push_back(std::move(std::get<std::unique_ptr<const statement_t>>(ret)));
  }
  return statements;
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression(environment_t const& env) {
  std::vector<std::unique_ptr<const expression_t>> expressions{};
  auto ret = parse_expression_primary(env);
  if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
    return *pe;
  }
  expressions.push_back(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  std::vector<lexer::token_t> ops{};
  return parse_expression_precedence_climbing(env, expressions, ops, 0);
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression_primary(environment_t const& env) {
  if (try_with(lexer::symbol_t::minus)) {
    next();
    auto ret = parse_expression_without_leading_sign(env);
    if (auto const* pe = std::get_if<parse_error_t>(&ret); pe != nullptr) {
      return *pe;
    }
    return std::make_unique<const expression_binary_op_t>(
        lexer::token_t{.symbol = lexer::symbol_t::times, .annotation = {.source = "*", .start = 0U, .length = 1U}},
        std::make_unique<const number_t>(lexer::token_t{.symbol = lexer::symbol_t::number,
                                                        .annotation = {.source = "-1", .start = 0U, .length = 2}}),
        std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  if (try_with(lexer::symbol_t::plus)) {
    next();
  }
  return parse_expression_without_leading_sign(env);
}

parser_t::ptr_result_t<expression_t> parser_t::parse_expression_without_leading_sign(environment_t const& env) {
  if (auto pe = must_be_any_of({lexer::symbol_t::number, lexer::symbol_t::ident, lexer::symbol_t::lparen});
      has_parse_error(pe)) {
    return pe;
  }

  if (try_with(lexer::symbol_t::number)) {
    auto ret = std::make_unique<const number_t>(*cur_token());
    next();
    return ret;
  }
  if (try_with(lexer::symbol_t::ident)) {
    auto const id = *cur_token();
    if (auto const prev_define = lookup_name(env, id); !prev_define.has_value()) {
      return parse_error_name_undefined_t{.name = id};
    }
    auto ret = std::make_unique<const ident_t>(id);
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
    return std::move(std::get<std::unique_ptr<const expression_t>>(ret));
  }
  __builtin_unreachable();
}

parser_t::ptr_result_t<expression_t>
parser_t::parse_expression_precedence_climbing(environment_t const& env,
                                               std::vector<std::unique_ptr<const expression_t>>& expressions,
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
      expressions.back() = std::make_unique<const expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
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
    expressions.push_back(std::move(std::get<std::unique_ptr<const expression_t>>(ret)));
  }
  parse_expression_reduce_all(expressions, ops);
  return std::move(expressions[0]);
}

void parser_t::parse_expression_reduce_all(std::vector<std::unique_ptr<const expression_t>>& expressions,
                                           std::vector<lexer::token_t>& ops) {
  // NOLINTNEXTLINE(hicpp-no-array-decay, cppcoreguidelines-pro-bounds-array-to-pointer-decay)
  assert(ops.size() + 1 == expressions.size());
  while (!ops.empty()) {
    auto rhs = std::move(expressions.back());
    expressions.pop_back();
    auto lhs = std::move(expressions.back());
    auto const op = ops.back();
    ops.pop_back();
    expressions.back() = std::make_unique<const expression_binary_op_t>(op, std::move(lhs), std::move(rhs));
  }
}
} // namespace parser
