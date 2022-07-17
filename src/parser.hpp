#ifndef PL0_PARSER_HPP__
#define PL0_PARSER_HPP__

#include "lexer_symbols.hpp"
#include "utils/strings.hpp"

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>

#include <any>
#include <functional>
#include <iostream>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

namespace codegen {
struct codegen_t;
}

namespace parser {
[[nodiscard]] inline constexpr std::string_view sv(lexer::token_t const& token) noexcept {
  return to_sv(token.annotation);
}

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
    os << ", but got " << sv(pe.got) << '\n';
    return os << annotation_to_error_string(pe.got.annotation) << '\n';
  }
};
struct parse_error_name_redefined_t {
  lexer::token_t pre_defined;
  lexer::token_t cur_defined;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_name_redefined_t const& pe) {
    return os << sv(pe.cur_defined) << " previously defined at\n"
              << annotation_to_error_string(pe.pre_defined.annotation) << "redefined at\n"
              << annotation_to_error_string(pe.cur_defined.annotation);
  }
};
struct parse_error_name_undefined_t {
  lexer::token_t name;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, parse_error_name_undefined_t const& pe) {
    return os << sv(pe.name) << " is undefined" << '\n' << annotation_to_error_string(pe.name.annotation) << '\n';
  }
};
using parse_error_t =
    std::variant<parse_error_ok_t, parse_error_empty_file_t, parse_error_early_eof_t, parse_error_unexpected_t,
                 parse_error_name_redefined_t, parse_error_name_undefined_t>;

[[maybe_unused]] std::ostream& operator<<(std::ostream& os, parse_error_t const& pe);

[[nodiscard]] inline bool has_parse_error(parse_error_t const& error) noexcept {
  return !std::holds_alternative<parse_error_ok_t>(error);
}

template <typename T, typename... Ts> [[nodiscard]] std::string info_str(T&& v, Ts&&... vs) {
  return utils::str::to_str(std::forward<T>(v), std::forward<Ts>(vs)...);
}

struct environment_t;

struct statement_t {
  statement_t() = default;
  statement_t(statement_t const&) = default;
  statement_t(statement_t&&) noexcept = default;
  statement_t& operator=(statement_t const&) & = default;
  statement_t& operator=(statement_t&&) & noexcept = default;
  virtual ~statement_t() = default;

  [[nodiscard]] virtual std::string to_string() const = 0;

  virtual void codegen(codegen::codegen_t& cg) const = 0;
};

struct expression_t {
  expression_t() = default;
  expression_t(expression_t const&) = default;
  expression_t(expression_t&&) noexcept = default;
  expression_t& operator=(expression_t const&) & = default;
  expression_t& operator=(expression_t&&) & noexcept = default;
  virtual ~expression_t() = default;

  [[nodiscard]] virtual std::string to_string() const = 0;

  virtual llvm::Value* codegen(codegen::codegen_t& cg) const = 0;
};

struct expression_binary_op_t : expression_t {
  expression_binary_op_t(lexer::token_t const& op, std::unique_ptr<const expression_t>&& lhs,
                         std::unique_ptr<const expression_t>&& rhs)
      : m_op(op), m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {}

  [[nodiscard]] std::string to_string() const override {
    return info_str('(', sv(m_op), ' ', m_lhs->to_string(), ' ', m_rhs->to_string(), ')');
  }

  llvm::Value* codegen(codegen::codegen_t& cg) const override;

private:
  lexer::token_t m_op;
  std::unique_ptr<const expression_t> m_lhs;
  std::unique_ptr<const expression_t> m_rhs;
};

struct number_t : expression_t {
  explicit number_t(lexer::token_t const& value) : m_value(value) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_value)); }

  llvm::Value* codegen(codegen::codegen_t& cg) const override;

private:
  lexer::token_t m_value;
};

struct ident_t : expression_t {
  explicit ident_t(lexer::token_t const& name) : m_name(name) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_name)); }

  llvm::Value* codegen(codegen::codegen_t& cg) const override;

private:
  lexer::token_t m_name;
};

struct call_t : statement_t {
  explicit call_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const override { return info_str("call ", sv(m_ident)); }

  void codegen(codegen::codegen_t& cg) const override;

private:
  lexer::token_t m_ident;
};

struct in_t : statement_t {
  explicit in_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const override { return info_str("?", sv(m_ident)); }

  void codegen(codegen::codegen_t& cg) const override;

private:
  lexer::token_t m_ident;
};

struct out_t : statement_t {
  explicit out_t(std::unique_ptr<const expression_t>&& expression) : m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str("!", m_expression->to_string()); }

  void codegen(codegen::codegen_t& cg) const override;

private:
  std::unique_ptr<const expression_t> m_expression;
};

struct becomes_t : statement_t {
  becomes_t(lexer::token_t const& name, std::unique_ptr<const expression_t>&& expression)
      : m_name(name), m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str(sv(m_name), ":=", m_expression->to_string()); }

  void codegen(codegen::codegen_t& cg) const override;

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

  virtual llvm::Value* codegen(codegen::codegen_t& cg) const = 0;
};

struct odd_condition_t : condition_t {
  explicit odd_condition_t(std::unique_ptr<const expression_t>&& expression) : m_expression(std::move(expression)) {}

  [[nodiscard]] std::string to_string() const override { return info_str("odd ", m_expression->to_string()); }

  llvm::Value* codegen(codegen::codegen_t& cg) const override;

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

  llvm::Value* codegen(codegen::codegen_t& cg) const override;

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

  void codegen(codegen::codegen_t& cg) const override;

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

  void codegen(codegen::codegen_t& cg) const override;

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

  void codegen(codegen::codegen_t& cg) const override;

private:
  std::vector<std::unique_ptr<const statement_t>> m_statements;
};

struct const_t {
  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  const_t(lexer::token_t const& ident, lexer::token_t const& num) : m_ident(ident), m_num(num) {}

  [[nodiscard]] std::string to_string() const { return info_str("const ", sv(m_ident), "=", sv(m_num)); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

  friend codegen::codegen_t;

private:
  lexer::token_t m_ident;
  lexer::token_t m_num;
};

struct var_t {
  explicit var_t(lexer::token_t const& ident) : m_ident(ident) {}

  [[nodiscard]] std::string to_string() const { return info_str("var ", sv(m_ident)); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

  friend codegen::codegen_t;

private:
  lexer::token_t m_ident;
};

struct procedure_t {
  procedure_t(lexer::token_t const& ident, environment_t&& env)
      : m_ident(ident), m_env(std::make_unique<environment_t>(std::move(env))) {}

  [[nodiscard]] std::string to_string() const { return info_str("procedure ", sv(m_ident), ';', *m_env, ';'); }

  [[nodiscard]] lexer::token_t const& token() const noexcept { return m_ident; }

  friend codegen::codegen_t;

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

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, environment_t const& program) {
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

struct ast_t {
  explicit ast_t(environment_t&& top_env) : m_top_env(std::move(top_env)) {}

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, ast_t const& ast) { return os << ast.m_top_env; }

  friend codegen::codegen_t;

private:
  environment_t m_top_env;
};

struct parser_t {
  explicit parser_t(lexer::tokens_t tokens) noexcept : m_tokens(std::move(tokens)) {}

  [[nodiscard]] std::variant<ast_t, parse_error_t> parse();

private:
  template <typename R> using result_t = std::variant<R, parse_error_t>;
  template <typename R> using ptr_result_t = std::variant<std::unique_ptr<const R>, parse_error_t>;
  template <typename R> using ptr_vec_result_t = std::variant<std::vector<std::unique_ptr<const R>>, parse_error_t>;

  lexer::tokens_t::size_type m_cur_pos = 0;

  void next() noexcept { ++m_cur_pos; }

  [[nodiscard]] std::optional<const lexer::token_t> cur_token() const noexcept;

  [[nodiscard]] bool try_with(lexer::symbol_t s) const noexcept;
  [[nodiscard]] std::optional<const lexer::symbol_t> try_with_any_of(std::initializer_list<lexer::symbol_t> ss);
  [[nodiscard]] parse_error_t must_be_any_of(std::initializer_list<lexer::symbol_t> ss);
  [[nodiscard]] parse_error_t must_be(lexer::symbol_t s) const noexcept;

  [[nodiscard]] static std::optional<lexer::token_t> lookup_name(environment_t const& env, lexer::token_t const& name);

  [[nodiscard]] parse_error_t parse_program();

  [[nodiscard]] parse_error_t parse_block(environment_t& env);

  [[nodiscard]] parse_error_t parse_consts(environment_t& env);

  [[nodiscard]] parse_error_t parse_vars(environment_t& env);

  [[nodiscard]] parse_error_t parse_procedures(environment_t& env);

  [[nodiscard]] result_t<in_t> parse_in(environment_t const& env);

  [[nodiscard]] result_t<out_t> parse_out(environment_t const& env);

  [[nodiscard]] result_t<call_t> parse_call(environment_t const& /*env*/);

  [[nodiscard]] result_t<becomes_t> parse_becomes(environment_t const& env);

  [[nodiscard]] result_t<begin_end_t> parse_begin_end(environment_t const& env);

  [[nodiscard]] ptr_result_t<condition_t> parse_condition(environment_t const& env);

  [[nodiscard]] ptr_result_t<statement_t> parse_statement(environment_t const& env);

  [[nodiscard]] result_t<if_then_t> parse_if_then(environment_t const& env);

  [[nodiscard]] result_t<while_do_t> parse_while_do(environment_t const& env);

  [[nodiscard]] ptr_vec_result_t<statement_t> parse_statements(environment_t const& env);

  [[nodiscard]] ptr_result_t<expression_t> parse_expression(environment_t const& env);

  [[nodiscard]] ptr_result_t<expression_t> parse_expression_primary(environment_t const& env);

  [[nodiscard]] ptr_result_t<expression_t> parse_expression_without_leading_sign(environment_t const& env);

  [[nodiscard]] ptr_result_t<expression_t>
  parse_expression_precedence_climbing(environment_t const& env,
                                       std::vector<std::unique_ptr<const expression_t>>& expressions,
                                       std::vector<lexer::token_t>& ops, int precedence);

  static void parse_expression_reduce_all(std::vector<std::unique_ptr<const expression_t>>& expressions,
                                          std::vector<lexer::token_t>& ops);
  lexer::tokens_t m_tokens;
  environment_t m_top_env;
};
} // namespace parser

namespace codegen {
struct scope_t {
  scope_t* m_parent = nullptr;
  std::string_view name;
  std::map<std::string_view, llvm::ConstantInt*> m_consts;
  std::map<std::string_view, llvm::AllocaInst*> m_vars;
  std::map<std::string_view, llvm::Function*> m_funcs;

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, scope_t const& c) {
    os << "const table\n";
    for (auto const& p : c.m_consts) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "var table\n";
    for (auto const& p : c.m_vars) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "func table\n";
    for (auto const& p : c.m_funcs) {
      os << "  " << p.first << ':' << static_cast<void const*>(p.second) << '\n';
    }
    return os;
  }
};

struct codegen_t {
  codegen_t() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();

    auto* out_fn = create_std_out();
    add_fn_to_scope("out", out_fn);
  }

  void compile_env(parser::environment_t const& env) {
    for (auto const& c : env.consts) {
      add_const_to_scope(parser::sv(c.m_ident), llvm::ConstantInt::get(m_int_type, parser::sv(c.m_num), 10));
    }
    for (auto const& v : env.vars) {
      add_var_to_scope(parser::sv(v.m_ident), m_builder.CreateAlloca(m_int_type, nullptr, parser::sv(v.m_ident)));
    }
    for (auto const& p : env.procedures) {
      auto* proto = llvm::FunctionType::get(llvm::Type::getVoidTy(m_context), /*isVarArg*/ false);

      auto* fn = llvm::Function::Create(proto, llvm::Function::ExternalLinkage, parser::sv(p.m_ident), m_module);
      auto* bb = llvm::BasicBlock::Create(m_context, "entry", fn);
      if (bb == nullptr) {
        fn->eraseFromParent();
        assert(false); // NOLINT
      }
      m_builder.SetInsertPoint(bb);

      m_scopes.emplace_back();
      m_scopes.back().m_parent = m_cur_scope;
      m_cur_scope = &m_scopes.back();
      m_cur_scope->name = parser::sv(p.m_ident);
      compile_env(*p.m_env);
      m_cur_scope = m_cur_scope->m_parent;

      m_builder.CreateRetVoid();

      if (!llvm::verifyFunction(*fn, &llvm::errs())) {
        fn->eraseFromParent();
        assert(false); // NOLINT
      }

      add_fn_to_scope(parser::sv(p.m_ident), fn);
    }
    for (auto const& s : env.statements) {
      s->codegen(*this);
    }
  }

  llvm::Function* create_std_out() {
    auto* out =
        dyn_cast<llvm::Function>(m_module.getOrInsertFunction("out", m_builder.getVoidTy(), m_int_type).getCallee());

    auto* bb = llvm::BasicBlock::Create(m_context, "entry", out);
    m_builder.SetInsertPoint(bb);

    auto printf_fn = m_module.getOrInsertFunction(
        "printf", llvm::FunctionType::get(m_builder.getInt32Ty(), llvm::PointerType::get(m_builder.getInt8Ty(), 0),
                                          /*isVarArg*/ true));
    auto* val = &*out->arg_begin();
    auto* fmt = m_builder.CreateGlobalStringPtr("%ld\n", ".printf.fmt");
    m_builder.CreateCall(printf_fn, {fmt, val});
    m_builder.CreateRetVoid();

    if (!llvm::verifyFunction(*out)) {
      return nullptr;
    }
    return out;
  }

  llvm::LLVMContext m_context;
  llvm::Module m_module{"main", m_context};
  llvm::IRBuilder<> m_builder{m_context};

  llvm::IntegerType* m_int_type = m_builder.getInt64Ty();

  void add_fn_to_scope(std::string_view name, llvm::Function* fn) {
    assert(fn != nullptr); // NOLINT
    m_cur_scope->m_funcs[name] = fn;
  }
  void add_const_to_scope(std::string_view name, llvm::ConstantInt* value) {
    assert(value != nullptr); // NOLINT
    m_cur_scope->m_consts[name] = value;
  }
  void add_var_to_scope(std::string_view name, llvm::AllocaInst* value) {
    assert(value != nullptr); // NOLINT
    m_cur_scope->m_vars[name] = value;
  }

  template <typename R, typename C> R* find_name(C const& c, std::string_view name) {
    if (auto const it = std::find_if(c.cbegin(), c.cend(), [name](auto const& p) { return p.first == name; });
        it != c.cend()) {
      return it->second;
    }
    return nullptr;
  };

  llvm::ConstantInt* find_const(std::string_view name) {
    for (auto const* cur_scope = m_cur_scope; cur_scope != nullptr; cur_scope = cur_scope->m_parent) {
      if (auto* v = find_name<llvm::ConstantInt>(cur_scope->m_consts, name); v != nullptr) {
        return v;
      }
    }
    return nullptr;
  }

  llvm::AllocaInst* find_var(std::string_view name) {
    for (auto const* cur_scope = m_cur_scope; cur_scope != nullptr; cur_scope = cur_scope->m_parent) {
      if (auto const v = find_name<llvm::AllocaInst>(cur_scope->m_vars, name); v != nullptr) {
        return v;
      }
    }
    return nullptr;
  }

  llvm::Function* find_function(std::string_view name) {
    for (auto const* cur_scope = m_cur_scope; cur_scope != nullptr; cur_scope = cur_scope->m_parent) {
      // current procedure
      // don't support recursion
      if (cur_scope->m_parent != nullptr && cur_scope->name == name) {
        return nullptr;
      }
      if (auto* const v = find_name<llvm::Function>(cur_scope->m_funcs, name); v != nullptr) {
        return v;
      }
    }
    return nullptr;
  }

  std::vector<scope_t> m_scopes{1};
  scope_t* m_cur_scope{m_scopes.data()};
};
} // namespace codegen
#endif
