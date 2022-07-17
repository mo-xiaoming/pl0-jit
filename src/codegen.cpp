#include "codegen.hpp"
#include "parser.hpp"

namespace {
struct scope_t {
  std::map<std::string_view, llvm::ConstantInt*> m_consts;
  std::map<std::string_view, llvm::Value*> m_vars;
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

struct llvm_codegen_t {
  llvm_codegen_t() = default;
  llvm_codegen_t(llvm_codegen_t const&) = delete;
  llvm_codegen_t(llvm_codegen_t&&) = delete;
  llvm_codegen_t& operator=(llvm_codegen_t const&) = delete;
  llvm_codegen_t& operator=(llvm_codegen_t&&) = delete;
  ~llvm_codegen_t() = default;
#if 0
  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  llvm::ConstantInt* operator()(parser::const_t const& /*tag*/, lexer::token_t const& ident,
                                lexer::token_t const& num) {
    auto* value = llvm::ConstantInt::get(m_int_type, parser::sv(num), 10);
    assert(value != nullptr); // NOLINT
    add_const_to_scope(parser::sv(ident), value);
    return value;
  }
  llvm::ConstantInt* operator()(parser::number_t const& /*tag*/, lexer::token_t const& num) {
    auto* value = llvm::ConstantInt::get(m_int_type, parser::sv(num), 10);
    assert(value != nullptr); // NOLINT
    add_const_to_scope(parser::sv(num), value);
    return value;
  }
  llvm::Value* operator()(parser::var_t const& /*tag*/, lexer::token_t const& ident) {
    auto* value = llvm::ConstantInt::get(m_int_type, 0, /*isSigned*/ true);
    assert(value != nullptr); // NOLINT
    add_var_to_scope(parser::sv(ident), value);
    return value;
  }
  llvm::Value* operator()(parser::expression_binary_op_t const& /*tag*/, lexer::symbol_t op, llvm::Value* lhs,
                          llvm::Value* rhs) {
    switch (op) {
    case lexer::symbol_t::plus:
      return m_builder.CreateAdd(lhs, rhs, "addtmp");
    case lexer::symbol_t::minus:
      return m_builder.CreateSub(lhs, rhs, "subtmp");
    case lexer::symbol_t::times:
      return m_builder.CreateMul(lhs, rhs, "multmp");
    case lexer::symbol_t::divide:
      return m_builder.CreateSDiv(lhs, rhs, "divtmp");
    default:
      __builtin_unreachable();
    }
  }
  llvm::Value* operator()(parser::cmp_condition_t const& /*tag*/, lexer::symbol_t op, llvm::Value* lhs,
                          llvm::Value* rhs) {
    switch (op) {
      return m_builder.CreateICmpSGT(lhs, rhs, "gttmp");
    case lexer::symbol_t::greater_equal:
      return m_builder.CreateICmpSGE(lhs, rhs, "getmp");
    case lexer::symbol_t::less:
      return m_builder.CreateICmpSLT(lhs, rhs, "sttmp");
    case lexer::symbol_t::less_equal:
      return m_builder.CreateICmpSGE(lhs, rhs, "setmp");
    case lexer::symbol_t::equal:
      return m_builder.CreateICmpEQ(lhs, rhs, "eqtmp");
    case lexer::symbol_t::not_equal:
      return m_builder.CreateICmpNE(lhs, rhs, "netmp");
    default:
      __builtin_unreachable();
    }
  }
  llvm::Value* operator()(parser::ident_t const& /*tag*/, lexer::token_t const& /*ident*/) { return nullptr; }
  llvm::Value* operator()(parser::call_t const& /*tag*/, lexer::token_t const& ident) {
    auto* func = m_module.getFunction(parser::sv(ident));
    if (func == nullptr) {
      return func;
    }

    assert(func->arg_empty()); // NOLINT

    return m_builder.CreateCall(func);
  }
  template <typename BodyCodegenFunc>
  llvm::Function* operator()(parser::procedure_t const& /*tag*/, lexer::token_t const& ident,
                             BodyCodegenFunc&& body_codegen_func) {
    auto* func = m_module.getFunction(parser::sv(ident));
    if (func != nullptr) {
      std::cerr << "function " << parser::sv(ident) << " redefined\n";
      return nullptr;
    }

    auto* proto_type = llvm::FunctionType::get(llvm::Type::getVoidTy(m_context), /*isVarArg*/ false);
    if (proto_type == nullptr) {
      return nullptr;
    }

    func = llvm::Function::Create(proto_type, llvm::Function::ExternalLinkage, parser::sv(ident), m_module);
    if (func == nullptr) {
      return func;
    }

    auto* bb = llvm::BasicBlock::Create(m_context, "entry", func);
    if (bb == nullptr) {
      func->eraseFromParent();
      return nullptr;
    }
    m_builder.SetInsertPoint(bb);

    body_codegen_func();

    m_builder.CreateRetVoid();

    if (!llvm::verifyFunction(*func, &llvm::errs())) {
      func->eraseFromParent();
      return nullptr;
    }

    add_func_to_scope(parser::sv(ident), func);

    return func;
  }
  void operator()(auto&& /*unused*/) { __builtin_unreachable(); }

#endif
private:
  llvm::LLVMContext m_context;
  llvm::Module m_module{"main", m_context};
  llvm::IRBuilder<> m_builder{m_context};

  llvm::IntegerType* m_int_type = m_builder.getInt64Ty();

  void add_const_to_scope(const std::string_view name, llvm::ConstantInt* value) { cur_scope().m_consts[name] = value; }
  void add_var_to_scope(std::string_view const name, llvm::Value* value) { cur_scope().m_vars[name] = value; }
  void add_func_to_scope(std::string_view name, llvm::Function* value) { cur_scope().m_funcs[name] = value; }
  scope_t& cur_scope() {
    if (m_scopes.empty()) {
      return m_scopes.emplace_back();
    }
    return m_scopes.back();
  }
  scope_t& new_scope() { return m_scopes.emplace_back(); }
  scope_t* pop_scope() { return nullptr; }
  std::vector<scope_t> m_scopes;
};
} // namespace

namespace codegen {
#if 0
void generate(parser::ast_t const& ast) {
  auto gen = llvm_codegen_t{};
  ast.accept(gen);
  std::cout << gen << '\n';
}
#endif
} // namespace codegen
