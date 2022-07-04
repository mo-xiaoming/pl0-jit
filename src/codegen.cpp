#include "codegen.hpp"
#include "parser.hpp"

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <map>
#include <memory>

namespace {
struct llvm_codegen {
  llvm_codegen() = default;
  llvm_codegen(llvm_codegen const&) = delete;
  llvm_codegen(llvm_codegen&&) = delete;
  llvm_codegen& operator=(llvm_codegen const&) = delete;
  llvm_codegen& operator=(llvm_codegen&&) = delete;
  ~llvm_codegen() = default;

  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  llvm::Value* operator()(parser::const_t const& /*tag*/, lexer::token_t const& ident, lexer::token_t const& num) {
    auto* value = llvm::ConstantInt::get(int_type(), parser::sv(num), 10);
    assert(value != nullptr); // NOLINT
    m_const_symbol_table[parser::sv(ident)] = value;
    return value;
  }
  llvm::Value* operator()(parser::var_t const& /*tag*/, lexer::token_t const& ident) {
    auto* value = llvm::ConstantInt::get(int_type(), 0, true);
    assert(value != nullptr); // NOLINT
    m_var_symbol_table[parser::sv(ident)] = value;
    return value;
  }
  void operator()(auto&& /*unused*/) { __builtin_unreachable(); }

  [[maybe_unused]] friend std::ostream& operator<<(std::ostream& os, llvm_codegen const& c) {
    os << "const table\n";
    for (auto const& p : c.m_const_symbol_table) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "var table\n";
    for (auto const& p : c.m_var_symbol_table) {
      os << "  " << p.first << ':' << llvm::dyn_cast<llvm::ConstantInt>(p.second)->getSExtValue() << '\n';
    }
    os << "func table\n";
    for (auto const& p : c.m_func_symbol_table) {
      os << "  " << p.first << ':' << static_cast<void const*>(p.second) << '\n';
    }
    return os;
  }

private:
  llvm::IntegerType* int_type() {
    // NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
    static auto* t = llvm::IntegerType::get(m_context, 64);
    return t;
  }

  llvm::LLVMContext m_context;
  llvm::Module m_module{"main", m_context};
  llvm::IRBuilder<> m_builder{m_context};

  std::map<std::string_view, llvm::Value*> m_const_symbol_table;
  std::map<std::string_view, llvm::Value*> m_var_symbol_table;
  std::map<std::string_view, llvm::Value*> m_func_symbol_table;
};
} // namespace

namespace codegen {

void generate(parser::ast_t const& ast) {
  auto gen = llvm_codegen{};
  ast.accept(gen);
  std::cout << gen << '\n';
}

} // namespace codegen
