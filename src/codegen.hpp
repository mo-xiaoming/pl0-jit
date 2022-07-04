#ifndef PL0_CODEGEN_HPP__
#define PL0_CODEGEN_HPP__

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

namespace codegen {
namespace internal {
struct llvm_codegen {
  // NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
  void operator()(parser::const_t const&, lexer::token_t const& ident, lexer::token_t const& num) {
    std::cerr << "const\n";
  }
  void operator()(auto&& /*unused*/) { __builtin_unreachable(); }
};
} // namespace internal

struct codegen_t {
  void generate(parser::ast_t const& ast) { ast.accept(internal::llvm_codegen{}); }

private:
  std::unique_ptr<llvm::LLVMContext> m_context;
  std::unique_ptr<llvm::Module> m_module;
  std::unique_ptr<llvm::IRBuilder<>> m_builder;
  std::map<std::string_view, llvm::Value*> m_symbol_table;
};
} // namespace codegen
#endif
