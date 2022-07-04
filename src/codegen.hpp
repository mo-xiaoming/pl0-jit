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
struct codegen_t {
private:
  static std::unique_ptr<llvm::LLVMContext> the_context;
  static std::unique_ptr<llvm::Module> the_module;
  static std::unique_ptr<llvm::IRBuilder<>> the_builder;
  static std::map<std::string_view, llvm::Value*> the_symbol_table;
};
} // namespace codegen
