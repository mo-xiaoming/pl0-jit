#ifndef PL0_CODEGEN_HPP__
#define PL0_CODEGEN_HPP__

namespace parser {
struct ast_t;
}

namespace codegen {
void generate(parser::ast_t const& ast);
} // namespace codegen
#endif
