#include "lexer.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

int main() {
#if 1
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"(
  var a;
  procedure square;
  begin
    a := 3
  end;
  call square 
  .
  )"));
  auto parser = parser::parser_t{tokens};
#else
  auto const& [buffer, tokens] = lexer::lex_source_file("a.pl0");
  auto parser = parser::parser_t{std::get<lexer::tokens_t>(tokens)};
#endif
  auto const result = parser.parse();
  if (std::holds_alternative<parser::parse_error_t>(result)) {
    std::cerr << std::get<parser::parse_error_t>(result) << '\n';
    return 1;
  }
  auto const& ast = std::get<parser::ast_t>(result);

  ast.codegen();
}
