#include "lexer.hpp"
#include "parser.hpp"
#include "utils_for_test.hpp"

int main() {
#if 1
  auto const tokens = std::get<lexer::tokens_t>(lex_string(R"(
  procedure print_13_out_of_42;
  var a;
  begin
    a := 42;
    while a > 10 do
    begin
      a := a-1;
      if a = 13 then
        !a
    end
  end;
  call print_13_out_of_42
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
