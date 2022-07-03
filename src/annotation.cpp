#include "annotation.hpp"

#include <iomanip>

std::string annotation_to_error_string(annotation_t const& annotation) {
  std::ostringstream oss;
  auto const prev_nl_n = annotation.source.substr(0, annotation.start).rfind('\n');
  auto const prev_nl = (prev_nl_n == std::string_view::npos ? 0 : prev_nl_n);
  auto const next_nl = annotation.source.substr(annotation.start + annotation.length).find('\n');
  constexpr auto leading_space = 4UL;
  for (auto i = 0UL; i != leading_space; ++i) {
    oss << ' ';
  }
  oss << annotation.source.substr(prev_nl, next_nl) << '\n';
  for (auto i = 0UL; i != leading_space + annotation.start; ++i) {
    oss << ' ';
  }
  for (auto i = 0UL; i != annotation.length; ++i) {
    oss << '^';
  }
  oss << '\n';
  return std::move(oss).str();
}
