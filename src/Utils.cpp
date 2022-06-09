#include "Utils.hpp"

std::vector<std::string_view> splitLines(std::string_view s) {
  std::vector<std::string_view> v;
  using sty = std::decay_t<decltype(s)>;
  using ssty = decltype(s)::size_type;
  ssty prev = 0;
  while (prev != s.size()) {
    const ssty curr = s.find_first_of('\n', prev);
    if (curr != sty::npos) {
      v.emplace_back(&s[prev], curr - prev);
      prev = curr + 1;
    } else {
      v.emplace_back(&s[prev], s.size() - prev);
      break;
    }
  }
  return v;
}
