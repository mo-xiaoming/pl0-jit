#ifndef PL0_UTILS_STRINGS_HPP__
#define PL0_UTILS_STRINGS_HPP__

#include <sstream>
#include <string>

namespace utils::str {
template <typename T>
concept streamable = requires(T t, std::ostream& os) {
  os << t;
};
template <typename T, typename... Ts>
concept all_streamable = streamable<T> &&(streamable<Ts>&&...);

template <typename T, typename... Ts>
requires all_streamable<T, Ts...> std::string to_str(T&& v, Ts&&... vs) {
  std::ostringstream oss;
  oss << std::forward<T>(v);
  ((oss << std::forward<Ts>(vs)), ...);
  return std::move(oss).str();
}
} // namespace utils::str

#endif
