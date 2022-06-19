#ifndef PL0_ANNOTATION_HPP__
#define PL0_ANNOTATION_HPP__

#include <ostream>

struct annotation_t {
  char const* start;
  std::size_t length;

  [[nodiscard]] friend constexpr bool operator==(annotation_t const& lhs, annotation_t const& rhs) noexcept = default;

  friend std::ostream& operator<<(std::ostream& os, annotation_t const& v) {
    return os << "annotation_t{ .start=" << static_cast<void const*>(v.start) << ", .length=" << v.length << " }";
  }
};

#endif
