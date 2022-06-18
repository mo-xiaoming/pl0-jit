#ifndef PL0_ANNOTATION_HPP__
#define PL0_ANNOTATION_HPP__

#include "utils/strings.hpp"

#include <ostream>

struct annotation_t {
  char const* start;
  std::size_t length;

  [[nodiscard]] friend bool operator==(annotation_t const& lhs, annotation_t const& rhs) = default;

  friend std::ostream& operator<<(std::ostream& os, annotation_t const& v) {
    return os << utils::str::to_str("annotation_t{ .start=", static_cast<void const*>(v.start), ", .length=", v.length,
                                    " }");
  }
};

#endif
