#ifndef PL0_ANNOTATION_HPP__
#define PL0_ANNOTATION_HPP__

#include <cstddef>

struct annotation_t {
  char const* start;
  std::size_t length;
};

#endif