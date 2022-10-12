#pragma once

#include "types.hpp"

namespace lib {

template < typename T > T max(T a, T b) {
  return (a > b) ? a : b;
}

template < typename T > b8 is_equal(T a, T b) {
  return (u64)a == (u64)b;
}

template < typename T > b8 is_smaller(T a, T b) {
  return a < b;
}

template < typename T > b8 is_greater(T a, T b) {
  return a > b;
}

} // namespace lib
