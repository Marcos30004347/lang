#pragma once

namespace lib {

template < typename T > T max(T a, T b) {
  return (a > b) ? a : b;
}

template < typename T > T is_equal(T a, T b) {
  return a == b;
}

template < typename T > T is_smaller(T a, T b) {
  return a < b;
}

template < typename T > T is_greater(T a, T b) {
  return a > b;
}

} // namespace lib
