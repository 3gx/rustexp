#include <stdint.h>

int64_t c_loop(int64_t high) {
  int64_t total = 0;
  int64_t i;
  for (i = 1; i <= high; ++i) {
    if (i % 2 == 0) {
      total += i * 2;
    }
  }
  return total;
}

int64_t c_bits(int64_t high) {
  int64_t total = 0;
  int64_t i;
  for (i = 1; i <= high; ++i) {
    total += ((i % 2) - 1) & (i + i);
  }
  return total;
}

int64_t c_cheating(int64_t high) {
  int64_t total = 0;
  int64_t i;
  high *= 2;
  for (i = 4; i <= high; i += 4) {
    total += i;
  }
  return total;
}
