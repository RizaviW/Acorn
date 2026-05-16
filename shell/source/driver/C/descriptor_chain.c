#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#define LENGTH_MASK   0x000000000000FFFF
#define FLAGS_MASK    0x0000000000FF0000
#define ADDRESS_MASK  0xFFFFFFFFFF000000ULL
#define FLAGS_SHIFT   16
#define ADDRESS_SHIFT 24

typedef struct {
  uint16_t length;
  uint8_t  flags;
  uint64_t address;
} descriptor_t;

static inline descriptor_t parser(uint64_t descriptor) {
  return (descriptor_t) {
    .length  = descriptor & LENGTH_MASK,
    .flags   = (descriptor & FLAGS_MASK) >> FLAGS_SHIFT,
    .address = (descriptor & ADDRESS_MASK) >> ADDRESS_SHIFT
  };
}
