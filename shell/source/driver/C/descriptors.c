#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>

#define LENGTH_MASK   0x000000000000FFFFULL
#define FLAGS_MASK    0x0000000000FF0000ULL
#define ADDRESS_MASK  0xFFFFFFFFFF000000ULL

#define FLAGS_SHIFT   16
#define ADDRESS_SHIFT 24

typedef struct node_t {
    uint16_t length;
    uint8_t flags;
    uint64_t address;
    struct node_t* next;
} node_t;

typedef struct {
    uint32_t capacity;
    node_t* write_pointer;
    node_t* read_pointer;
} ring_t;

static inline node_t parser(uint64_t descriptor) {
    return (node_t) {
        .length  = (uint16_t)(descriptor & LENGTH_MASK),
        .flags   = (uint8_t)((descriptor & FLAGS_MASK) >> FLAGS_SHIFT),
        .address = (descriptor & ADDRESS_MASK) >> ADDRESS_SHIFT
    };
}

node_t* create_node(uint16_t length, uint8_t flags, uint64_t address) {
    node_t* node = malloc(sizeof(*node));
    if (node == NULL) return NULL;

    node->length = length;
    node->flags = flags;
    node->address = address;
    node->next = NULL;
    return node;
}

ring_t* create_ring(uint32_t capacity) {
    if (capacity == 0) return NULL;

    ring_t* ring = malloc(sizeof(*ring));
    if (ring == NULL) return NULL;

    ring->capacity = capacity;

    node_t* node = create_node(0, 0, 0);
    if (node == NULL) {
        free(ring);
        return NULL;
    }

    node_t* current = node;
    for (uint32_t i = 1; i < capacity; i++) {
        current->next = create_node(0, 0, 0);
        if (current->next == NULL) return NULL;
        current = current->next;
    }

    current->next = node;

    ring->write_pointer = node;
    ring->read_pointer = node;
    return ring;
}

int main(void) {
    ring_t* ring = create_ring(20);
    if (ring == NULL) return 1;

    node_t* current = ring->write_pointer;
    do {
        printf("Length: %u, Flags: 0x%02x, Address: 0x%" PRIx64 "\n", 
               current->length, current->flags, current->address);
        current = current->next;
    } while (current != ring->write_pointer);

    return 0;
}
