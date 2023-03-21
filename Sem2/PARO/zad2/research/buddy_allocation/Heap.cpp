#include "Heap.hpp"

#include <cmath> // log2, ceil

Heap::Heap(int capacity)
    : free_list(1 + get_free_list_index(capacity)),
      _memory(malloc(capacity)),
      _sz(capacity)
{
    std::fill_n(static_cast<char*>(_memory), _sz, 0x0);

    // single block of specified size is available at the beginning
    free_list.back().emplace_back(0, capacity - 1);
}

Heap::~Heap() { free(_memory); }

int Heap::get_free_list_index(int bytes) noexcept { return ceil(log2(bytes)); }
