#pragma once

#include <map>
#include <vector>

constexpr int HEAP_NULL = -1;

struct Heap
{
    explicit Heap(int capacity);
    ~Heap();

    int allocate(int req_size);
    void deallocate(int address);

    void print_allocated();
    void print_free();
    void print_memory();

    template <typename T> T* ref(int ptr) const noexcept
    {
        return reinterpret_cast<T*>((char*)_memory + ptr);
    }

private:
    std::vector<std::vector<std::pair<int, int>>> free_list; // the heap

    // exponent of the smallest power of two not less than requested bytes
    static int get_free_list_index(int bytes) noexcept;

    std::map<int, int> mp; // allocated memory blocks - use in deallocation

    void* _memory; // "true" memory page
    int _sz; // and page size (in bytes)
};
