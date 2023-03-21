#include "Heap.hpp"

#include <cstring> // memcpy
#include <iostream>

void demo_fragmentation();
void demo_memory_access();

void demo_deallocation_with_block_coalescing();
void demo_deallocation_no_coalescence_possible();

int main(int, char*[])
{
    demo_fragmentation();
    demo_memory_access();

    demo_deallocation_with_block_coalescing();
    demo_deallocation_no_coalescence_possible();
    return 0;
}

void demo_fragmentation()
{
    std::cout << "[DEMO] Heap Fragmentation" << std::endl;

    Heap h(128);
    h.print_free();
    h.allocate(32);
    h.allocate(5); // 3 bytes wasted here
    h.allocate(64);

    h.print_allocated(); // there is free space between 40 and 63 = 24 bytes

    h.allocate(20); // but no 20 bytes free in single block
    h.print_free();
    h.allocate(16); // 16 plus 8 which is more than 20...
    h.allocate(8); // ...and it is OK to allocate

    h.print_allocated();
}

void demo_memory_access()
{
    std::cout << "[DEMO] Heap Memory Access" << std::endl;

    Heap h(128);
    h.print_free();

    h.allocate(8);
    int hello = h.allocate(16);
    memcpy(h.ref<char*>(hello), "HELLO WORLD!!!!", 16);

    int addr = h.allocate(5);
    std::cout << "address = " << addr << "(0x" << std::hex << addr << ")"
              << std::endl;

    *(h.ref<int>(addr)) = 0x11'22'33'44;

    h.print_memory();

    addr = h.allocate(3);

    std::cout << "address = " << std::dec << addr << std::endl;
}

void demo_deallocation_with_block_coalescing()
{
    std::cout << "[DEMO] Heap Deallocation" << std::endl;
    Heap h(128);

    h.print_free();
    h.allocate(16);
    h.allocate(16);
    h.allocate(16);
    h.allocate(16);
    h.print_allocated();
    h.print_free();

    h.deallocate(0);
    h.deallocate(9); // invalid request - no such pointer

    h.print_free();
    h.deallocate(32);
    h.print_free();
    h.deallocate(16); // coalescing
    h.print_free();

    h.allocate(32);
    h.print_allocated();
}

void demo_deallocation_no_coalescence_possible()
{
    std::cout << "[DEMO] Heap Deallocation - block coalescing impossible"
              << std::endl;
    Heap h(128);

    h.print_free();
    h.allocate(16);
    h.allocate(16);
    h.allocate(16);
    h.allocate(16);
    h.print_allocated();
    h.print_free();

    h.deallocate(16);
    h.deallocate(32);
    h.print_free(); // physical neighbours, but coalescing impossible

    h.deallocate(0); // coalescing
    h.print_free(); // coalescence is now unlocked
}
