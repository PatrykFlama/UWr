#include "Heap.hpp"

#include <iostream>

#define FMT_HEADER_ONLY // no need to #undef, we are in cpp file
#include <fmt/format.h>

void Heap::print_allocated()
{
    std::cout << "Heap allocations: ";
    for (auto const& block : this->mp) {
        std::cout << fmt::format("[{}B:({}-{})]",
                                 block.second,
                                 block.first,
                                 block.first + block.second - 1);
    }
    std::cout << std::endl;
}

void Heap::print_free()
{
    std::cout << "Heap availability: ";
    for (int i = 0; i < free_list.size(); ++i) {
        std::cout << fmt::format("[{:d}B:", 1 << i);
        for (auto const& block : free_list[i])
            std::cout << fmt::format("({}-{})", block.first, block.second);
        std::cout << "]";
    }
    std::cout << std::endl;
}

void Heap::print_memory()
{
    constexpr int width = 16; // line width
    constexpr int word = 4; // word marker

    std::cout << "     ";
    for (int i = 0; i < width; ++i) {
        if (!(i % word)) {
            std::cout << fmt::format("{:2x} ", i);
        } else {
            std::cout << "   ";
        }
    }

    for (int i = 0; i < _sz; ++i) {
        if (!(i % width)) {
            std::cout << fmt::format("\n{:#04x}:", i);
        }
        std::cout << fmt::format(
            "{:2x} ", static_cast<std::uint8_t>(((char*)_memory)[i]));
    }
    std::cout << std::endl;
}
