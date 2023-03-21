#include "logger.hpp"

#include <iostream>

#define FMT_HEADER_ONLY // no need to #undef, we are in cpp file
#include <fmt/format.h>

namespace logger
{
void log_out_of_memory_max_size_exceeded(int requested_size,
                                         std::size_t max_size)
{
    // there is no block greater than 2 to the power of free_list size
    std::cout << fmt::format("Allocate request {}B failed - out of memory! "
                             "Max block size: {}B",
                             requested_size,
                             (1 << max_size))
              << std::endl;
}

void log_out_of_memory_block_truncated(int requested_size,
                                       int truncated_block_size)
{
    // block is truncated when heap size is not exactly the power of 2
    std::cout << fmt::format("Allocate request {}B failed - out of memory! "
                             "Last block truncated to {}B.",
                             requested_size,
                             truncated_block_size)
              << std::endl;
}

void log_allocation_success(int requested_size,
                            std::pair<int, int> const& block)
{
    auto block_size = block.second - block.first + 1;
    std::cout << fmt::format(
        "Allocate request {}B success - memory block ({}-{}) allocated.",
        requested_size,
        block.first,
        block.second);
    if (block_size > requested_size) {
        std::cout << fmt::format("Note: {}B wasted due to block alignment.",
                                 block_size - requested_size);
    }
    std::cout << std::endl;
}

void log_out_of_memory(int requested_size)
{
    std::cout << fmt::format("Sorry, failed to allocate memory."
                             " Block suitable for {}B not available.",
                             requested_size)
              << std::endl;
}

void log_deallocation_success(std::pair<int, int> const& block)
{
    std::cout << fmt::format(
                     "Deallocate success - memory block ({0}-{1}) freed.",
                     block.first,
                     block.second)
              << std::endl;
}

void log_coalescence(std::pair<int, int> a,
                     std::pair<int, int> b,
                     std::pair<int, int> const& r)
{
    if (a.first > b.first) {
        std::swap(a, b); // sort by block begin for nicer output
    }

    std::cout << fmt::format("Buddies found: ({}-{})({}-{}). ",
                             a.first,
                             a.second,
                             b.first,
                             b.second)
              << fmt::format("Coalescing to ({}-{})", r.first, r.second)
              << std::endl;
}

void log_coalescence_stop(std::pair<int, int> const& block)
{
    std::cout << fmt::format(
                     "No free buddy found for block: ({}-{}). Coalescing stop.",
                     block.first,
                     block.second)
              << std::endl;
}

void log_invalid_deallocate_request(int address)
{
    std::cout << fmt::format("Sorry, invalid free request: {}", address)
              << std::endl;
}

} // namespace logger
