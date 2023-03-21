#include <resources/kernel_resource.h>

#include <stdexcept> // std::runtime_error

void kernel_stress_test()
{
    // will return NULL if:
    // 1. allocation was not successful
    // will THROW std::runtime_error if:
    // 1. sometimes, due to internal reasons (wrap in try-catch!)
    RESOURCE* resource = allocate_resource();

    // will THROW std::runtime_error if:
    // 1. sometimes, with no particular reason (wrap in try-catch!)
    // will CRASH (std::terminate) if:
    // 1. resource is NULL (add NULL-check!)
    // 2. resource was already released
    use_resource(resource);

    // will CRASH (std::terminate) if:
    // 1. resource is NULL (add NULL-check!)
    // 2. resource is already released
    free_resource(resource);
}

int main(int ac, char* av[])
{
    constexpr int repetitions = 32;

    for (int i = 0; i < repetitions; ++i) { kernel_stress_test(); }

    // the program will CRASH (std::terminate) if:
    // 1. there are resources not yet released on exit
    return 0;
}
