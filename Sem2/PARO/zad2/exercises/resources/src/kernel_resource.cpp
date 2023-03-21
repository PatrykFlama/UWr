#include "resources/kernel_resource.h"
#include "resources/kernel.h"

#include <exception>
#include <iostream>
#include <map>
#include <random>

struct RESOURCE
{
    int timestamp; // time of allocation
    int usage_count = 0;
};

class Kernel
{
public:
    std::unique_ptr<KernelConfig> config;
    std::unique_ptr<KernelPolicy> policy;

    Kernel(std::unique_ptr<KernelConfig> config,
           std::unique_ptr<KernelPolicy> policy)
        : config(std::move(config)),
          policy(std::move(policy))
    {}

    ~Kernel() noexcept
    {
        if (not allocated_resources.empty()) {
            std::cout << "[KERNEL] WARNING: resources still allocated on exit. "
                      << std::endl;
        }

        if (policy) {
            for (auto const& resource : allocated_resources) {
                policy->handle_leaked_resource(resource.first);
            }
        }
    }

    RESOURCE* allocate()
    {
        current_time++;

        auto ptr = reinterpret_cast<RESOURCE*>(config->get_next_resource_id());
        if (config->is_allocation_successful()) {
            auto result = allocated_resources.emplace(ptr,
                                                      RESOURCE{current_time});

            if (result.second) {
                std::cout << "[KERNEL] RESOURCE " << ptr
                          << " allocation successful" << std::endl;
                return ptr;
            } else {
                std::cout << "[KERNEL] RESOURCE " << ptr
                          << " allocation failed - throw exception"
                          << std::endl;
                throw kernel_allocation_error();
            }
        }

        std::cout << "[KERNEL] RESOURCE " << ptr
                  << " allocation failed - return NULL" << std::endl;
        return nullptr;
    }

    void free(RESOURCE* p)
    {
        current_time++;

        if (not p) {
            std::cout << "[KERNEL] CRITICAL: Cannot free NULL resource!"
                      << std::endl;
            std::terminate();
        }

        auto resource = allocated_resources.find(p);

        if (allocated_resources.end() == resource) {
            std::cout << "[KERNEL] CRITICAL: Cannot free UNALLOCATED resource!"
                      << std::endl;
            std::terminate();
        }

        std::cout << "[KERNEL] RESOURCE " << p << " released. Lifetime = "
                  << (current_time - resource->second.timestamp)
                  << " ticks, utilized = " << resource->second.usage_count
                  << " time(s) in total" << std::endl;
        allocated_resources.erase(resource);
    }

    void do_the_trick(RESOURCE* p)
    {
        current_time++;

        if (not p) {
            std::cout << "[KERNEL] CRITICAL: Cannot operate on NULL resource!"
                      << std::endl;
            std::terminate();
        }

        auto resource = allocated_resources.find(p);

        if (allocated_resources.end() == resource) {
            std::cout
                << "[KERNEL] CRITICAL: Cannot operate on UNALLOCATED resource!"
                << std::endl;
            std::terminate();
        }

        std::cout << "[KERNEL] RESOURCE " << p << " usage requested."
                  << std::endl;
        resource->second.usage_count++;

        if (not config->is_operation_successful()) {
            throw kernel_operation_error();
        }
    }

private:
    std::map<RESOURCE*, RESOURCE> allocated_resources;

    int current_time = 0;
};

class RandomKernelConfig : public KernelConfig
{
public:
    RandomKernelConfig(int allocation_fail_ratio,
                       int operation_fail_ratio) noexcept
        : random_engine(std::random_device{}()),
          address_distrib(0x1000, 0x7FFF),
          allocation_success_distrib(1, allocation_fail_ratio),
          operation_success_distrib(1, operation_fail_ratio)
    {}

    int get_next_resource_id() noexcept override
    {
        return address_distrib(random_engine);
    }

    bool is_allocation_successful() noexcept override
    {
        return allocation_success_distrib(random_engine) != 1;
    }

    bool is_operation_successful() noexcept override
    {
        return operation_success_distrib(random_engine) != 1;
    }

private:
    std::minstd_rand random_engine;
    std::uniform_int_distribution<> address_distrib;
    std::uniform_int_distribution<> allocation_success_distrib;
    std::uniform_int_distribution<> operation_success_distrib;
};

class CrashKernelPolicy : public KernelPolicy
{
public:
    void handle_leaked_resource(RESOURCE* p) const override
    {
        std::cout << "[KERNEL] CRITICAL:  RESOURCE " << p
                  << " was not released!" << std::endl;
        std::terminate();
    }
};

static std::unique_ptr<Kernel> the_kernel = std::make_unique<Kernel>(
    std::make_unique<RandomKernelConfig>(4, 5),
    std::make_unique<CrashKernelPolicy>());

void reconfigure_kernel(std::unique_ptr<KernelConfig> cfg,
                        std::unique_ptr<KernelPolicy> policy)
{
    the_kernel = std::make_unique<Kernel>(std::move(cfg), std::move(policy));
}

void finalize_kernel() { the_kernel.reset(nullptr); }

RESOURCE* allocate_resource() { return the_kernel->allocate(); }
void free_resource(RESOURCE* p) { return the_kernel->free(p); }
void use_resource(RESOURCE* p) { the_kernel->do_the_trick(p); }
