#include <resources/kernel_resource.h>

#include <iostream>
#include <vector>

#include <memory> // std::unique_ptr

using Event = int;
using UniqueResource = std::unique_ptr<RESOURCE, decltype(&free_resource)>;

std::unique_ptr<Event> create_event(int event_id) noexcept { 
    return std::make_unique<Event>(event_id);
}

void process_init(UniqueResource resource) noexcept
try {
    if (resource) {
        use_resource(resource.get());
    }
} catch (std::runtime_error const& ex) {
    std::cout << ex.what() << std::endl;
}

void process_event(std::unique_ptr<Event> event,
                   UniqueResource temp_resource,
                   RESOURCE* proc_resource) noexcept
try {
    std::cout << "Processing Event(" << *event << ")" << std::endl;

    if (temp_resource) {
        use_resource(temp_resource.get());
    }

    if (proc_resource) {
        use_resource(proc_resource);
    }

    event.reset();
} catch (std::runtime_error const& ex) {
    std::cout << ex.what() << std::endl;
}

int main(int ac, char* av[])
{
    auto init_resource = UniqueResource(allocate_resource(), &free_resource);
    process_init(std::move(init_resource));

    auto proc_resource = UniqueResource(allocate_resource(), &free_resource);

    std::vector<int> simulation = {
        12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144};
    for (auto id : simulation) {
        auto event = create_event(id);
        auto temp_resource = UniqueResource(allocate_resource(), &free_resource);
        process_event(move(event), std::move(temp_resource), proc_resource.get());
    }

    return 0;
}
