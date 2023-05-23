#include <iostream>
#include <memory>

#include "Greeter.hpp"
#include "GreeterUpdater.hpp"

int main(){
    auto updater = std::make_unique<GreeterUpdater>();

    Greeter greeter(std::move(updater));
    greeter.greet();
}
