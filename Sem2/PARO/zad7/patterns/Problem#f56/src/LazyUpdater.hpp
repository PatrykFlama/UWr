#pragma once

#include <iostream>
#include <memory>

#include "Updater.hpp"
#include "GreeterUpdater.hpp"

class LazyUpdater : public Updater{
    std::unique_ptr<GreeterUpdater> updater = nullptr;
public:
    LazyUpdater(){}

    void checkForUpdates(){
        if(!updater) updater = std::make_unique<GreeterUpdater>();
        updater->checkForUpdates();
    }
};


