#pragma once

#include "Address.hpp"
#include "solution/Storage.hpp"

#include <sstream>
#include <string>

class EventLog
{
public:
    EventLog(Storage& storage) : storage(storage)
    {
    }

    void log(const std::string& message)
    {
        std::stringstream buffer;
        buffer << "[" << (logsStored++) << "] " << message;
        storage.append(buffer.str());
    }

private:
    Storage &storage;
    unsigned logsStored = 0;
};
