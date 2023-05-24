#pragma once
#include <string>

//create interface here
class Storage
{
public:
    virtual void append(const std::string &entry) = 0;
    virtual ~Storage() = default;
};