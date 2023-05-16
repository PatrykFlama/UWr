#pragma once
#include "../BankAccount.hpp"

//define interface here
class Debitable {
public:
    virtual bool debit(unsigned amount) = 0;
    virtual ~Debitable() = default;
};
