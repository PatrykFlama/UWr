#pragma once
#include "../BankAccount.hpp"

//define interface here
class MoneyTransferTarget {
public:
    virtual void credit(unsigned deposit) = 0;
    virtual ~MoneyTransferTarget() = default;
};
