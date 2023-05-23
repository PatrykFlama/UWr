#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CppRequirements: public BetterCandidatesValidator
{
public:
    bool validate(const Candidate& c) const override
    {
    }
private:
    unsigned minCppRequirements;
};
