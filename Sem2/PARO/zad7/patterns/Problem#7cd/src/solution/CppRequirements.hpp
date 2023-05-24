#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CppRequirements: public BetterCandidatesValidator
{
public:
    CppRequirements(unsigned minCppRequirements)
        :minCppRequirements(minCppRequirements){}

    bool validate(const Candidate& c) const override
    {
        return c.cppFluency >= minCppRequirements;
    }
private:
    unsigned minCppRequirements;
};
