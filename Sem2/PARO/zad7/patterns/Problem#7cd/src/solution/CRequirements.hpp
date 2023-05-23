#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CRequirements: public BetterCandidatesValidator
{
public:
    CRequirements(unsigned minCRequirements)
        :minCRequirements(minCRequirements){}

    bool validate(const Candidate& c) const override
    {
        return c.cFluency >= minCRequirements;
    }
private:
    unsigned minCRequirements;
};
