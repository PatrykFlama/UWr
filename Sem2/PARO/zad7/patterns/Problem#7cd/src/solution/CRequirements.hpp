#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CRequirements: public BetterCandidatesValidator
{
public:
    bool validate(const Candidate& c) const override
    {
    }
private:
    unsigned minCRequirements;
};
