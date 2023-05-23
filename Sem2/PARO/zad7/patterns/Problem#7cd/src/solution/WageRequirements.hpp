#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class WageRequirements: public BetterCandidatesValidator
{
public:
    bool validate(const Candidate& c) const override
    {
    }
private:
    unsigned maxPreferredWage;
};
