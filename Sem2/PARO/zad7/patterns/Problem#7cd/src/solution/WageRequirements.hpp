#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class WageRequirements: public BetterCandidatesValidator
{
public:
    WageRequirements(unsigned maxPreferredWage)
        :maxPreferredWage(maxPreferredWage){}
    bool validate(const Candidate& c) const override
    {
        return c.preferredWage <= maxPreferredWage;
    }
private:
    unsigned maxPreferredWage;
};
