#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CooperationNeeded: public BetterCandidatesValidator
{
public:
    CooperationNeeded() = default;
    
    bool validate(const Candidate& c) const override
    {
        return c.cooperative;
    }
};
