#pragma once

#include "BetterCandidatesValidator.hpp"
#include "../Candidate.hpp"

class CooperationNeeded: public BetterCandidatesValidator
{
public:
    bool validate(const Candidate& c) const override
    {
    }
};
