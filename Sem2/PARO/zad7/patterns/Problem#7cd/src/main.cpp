#include <vector>
#include <iostream>

#include "Application.hpp"
#include "Candidate.hpp"
#include "solution/CooperationNeeded.hpp"
#include "solution/CppRequirements.hpp"
#include "solution/CRequirements.hpp"
#include "solution/WageRequirements.hpp"

int main()
{
    std::vector<Candidate> candidates{
        {"Adelajda", 15, 87, 0, 12000},
        {"Brunhilda", 85, 42, 1, 11000},
        {"Ciechosław", 97, 92, 1, 25000},
        {"Domażyr", 91, 45, 0, 10000}};

    std::unique_ptr<BetterCandidatesValidator> requirements = std::make_unique<CooperationNeeded>();
    requirements->add(std::make_unique<CppRequirements>(20));
    requirements->add(std::make_unique<CRequirements>(0));
    requirements->add(std::make_unique<WageRequirements>(15000));

    for (auto candidate: getFilteredCandidates(candidates, move(requirements)))
    {
        std::cout << "candidate " << candidate.name << " seems to fit" << std::endl;
    }
}
