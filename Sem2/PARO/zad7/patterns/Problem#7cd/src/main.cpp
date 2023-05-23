#include <vector>
#include <iostream>

#include "Application.hpp"
#include "Candidate.hpp"

int main()
{
    std::vector<Candidate> candidates{
        {"Adelajda", 15, 87, 0, 12000},
        {"Brunhilda", 85, 42, 1, 11000},
        {"Ciechosław", 97, 92, 1, 25000},
        {"Domażyr", 91, 45, 0, 10000}};

    CandidatesValidator requirements(20, 0, 15000);

    for (auto candidate: getFilteredCandidates(candidates, requirements))
    {
        std::cout << "candidate " << candidate.name << " seems to fit" << std::endl;
    }
}
