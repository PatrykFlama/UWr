#include <bits/stdc++.h>
using namespace std;


class Cell {
public:
    int index, cellType, resources, myAnts, oppAnts;
    vector<int> neighbors;

    Cell(int index, int cellType, int resources, vector<int> neighbors, int myAnts = 0, int oppAnts = 0) {
        this->index = index;
        this->cellType = cellType;
        this->resources = resources;
        this->neighbors = neighbors;
        this->myAnts = myAnts;
        this->oppAnts = oppAnts;
    }
};

int main() {
    vector<Cell> cells;
    vector<int> myBases, oppBases;
    int numberOfCells;              // amount of hexagonal cells in this map
    int TOTAL_CRYSTALS = 0;
    int TOTAL_EGGS = 0;
    int TOTAL_MY_ANTS = 0;

    // ------------------ INIT INPUT ------------------
    cin >> numberOfCells; cin.ignore();
    for (int i = 0; i < numberOfCells; i++) {
        int type;                   // 0 for empty, 1 for eggs, 2 for crystal
        int initialResources;       // the initial amount of eggs/crystals on this cell
        int neigh0;                 // the index of the neighbouring cell for each direction
        int neigh1;
        int neigh2;
        int neigh3;
        int neigh4;
        int neigh5;
        cin >> type >> initialResources >> neigh0 >> neigh1 >> neigh2 >> neigh3 >> neigh4 >> neigh5; cin.ignore();
        Cell cell(i, type, (type == 0 ? 0 : initialResources), {neigh0, neigh1, neigh2, neigh3, neigh4, neigh5}, 0, 0);
        cells.push_back(cell);

        if(type == 1) TOTAL_EGGS += initialResources;
        else if(type == 2) TOTAL_CRYSTALS += initialResources;
    }
    
    int numberOfBases;
    cin >> numberOfBases; cin.ignore();
    for (int i = 0; i < numberOfBases; i++) {
        int myBaseIndex;
        cin >> myBaseIndex; cin.ignore();
        myBases.push_back(myBaseIndex);
    }
    for (int i = 0; i < numberOfBases; i++) {
        int oppBaseIndex;
        cin >> oppBaseIndex; cin.ignore();
        oppBases.push_back(oppBaseIndex);
    }

    // game loop
    while (true) {
        // ------------------ TURN INPUT ------------------
        for (int i = 0; i < numberOfCells; i++) {
            int resources;          // the current amount of eggs/crystals on this cell
            int myAnts;             // the amount of your ants on this cell
            int oppAnts;            // the amount of opponent ants on this cell
            cin >> resources >> myAnts >> oppAnts; cin.ignore();

            cells[i].resources = resources;
            cells[i].myAnts = myAnts;
            cells[i].oppAnts = oppAnts;
        }

        // ------------------ TURN LOGIC && OUTPUT ------------------
        for(auto i : cells) {
            if(i.cellType != 0) cout << "LINE " << myBases[0] << ' ' << i.index << ' ' << i.resources << ';';
        }
        cout << endl;
    }
}

