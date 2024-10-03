#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;



int main()
{
    int podid=0;

    // game loop
    while (1) {
        int resources;
        cin >> resources; cin.ignore();

        int num_travel_routes;
        cin >> num_travel_routes; cin.ignore();
        for (int i = 0; i < num_travel_routes; i++) {
            int building_id_1;
            int building_id_2;
            int capacity;
            cin >> building_id_1 >> building_id_2 >> capacity; cin.ignore();
        }

        int num_pods;
        cin >> num_pods; cin.ignore();
        cerr << "-----------\n";
        for (int i = 0; i < num_pods; i++) {
            string pod_properties;
            getline(cin, pod_properties);
            cerr << pod_properties << '\n';
        }

        int num_new_buildings;
        cin >> num_new_buildings; cin.ignore();
        for (int i = 0; i < num_new_buildings; i++) {
            string building_properties;
            getline(cin, building_properties);
        }

        // Write an action using cout. DON'T FORGET THE "<< endl"
        // To debug: cerr << "Debug messages..." << endl;

        podid++;
        cout << "TUBE 0 1;TUBE 0 2;POD "<<podid<<" 0 1 0 2 0 1 0 2" << endl; // TUBE | UPGRADE | TELEPORT | POD | DESTROY | WAIT
    }
}