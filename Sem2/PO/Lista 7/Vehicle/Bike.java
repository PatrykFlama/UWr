package Vehicle;

public class Bike extends Vehicle{
    String bike_type;

    public Bike(int capacity, float energy_usage, String energy_source, float efficiency, String type) {
        super(capacity, energy_usage, energy_source, efficiency);
        bike_type = type;
    }

    public String toString(){
        return  "capacity " + String.valueOf(capacity) + 
                "; energy usage " + String.valueOf(energy_usage) +
                "; energy source " + energy_source +
                "; efficiency " + String.valueOf(efficiency) +
                "; energy per person " + energy_usage/capacity +
                "; bike type " + bike_type;
    }
}
