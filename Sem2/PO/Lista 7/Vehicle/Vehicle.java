package Vehicle;
import java.io.Serializable;

public class Vehicle implements Serializable {
    int capacity;       // people
    float energy_usage;       // kWh/km
    String energy_source;
    float efficiency;

    public Vehicle(int capacity, float energy_usage, 
                   String energy_source, float efficiency){
        this.capacity = capacity;
        this.energy_usage = energy_usage;
        this.energy_source = energy_source;
        this.efficiency = efficiency;
    }

    public String toString(){
        return  "capacity " + String.valueOf(capacity) + 
                "; energy usage " + String.valueOf(energy_usage) +
                "; energy source " + energy_source +
                "; efficiency " + String.valueOf(efficiency) +
                "; energy per person " + energy_usage/capacity;
    }
}
