package Vehicle;

public class Car extends Vehicle{
    int max_speed;

    public Car(int capacity, float energy_usage, String energy_source, float efficiency, int max_speed) {
        super(capacity, energy_usage, energy_source, efficiency);
        this.max_speed = max_speed;
    }

    public String toString(){
        return  "capacity " + String.valueOf(capacity) + 
                "; energy usage " + String.valueOf(energy_usage) +
                "; energy source " + energy_source +
                "; efficiency " + String.valueOf(efficiency) +
                "; energy per person " + energy_usage/capacity +
                "; max speed " + String.valueOf(max_speed);
    }
}