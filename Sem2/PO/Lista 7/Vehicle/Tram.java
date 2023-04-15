package Vehicle;

public class Tram extends Vehicle{
    String line;

    public Tram(int capacity, float energy_usage, String energy_source, float efficiency, String line) {
        super(capacity, energy_usage, energy_source, efficiency);
        this.line = line;
    }

    public String toString(){
        return  "capacity " + String.valueOf(capacity) + 
                "; energy usage " + String.valueOf(energy_usage) +
                "; energy source " + energy_source +
                "; efficiency " + String.valueOf(efficiency) +
                "; energy per person " + energy_usage/capacity +
                "; line " + line;
    }
}
