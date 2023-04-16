package Vehicle;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;

public class Bike extends Vehicle{
    String bike_type;

    public Bike(){ this(1, 0.5f, "electric", 0.9f, "city"); }
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

    public static Bike readObj(String file){
        try {
            FileInputStream fileInput = new FileInputStream(file);
            ObjectInputStream objectInput = new ObjectInputStream(fileInput);

            Bike obj = (Bike) objectInput.readObject();
            objectInput.close();
            fileInput.close();
            return obj;
        } catch (FileNotFoundException e) {
            System.out.println("Could not open " + file);
        } catch (IOException e) {
            System.out.println("Error while reading data!");
        } catch (ClassNotFoundException e) {
            System.out.println("Bike not found!");
        }

        return new Bike();
    }

    // TODO: add object edition
}
