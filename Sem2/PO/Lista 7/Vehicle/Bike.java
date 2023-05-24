package Vehicle;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;

import Vehicle.Editor.BikeEditor;

public class Bike extends Vehicle{
    public String bike_type;

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
            System.out.println("File does not exist, created new object");
            return new Bike();
        } catch (IOException e) {
            System.out.println("Error while reading data!");
        } catch (ClassNotFoundException e) {
            System.out.println("Bike not found!");
        }

        return null;
    }

    public void Editor(String file_name){
        BikeEditor editor = new BikeEditor(this, file_name);
        editor.run();
    }
}
