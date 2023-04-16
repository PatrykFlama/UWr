package Vehicle;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import Vehicle.Editor.VehicleEditor;

public class Vehicle implements Serializable {
    private static final long serialVersionUID = 1L;

    public int capacity;       // people
    public float energy_usage;       // kWh/km
    public String energy_source;
    public float efficiency;

    public Vehicle(){ this(0, 0f, "", 1f); }
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

    public static void saveObj(Vehicle obj, String file){
        try{
            FileOutputStream fileOutput = new FileOutputStream(file);
            ObjectOutputStream objectOutput = new ObjectOutputStream(fileOutput);

            objectOutput.writeObject(obj);
            objectOutput.close();
            fileOutput.close();
        } catch (IOException e){
            System.out.println("Couldn't save object!");
        }
    }

    public static Vehicle readObj(String file){
        try {
            FileInputStream fileInput = new FileInputStream(file);
            ObjectInputStream objectInput = new ObjectInputStream(fileInput);

            Vehicle obj = (Vehicle) objectInput.readObject();
            objectInput.close();
            fileInput.close();
            return obj;
        } catch (FileNotFoundException e) {
            System.out.println("File does not exist, created new object");
            return new Vehicle();
        } catch (IOException e) {
            System.out.println("Error while reading data!");
        } catch (ClassNotFoundException e) {
            System.out.println("Vehicle not found!");
        }

        return null;
    }

    public void Editor(String file_name){
        VehicleEditor editor = new VehicleEditor(this, file_name);
        editor.run();
    }
}
