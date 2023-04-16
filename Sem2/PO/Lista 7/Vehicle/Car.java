package Vehicle;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;

import Vehicle.Editor.CarEditor;

public class Car extends Vehicle{
    public int max_speed;

    public Car(){ this(5, 25f, "electric", 0.8f, 180); }
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

    public static Car readObj(String file){
        try {
            FileInputStream fileInput = new FileInputStream(file);
            ObjectInputStream objectInput = new ObjectInputStream(fileInput);

            Car obj = (Car) objectInput.readObject();
            objectInput.close();
            fileInput.close();
            return obj;
        } catch (FileNotFoundException e) {
            System.out.println("File does not exist, created new object");
            return new Car();
        } catch (IOException e) {
            System.out.println("Error while reading data!");
        } catch (ClassNotFoundException e) {
            System.out.println("Car not found!");
        }

        return null;
    }

    public void Editor(String file_name){
        CarEditor editor = new CarEditor(this, file_name);
        editor.run();
    }
}