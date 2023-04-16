/*
Patryk Flama
Lista 7
Kompilacja: javac *.java; java zad 
*/

// import Figure.*;
import Vehicle.*;

public class zad{
    public static void main(String[] args){
        // testFigure();
        // testVehicle();

        if(args.length < 2){
            System.out.println("Too few arguments!");
            System.out.println("Proper use: [file name] [object name]");
            return;
        }

        String file_name = args[0];
        String object_name = args[1];
        Vehicle obj;

        switch (object_name) {
            case "Vehicle":
                obj = Vehicle.readObj(file_name);
                break;
            case "Car":
                obj = Car.readObj(file_name);
                break;
            case "Bike":
                obj = Bike.readObj(file_name);
                break;

            default:
                System.out.println("Given class doesn't exist!");
                return;
        }

        obj.Editor(file_name);
    }

    // static void testFigure(){
    //     Circle circ = new Circle(1, new Point(1, 2));
    //     Triangle trig = new Triangle(new Point(0, 0), new Point(1, 0), new Point(0, 1));

    //     System.out.println(circ.toString());
    //     System.out.println(trig.toString());
    // }

    static void testVehicle(){
        Bike bike_human_power = new Bike(1, 0.1f, "food", 0.8f, "MTB");
        Bike bike_electric    = new Bike(1, 0.5f, "electric", 0.9f, "uphill");
        Car car_electric = new Car(5, 30f, "electric", 0.7f, 300);
        Car car_petrol   = new Car(5, 120f, "electric", 0.3f, 80);

        Bike.saveObj(bike_human_power,"bike");
        Car.saveObj(car_petrol, "car");
        bike_human_power = Bike.readObj("bike");
        car_petrol = Car.readObj("car");

        System.out.println(bike_human_power.toString());
        System.out.println(bike_electric.toString());
        System.out.println(car_electric.toString());
        System.out.println(car_petrol.toString());
    }
}
