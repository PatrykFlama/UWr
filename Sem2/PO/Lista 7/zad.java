import Figure.*;
import Vehicle.*;

public class zad{
    public static void main(String[] args){
        testFigure();
        testVehicle();
    }

    static void testFigure(){
        Circle circ = new Circle(1, new Point(1, 2));
        Triangle trig = new Triangle(new Point(0, 0), new Point(1, 0), new Point(0, 1));

        System.out.println(circ.toString());
        System.out.println(trig.toString());
    }

    static void testVehicle(){
        Bike bike_human_power = new Bike(1, 0.1f, "food", 0.8f, "MTB");
        Bike bike_electric    = new Bike(1, 0.5f, "electric", 0.9f, "uphill");
        Car car_electric = new Car(5, 30f, "electric", 0.7f, 300);
        Car car_petrol   = new Car(5, 120f, "electric", 0.3f, 80);
        Tram tram = new Tram(150, 3f, "electric", 0.85f, "A");

        System.out.println(bike_human_power.toString());
        System.out.println(bike_electric.toString());
        System.out.println(car_electric.toString());
        System.out.println(car_petrol.toString());
        System.out.println(tram.toString());
    }
}
