/*
Patryk Flama
Lista 6
kompilacja: javac zad.java; java zad; rm *.class
*/


/* #region //* Vehicle */
class Vehicle{
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

class Bike extends Vehicle{
    public Bike(int capacity, float energy_usage, String energy_source, float efficiency) {
        super(capacity, energy_usage, energy_source, efficiency);
    }
}

class Car extends Vehicle{
    public Car(int capacity, float energy_usage, String energy_source, float efficiency) {
        super(capacity, energy_usage, energy_source, efficiency);
    }
}

class Tram extends Vehicle{
    public Tram(int capacity, float energy_usage, String energy_source, float efficiency) {
        super(capacity, energy_usage, energy_source, efficiency);
    }
}
/* #endregion */

/* #region //* Figure */
class Point{
    float x, y;

    public Point(){
        this(0f, 0f);
    }
    public Point(float _x, float _y){
        x = _x;
        y = _y;
    }

    public String toString(){
        return String.valueOf(x) + " " + String.valueOf(y);
    }
}

class Figure{
    float area, circuit;
    Point[] verticies;

    public Figure(){
        area = 0;
        circuit = 0;
        verticies = new Point[0];
    }

    public String toString(){
        String res = "area " + String.valueOf(area) + "; circuit " + String.valueOf(circuit) + "; points ";
        for(Point el : verticies) res += el.toString() + "; ";
        return res;
    }
}

class Circle extends Figure{
    public Circle(float r){
        this(r, new Point(0f, 0f));
    }
    public Circle(float r, Point pos){
        area = (float)Math.PI * r*r;
        circuit = 2 * (float)Math.PI * r;
        verticies = new Point[1];
        verticies[0] = pos;
    }
}

class Triangle extends Figure{
    public Triangle(Point a, Point b, Point c){
        verticies = new Point[]{a, b, c};
        area = clac_area();
        circuit = calc_circuit();
    }

    float clac_area(){
        return Math.abs(
            verticies[0].x * verticies[1].y +
            verticies[1].x * verticies[2].y +
            verticies[2].x * verticies[0].y -
            verticies[0].y * verticies[1].x -
            verticies[1].y * verticies[2].x -
            verticies[2].y * verticies[0].x
        ) / 2;
    }

    float calc_circuit(){
        return (float)(
            Math.sqrt(Math.pow((double)(verticies[0].x - verticies[1].x), 2) +
                      Math.pow((double)(verticies[0].y - verticies[1].y), 2)) +
            Math.sqrt(Math.pow((double)(verticies[1].x - verticies[2].x), 2) +
                      Math.pow((double)(verticies[1].y - verticies[2].y), 2)) +
            Math.sqrt(Math.pow((double)(verticies[2].x - verticies[0].x), 2) +
                      Math.pow((double)(verticies[2].y - verticies[0].y), 2)));
    }
}
/* #endregion */



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
        Bike bike_human_power = new Bike(1, 0.1f, "food", 0.8f);
        Bike bike_electric    = new Bike(1, 0.5f, "electric", 0.9f);
        Car car_electric = new Car(5, 30f, "electric", 0.7f);
        Car car_petrol   = new Car(5, 120f, "electric", 0.3f);
        Tram tram = new Tram(150, 3f, "electric", 0.85f);

        System.out.println(bike_human_power.toString());
        System.out.println(bike_electric.toString());
        System.out.println(car_electric.toString());
        System.out.println(car_petrol.toString());
        System.out.println(tram.toString());
    }
}
