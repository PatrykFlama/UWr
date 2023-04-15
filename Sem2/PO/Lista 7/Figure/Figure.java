package Figure;
import java.io.Serializable;

public class Figure implements Serializable {
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
