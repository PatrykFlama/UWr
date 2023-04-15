package Figure;
import java.io.Serializable;

public class Point implements Serializable {
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