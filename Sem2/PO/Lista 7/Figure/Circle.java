package Figure;

public class Circle extends Figure{
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
