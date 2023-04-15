package Figure;

public class Triangle extends Figure{
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