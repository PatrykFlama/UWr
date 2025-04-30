package List7;

import java.awt.*;

public class Square implements Shape {
    private int side;
    private Color color;
    private Point topLeft;

    public Square(int s, Color c) {
        side = s;
        color = c;
        topLeft = new Point();
    }

    public void draw(Graphics g) {
        g.setColor(color);
        g.fillRect(topLeft.x, topLeft.y, side, side);
    }

    public void setPosition(Point p) {
        topLeft = new Point(p.x - side / 2, p.y - side / 2);
    }

    public void move(int dx, int dy) {
        topLeft.translate(dx, dy);
    }

    public boolean contains(Point p) {
        return p.x >= topLeft.x && p.x <= topLeft.x + side &&
                p.y >= topLeft.y && p.y <= topLeft.y + side;
    }

    public Square clone() {
        return new Square(side, color);
    }
}
