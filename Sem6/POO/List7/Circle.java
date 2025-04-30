package List7;

import java.awt.*;

public class Circle implements Shape {
    private int radius;
    private Color color;
    private Point center;

    public Circle(int r, Color c) {
        radius = r;
        color = c;
        center = new Point();
    }

    public void draw(Graphics g) {
        g.setColor(color);
        g.fillOval(center.x - radius, center.y - radius, radius * 2, radius * 2);
    }

    public void setPosition(Point p) {
        center = new Point(p);
    }

    public void move(int dx, int dy) {
        center.translate(dx, dy);
    }

    public boolean contains(Point p) {
        return center.distance(p) <= radius;
    }

    public Circle clone() {
        return new Circle(radius, color);
    }
}
