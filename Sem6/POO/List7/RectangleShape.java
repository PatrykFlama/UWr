package List7;

import java.awt.*;

public class RectangleShape implements Shape {
    private int width, height;
    private Color color;
    private Point topLeft;

    public RectangleShape(int w, int h, Color c) {
        width = w;
        height = h;
        color = c;
        topLeft = new Point();
    }

    public void draw(Graphics g) {
        g.setColor(color);
        g.fillRect(topLeft.x, topLeft.y, width, height);
    }

    public void setPosition(Point p) {
        topLeft = new Point(p.x - width / 2, p.y - height / 2);
    }

    public void move(int dx, int dy) {
        topLeft.translate(dx, dy);
    }

    public boolean contains(Point p) {
        return p.x >= topLeft.x && p.x <= topLeft.x + width &&
                p.y >= topLeft.y && p.y <= topLeft.y + height;
    }

    public RectangleShape clone() {
        return new RectangleShape(width, height, color);
    }
}
