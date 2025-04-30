package List7;

import java.awt.*;

public interface Shape extends Cloneable {
    void draw(Graphics g);

    void setPosition(Point p);

    void move(int dx, int dy);

    boolean contains(Point p);

    Shape clone();
}
