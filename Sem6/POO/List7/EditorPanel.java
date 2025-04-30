package List7;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

class EditorPanel extends JPanel {
    private java.util.List<Shape> shapes = new ArrayList<>();
    private Deque<Memento> undoStack = new ArrayDeque<>();
    private Deque<Memento> redoStack = new ArrayDeque<>();
    private GraphicEditor.Mode mode;
    private Shape selectedShape;
    private Point lastPoint;
    private Map<GraphicEditor.Mode, Shape> prototypes = new HashMap<>();

    public EditorPanel() {
        setBackground(Color.WHITE);
        prototypes.put(GraphicEditor.Mode.CIRCLE, new Circle(40, Color.RED));
        prototypes.put(GraphicEditor.Mode.SQUARE, new Square(80, Color.GREEN));
        prototypes.put(GraphicEditor.Mode.RECTANGLE, new RectangleShape(120, 60, Color.BLUE));

        MouseAdapter ma = new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                lastPoint = e.getPoint();
                switch (mode) {
                    case CIRCLE:
                    case SQUARE:
                    case RECTANGLE:
                        Shape proto = prototypes.get(mode).clone();
                        proto.setPosition(lastPoint);
                        saveState(new AddCommand(proto));
                        shapes.add(proto);
                        repaint();
                        break;
                    case MOVE:
                        selectedShape = findShapeAt(lastPoint);
                        break;
                    case DELETE:
                        Shape d = findShapeAt(lastPoint);
                        if (d != null) {
                            saveState(new DeleteCommand(d));
                            shapes.remove(d);
                            repaint();
                        }
                        break;
                }
            }

            public void mouseDragged(MouseEvent e) {
                if (mode == GraphicEditor.Mode.MOVE && selectedShape != null) {
                    Point p = e.getPoint();
                    int dx = p.x - lastPoint.x, dy = p.y - lastPoint.y;
                    saveState(new MoveCommand(selectedShape, dx, dy));
                    selectedShape.move(dx, dy);
                    lastPoint = p;
                    repaint();
                }
            }

            public void mouseReleased(MouseEvent e) {
                selectedShape = null;
            }
        };
        addMouseListener(ma);
        addMouseMotionListener(ma);
    }

    public void setMode(GraphicEditor.Mode m) {
        this.mode = m;
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        shapes.forEach(s -> s.draw(g));
    }

    private Shape findShapeAt(Point p) {
        for (int i = shapes.size() - 1; i >= 0; i--) {
            if (shapes.get(i).contains(p))
                return shapes.get(i);
        }
        return null;
    }

    private void saveState(Command cmd) {
        Memento m = cmd.createMemento();
        undoStack.push(m);
        redoStack.clear();
    }

    public void undo() {
        if (!undoStack.isEmpty()) {
            Memento m = undoStack.pop();
            m.undo();
            redoStack.push(m);
            repaint();
        }
    }

    public void redo() {
        if (!redoStack.isEmpty()) {
            Memento m = redoStack.pop();
            m.redo();
            undoStack.push(m);
            repaint();
        }
    }

    // --- Command & Memento ---
    interface Command {
        Memento createMemento();
    }

    abstract class Memento {
        abstract void undo();

        abstract void redo();
    }

    class AddCommand implements Command {
        private Shape shape;

        AddCommand(Shape s) {
            shape = s;
        }

        public Memento createMemento() {
            return new Memento() {
                public void undo() {
                    shapes.remove(shape);
                }

                public void redo() {
                    shapes.add(shape);
                }
            };
        }
    }

    class DeleteCommand implements Command {
        private Shape shape;
        int idx;

        DeleteCommand(Shape s) {
            shape = s;
            idx = shapes.indexOf(s);
        }

        public Memento createMemento() {
            return new Memento() {
                public void undo() {
                    shapes.add(idx, shape);
                }

                public void redo() {
                    shapes.remove(shape);
                }
            };
        }
    }

    class MoveCommand implements Command {
        private Shape shape;
        int dx, dy;

        MoveCommand(Shape s, int dx, int dy) {
            shape = s;
            this.dx = dx;
            this.dy = dy;
        }

        public Memento createMemento() {
            return new Memento() {
                public void undo() {
                    shape.move(-dx, -dy);
                }

                public void redo() {
                    shape.move(dx, dy);
                }
            };
        }
    }
}
