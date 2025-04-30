package List7;

import javax.swing.*;
import java.awt.*;

public class GraphicEditor extends JFrame {
    public enum Mode {
        CIRCLE, SQUARE, RECTANGLE, MOVE, DELETE
    }

    private Mode currentMode = Mode.CIRCLE;
    private EditorPanel canvas;

    public GraphicEditor() {
        super("Simplified Graphic Editor");
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setSize(800, 600);

        JToolBar toolbar = new JToolBar();
        addButton(toolbar, "Circle", () -> currentMode = Mode.CIRCLE);
        addButton(toolbar, "Square", () -> currentMode = Mode.SQUARE);
        addButton(toolbar, "Rectangle", () -> currentMode = Mode.RECTANGLE);
        addButton(toolbar, "Move", () -> currentMode = Mode.MOVE);
        addButton(toolbar, "Delete", () -> currentMode = Mode.DELETE);
        addButton(toolbar, "Undo", () -> canvas.undo());
        addButton(toolbar, "Redo", () -> canvas.redo());
        add(toolbar, BorderLayout.NORTH);

        canvas = new EditorPanel();
        canvas.setMode(currentMode);
        add(canvas, BorderLayout.CENTER);
    }

    private void addButton(JToolBar tb, String name, Runnable action) {
        JButton btn = new JButton(name);
        btn.addActionListener(_ -> {
            action.run();
            canvas.setMode(currentMode);
        });
        tb.add(btn);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            new GraphicEditor().setVisible(true);
        });
    }
}
