package Vehicle.Editor;

import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

import Vehicle.Car;


public class CarEditor extends JComponent implements ActionListener {
    Car obj;
    String file_name;

    JTextField capacity;       // people
    JTextField energy_usage;       // kWh/km
    JTextField energy_source;
    JTextField efficiency;
    JTextField max_speed;
    
    public CarEditor(Car obj, String file_name){
        this.obj = obj;
        this.file_name = file_name;

        capacity = new JTextField(String.valueOf(obj.capacity));     
        energy_usage = new JTextField(String.valueOf(obj.energy_usage));
        energy_source = new JTextField(obj.energy_source);
        efficiency = new JTextField(String.valueOf(obj.efficiency));
        max_speed = new JTextField(String.valueOf(obj.max_speed));
    }

    public void run(){
        JFrame frame = new JFrame("Vehicle editor");
        frame.setSize(200, 200);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);
        
        
        Container cont = frame.getContentPane();
        cont.setLayout(new GridLayout(6, 2));
        setContainer(cont);

        frame.setVisible(true);
    }
    
    void setContainer(Container container){
        container.add(new JLabel("Capacity"));
        container.add(capacity);
        container.add(new JLabel("Energy usage"));
        container.add(energy_usage);
        container.add(new JLabel("Energy source"));
        container.add(energy_source);
        container.add(new JLabel("Efficiency"));
        container.add(efficiency);
        container.add(new JLabel("Max speed"));
        container.add(max_speed);

        JButton button = new JButton("SAVE");
        button.addActionListener(this);
        container.add(button);
    }

    public void actionPerformed(ActionEvent e) {
        obj.capacity = Integer.valueOf(capacity.getText());
        obj.efficiency = Float.valueOf(efficiency.getText());
        obj.energy_source = energy_source.getText();
        obj.energy_usage = Float.valueOf(energy_usage.getText());
        obj.max_speed = Integer.valueOf(max_speed.getText());

        Car.saveObj(obj, file_name);
    }
}
