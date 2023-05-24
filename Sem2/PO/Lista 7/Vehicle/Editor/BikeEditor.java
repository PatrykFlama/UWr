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

import Vehicle.Bike;


public class BikeEditor extends JComponent implements ActionListener {
    Bike obj;
    String file_name;

    JTextField capacity;       // people
    JTextField energy_usage;       // kWh/km
    JTextField energy_source;
    JTextField efficiency;
    JTextField bike_type;
    
    public BikeEditor(Bike obj, String file_name){
        this.obj = obj;
        this.file_name = file_name;

        capacity = new JTextField(String.valueOf(obj.capacity));     
        energy_usage = new JTextField(String.valueOf(obj.energy_usage));
        energy_source = new JTextField(obj.energy_source);
        efficiency = new JTextField(String.valueOf(obj.efficiency));
        bike_type = new JTextField(obj.bike_type);
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
        container.add(new JLabel("Bike type"));
        container.add(bike_type);

        JButton button = new JButton("SAVE");
        button.addActionListener(this);
        container.add(button);
    }

    public void actionPerformed(ActionEvent e) {
        obj.capacity = Integer.valueOf(capacity.getText());
        obj.efficiency = Float.valueOf(efficiency.getText());
        obj.energy_source = energy_source.getText();
        obj.energy_usage = Float.valueOf(energy_usage.getText());
        obj.bike_type = bike_type.getText();

        Bike.saveObj(obj, file_name);
    }
}
