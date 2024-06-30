import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;

public class TankTest extends JFrame {
    private JPanel controlPanel;
    private JPanel drawingPanel;
    private JComboBox<String> colorComboBox;
    private JSpinner cannonSpinner;

    private Color[] colors = {Color.BLACK, Color.RED, Color.GREEN, Color.BLUE, Color.YELLOW, Color.ORANGE};
    private String[] colorNames = {"Чёрный", "Красный", "Зелёный", "Синий", "Жёлтый", "Оранжевый"};
    private int cannonCount = 1;
    private Color tankColor = Color.BLACK;

    public TankTest() {
        setTitle("Tank");
        setSize(1200, 750);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        controlPanel = new JPanel();
        controlPanel.setLayout(new FlowLayout());

        colorComboBox = new JComboBox<>(colorNames);
        colorComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int selectedIndex = colorComboBox.getSelectedIndex();
                tankColor = colors[selectedIndex];
                drawingPanel.repaint();
            }
        });
        controlPanel.add(new JLabel("Цвет танка:"));
        controlPanel.add(colorComboBox);

        SpinnerModel cannonModel = new SpinnerNumberModel(1, 1, 10, 1);
        cannonSpinner = new JSpinner(cannonModel);
        cannonSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                cannonCount = (int) cannonSpinner.getValue();
                drawingPanel.repaint();
            }
        });
        controlPanel.add(new JLabel("Количество пушек:"));
        controlPanel.add(cannonSpinner);

        add(controlPanel, BorderLayout.NORTH);

        drawingPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                drawTank(g);
            }

            private void drawTank(Graphics g) {
                Graphics2D g2d = (Graphics2D) g;
                g2d.setColor(tankColor);

                g2d.fillRect(50, 150, 600, 350);

                int x = 50 + 600;
                int y = 200;
                for (int i = 0; i < cannonCount; i++) {
                    g2d.setStroke(new BasicStroke(5.0f));
                    g2d.drawLine(x, y, x + 200, y - 50);

                    y += 25;
                }
            }
        };
        add(drawingPanel, BorderLayout.CENTER);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                new TankDrawingApp().setVisible(true);
            }
        });
    }
}
