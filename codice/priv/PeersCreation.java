import java.awt.EventQueue;
import java.awt.Font;

import javax.swing.JFrame;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import javax.swing.table.TableColumn;

import javax.swing.JFileChooser;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.nio.file.Paths;




public class PeersCreation {

    private JFrame _frame;
    private JTable _table = new MyTable();
    private JTextField _variables;
    private JTextField _neighbors;
    private JTextField _init;
    private JTextField _loop;

    private final Font _labelsFont = new Font("Arial", Font.BOLD, 17);
    private final Font _tableHeaderAndLabelFont = new Font("Arial", Font.BOLD, 20);
    private final Font _tableCellsFont = new Font("Arial", Font.PLAIN, 17);

    private final String FILE_NAME = ".starting_peers.txt";




    public static void main(String[] args) {
        EventQueue.invokeLater(new Runnable() {
            public void run() {
                try {
                    PeersCreation window = new PeersCreation();
                    window._frame.setVisible(true);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });
    }


    public PeersCreation() {
        initializeFrame();
        DefaultTableModel model = initializeTable();
        initializeTextFields();
        initializeLabels();
        initializeScrollPane();
        initializeButtons(model);
        writePlaceHoldersInLabels();
    }



    private void initializeFrame() {
        _frame = new JFrame("Initial peers");
        _frame.setBounds(430, 200, 1000, 600);
        _frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        _frame.getContentPane().setLayout(null);
        _frame.setResizable(false);
    }


    private DefaultTableModel initializeTable() {
        Object[] colomns = {
            "ID",
            "Variables",
            "Neighbors",
            "Init",
            "Loop"
        };
        DefaultTableModel model = new DefaultTableModel();
        model.setColumnIdentifiers(colomns);
        _table.setModel(model);
        _table.getTableHeader().setReorderingAllowed(false);
        _table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        _table.getColumnModel().getColumn(0).setPreferredWidth(60);
        _table.getColumnModel().getColumn(1).setPreferredWidth(155);
        _table.getColumnModel().getColumn(2).setPreferredWidth(140);
        _table.getColumnModel().getColumn(3).setPreferredWidth(310);
        _table.getColumnModel().getColumn(4).setPreferredWidth(310);
        _table.setRowHeight(30);
        _table.getTableHeader().setFont(_tableHeaderAndLabelFont);
        _table.setFont(_tableCellsFont);
        return model;
    }


    private void initializeTextFields() {
        _variables = new JTextField();
        _variables.setFont(new Font("Arial", Font.PLAIN, 20));
        _variables.setBounds(10, 11, 600, 30);
        _frame.getContentPane().add(_variables);
        _variables.setColumns(10);

        _neighbors = new JTextField();
        _neighbors.setFont(new Font("Arial", Font.PLAIN, 20));
        _neighbors.setBounds(10, 47, 600, 30);
        _frame.getContentPane().add(_neighbors);
        _neighbors.setColumns(10);

        _init = new JTextField();
        _init.setFont(new Font("Arial", Font.PLAIN, 20));
        _init.setBounds(10, 83, 600, 30);
        _frame.getContentPane().add(_init);
        _init.setColumns(10);

        _loop = new JTextField();
        _loop.setFont(new Font("Arial", Font.PLAIN, 20));
        _loop.setBounds(10, 119, 600, 30);
        _frame.getContentPane().add(_loop);
        _loop.setColumns(10);
    }


    private void initializeLabels() {
        JLabel lblName = new JLabel("Variables");
        lblName.setBounds(630, 11, 660, 35);
        lblName.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(lblName);

        JLabel lblPassword = new JLabel("Neighbors");
        lblPassword.setBounds(630, 42, 660, 42);
        lblPassword.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(lblPassword);

        JLabel lblEmpId = new JLabel("Init");
        lblEmpId.setBounds(630, 73, 660, 50);
        lblEmpId.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(lblEmpId);

        JLabel lblEmpSalary = new JLabel("Loop");
        lblEmpSalary.setBounds(630, 104, 660, 60);
        lblEmpSalary.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(lblEmpSalary);
    }


    private void initializeScrollPane() {
        JScrollPane scrollPane = new JScrollPane(_table);
        scrollPane.setBounds(10, 170, 980, 380);
        _frame.getContentPane().add(scrollPane);
    }


    private void initializeButtons(DefaultTableModel model) {
        JButton btnAdd = new JButton("Add peer");
        Object[] row = new Object[5];
        btnAdd.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                row[0] = "" + _table.getRowCount();
                row[1] = _variables.getText();
                row[2] = _neighbors.getText();
                row[3] = _init.getText();
                row[4] = _loop.getText();

                model.addRow(row);
                writePlaceHoldersInLabels();
            }
        });

        JButton confirmButton = new JButton("Confirm");
        confirmButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                JLabel label = new JLabel("Are you sure to confirm?");
                label.setFont(_labelsFont);
                int input = JOptionPane.showConfirmDialog(null,
                    label, "Select an Option...",
                    JOptionPane.YES_NO_CANCEL_OPTION);
                if (input == 0) {
                    generatePeerFile(_table.getRowCount() > 0);
                    _frame.dispose();
                }
            }
        });

        btnAdd.setBounds(800, 115, 150, 30);
        btnAdd.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(btnAdd);
        confirmButton.setBounds(800, 15, 150, 30);
        confirmButton.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(confirmButton);



        JButton _loadButton = new JButton("Load file");
        _loadButton.setBounds(800, 65, 150, 30);
        _loadButton.setFont(_tableHeaderAndLabelFont);
        _frame.getContentPane().add(_loadButton);
        _loadButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                JFileChooser fileChooser = new JFileChooser();
                fileChooser.setCurrentDirectory(new File(System.getProperty("user.home")));
                int result = fileChooser.showOpenDialog(_frame);
                if (result == JFileChooser.APPROVE_OPTION) {
                    JLabel label = new JLabel("Are you sure to use the selected file?");
                    label.setFont(_labelsFont);
                    int input = JOptionPane.showConfirmDialog(null,
                        label, "Select an Option...",
                        JOptionPane.YES_NO_CANCEL_OPTION);
                    if (input == 0) {
                        File selectedFile = fileChooser.getSelectedFile();
                        String fileContent = readFileAsString(selectedFile.toString());
                        writeFile(parseFile(fileContent.replace("\n", "").replace(" ","")));
                        _frame.dispose();
                    }
                }
            }
        });
    }


    private void writePlaceHoldersInLabels() {
        _variables.setText("{}");
        _neighbors.setText("[]");
        _init.setText("[]");
        _loop.setText("[]");
    }


    private void generatePeerFile(boolean atLeastOnePeer) {

        String fileContent = "[";
        for (int i = 0; i < _table.getRowCount(); i++) {
            fileContent += "{";

            for (int j = 0; j < _table.getColumnCount(); j++) {
                if (j > 2) {
                    fileContent += createFunctionStrings("" + _table.getModel().getValueAt(i, j)) + ",";
                } else {
                    fileContent += _table.getModel().getValueAt(i, j) + ",";
                }
            }

            fileContent = fileContent.substring(0, fileContent.length() - 1) + "},\n";
        }

        if (atLeastOnePeer) {
            fileContent = fileContent.substring(0, fileContent.length() - 2) + " ]";
        } else {
            fileContent += " ]";
        }

        writeFile(fileContent.replace("\n", ""));
    }


    public String readFileAsString(String fileName) {
        try {
            String data = "";
            data = new String(Files.readAllBytes(Paths.get(fileName)));
            return data;
        } catch (IOException e) {
            System.err.println("ERROR: Cannot read the file.");
            e.printStackTrace();
        }
        return null;
    }


    private void writeFile(String toWrite) {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(FILE_NAME));
            writer.write(toWrite);
            writer.close();
        } catch (IOException e) {
            System.err.println("ERROR: Cannot write in file.");
            e.printStackTrace();
        }
    }


    private String createFunctionStrings(String str) {
        String toReturn = "";
        String[] strs = str.replace(" ", "").split("},");

        for (int i = 0; i < strs.length; i++) {
            toReturn += strs[i].replace(",", "\",\"").replace("{", "{\"").replace("}", "\"}");

            if (i < strs.length - 1) {
                toReturn += "\"},";
            }
        }
        return toReturn;
    }


    private String parseFile(String strToParse) {
      String str = strToParse.substring(1, strToParse.length() - 1);
      String toReturn = "";
      String toTransform = "";
      boolean insideSquareBrackets = false;
      
      for(int i = 0; i < str.length(); i++) {
        
        if (str.charAt(i) != '[' && str.charAt(i) != ']' && !insideSquareBrackets) {
          toReturn += str.charAt(i);
        
        } else if (str.charAt(i) == '[' && str.charAt(i + 1) == '{') {
          toTransform += str.charAt(i);
          insideSquareBrackets = true;

      } else if (str.charAt(i) == ']') {
          toTransform += str.charAt(i);
          toReturn += createFunctionStrings(toTransform);
          toTransform = "";
          insideSquareBrackets = false;
        
        } else if (insideSquareBrackets) {
          toTransform += str.charAt(i);
        
        } else {
          toReturn += str.charAt(i);
        }

      }

      return "[" + toReturn + "]";
    }




    private class MyTable extends JTable {
        @Override
        public boolean isCellEditable(int row, int column) {
            return column != 0;
        }
    }




}