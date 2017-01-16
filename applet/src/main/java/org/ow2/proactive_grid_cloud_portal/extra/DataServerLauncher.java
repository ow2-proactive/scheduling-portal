/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.extra;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.prefs.Preferences;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.UIManager;


/**
 * Simple swing app that prompts for a PAProgramming or PAScheduling
 * installation path and looks for a pa-dataserver-gui[.bat] script
 * to launch the dataserver gui.
 * 
 * @author mschnoor
 *
 */
public class DataServerLauncher implements ActionListener {

    /**
     * Entry point
     * 
     * @param args [0] : name of the application type, eg 'vnc'
     *              [1] : argument for the application, eg 'localhost:1'
     */
    public static void main(String[] args) {
        setLF();
        DataServerLauncher r = new DataServerLauncher();
        r.buildAndShow();
    }

    /** name of the user preferences node */
    private static String PREF_NODE = DataServerLauncher.class.getCanonicalName();

    /** name of the property that contains the ProActive Programming path */
    private static String PA_PATH = "pa.path";

    /** preferences node referenced by PREF_NODE */
    private Preferences prefs = null;

    /** path to the proactive programming installation */
    private File paPath = null;

    /** displays this.paPath */
    private JTextField pathField = null;

    /** root window */
    private JFrame frame = null;

    /** button that triggers visualization */
    private JButton okButton = null;

    private DataServerLauncher() {
        this.prefs = Preferences.userRoot().node(PREF_NODE);
        String p = prefs.get(PA_PATH, null);
        if (p != null)
            this.paPath = new File(p);
    }

    /**
     * Create the GUI, show it
     */
    private void buildAndShow() {
        JPanel centerPane = new JPanel();
        GridBagLayout layout = new GridBagLayout();
        centerPane.setLayout(layout);
        GridBagConstraints c = new GridBagConstraints();

        JPanel header = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0));
        header.setBackground(Color.white);
        header.setMinimumSize(new Dimension(100, 60));
        header.add(new JLabel(new ImageIcon(this.getClass().getResource("header_split.png"))));
        header.add(new JLabel("    "));
        header.add(new JLabel("<html><b><font size=+1>Data Servers</font></b><br>" +
                              "<font size=-1>Manage Data Servers to send data<br>and retrieve computation results.</font></html>"));

        c.gridwidth = 3;
        c.weightx = 1.0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(0, 0, 15, 0);
        centerPane.add(header, c);

        c.gridy = 2;
        c.insets = new Insets(5, 5, 5, 5);
        centerPane.add(new JLabel("<html>Select the path to a local<br>ProActive Programming directory.</html>"), c);

        c.gridx = 0;
        c.gridwidth = 1;
        c.weightx = 0;
        c.gridy = 3;
        c.fill = GridBagConstraints.NONE;
        c.anchor = GridBagConstraints.LINE_END;
        centerPane.add(new JLabel("Executable"), c);

        c.gridx = 1;
        c.weightx = 1;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.anchor = GridBagConstraints.LINE_START;
        this.pathField = new JTextField(paPath != null ? paPath.getAbsolutePath() : "");
        pathField.setEditable(false);
        centerPane.add(pathField, c);

        c.gridx = 2;
        c.fill = GridBagConstraints.NONE;
        c.weightx = 0;
        JButton browseButton = new JButton("Open", new ImageIcon(this.getClass().getResource("folder.png")));
        browseButton.setActionCommand("browse");
        browseButton.addActionListener(this);
        centerPane.add(browseButton, c);

        this.okButton = new JButton("Manage Servers", new ImageIcon(this.getClass().getResource("server.png")));
        okButton.setEnabled(paPath != null && paPath.exists());
        okButton.addActionListener(this);
        okButton.setActionCommand("ok");
        JPanel buttonPane = new JPanel();
        buttonPane.setLayout(new FlowLayout(FlowLayout.CENTER));
        buttonPane.add(okButton);

        JPanel contentPane = new JPanel();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(centerPane, BorderLayout.NORTH);
        contentPane.add(buttonPane, BorderLayout.SOUTH);

        this.frame = new JFrame("Remote Visualization");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setContentPane(contentPane);
        frame.setResizable(true);
        frame.setMinimumSize(new Dimension(370, 240));
        frame.setPreferredSize(new Dimension(370, 240));
        frame.setLocationRelativeTo(null);

        try {
            frame.setIconImages(getIcons());
        } catch (Throwable t) {
            // java6 required
            try {
                frame.setIconImage(getIcon());
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        frame.pack();
        frame.setVisible(true);
    }

    private List<Image> getIcons() {
        String[] icons = { "16", "24", "32", "48", "64", "128" };
        List<Image> images = new ArrayList<Image>(icons.length);

        for (String size : icons) {
            String name = size + ".png";
            URL url = this.getClass().getResource(name);
            try {
                Image img = ImageIO.read(url);
                images.add(img);
            } catch (IOException e) {
                System.out.println("Failed to open icon: " + url.toString());
            }
        }
        return images;
    }

    private Image getIcon() throws IOException {
        String name = "32.png";
        URL url = this.getClass().getResource(name);
        return ImageIO.read(url);
    }

    /**
     * Set the swing look&feel
     */
    private static void setLF() {
        try {
            if (System.getProperty("os.name").toLowerCase().contains("linux")) {
                UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
            } else {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals("browse")) {
            JFileChooser fc = new JFileChooser();
            fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            int ret = fc.showOpenDialog(this.frame);
            if (ret == JFileChooser.APPROVE_OPTION) {
                File f = fc.getSelectedFile();
                if (f != null && f.exists()) {
                    this.paPath = f;
                    this.prefs.put(PA_PATH, this.paPath.getAbsolutePath());
                    this.pathField.setText(this.paPath.getAbsolutePath());
                    this.okButton.setEnabled(true);
                }
            }
        } else if (e.getActionCommand().equals("ok")) {
            String os = System.getProperty("os.name").toLowerCase();
            String exec = null;
            String[] cmdArr = null;

            if (os.contains("mac") || os.contains("nix") || os.contains("nux")) {
                exec = this.paPath.getAbsolutePath() + "/bin/pa-dataserver-gui.sh";
                if (!new File(exec).exists())
                    exec = this.paPath.getAbsolutePath() + "/bin/unix/pa-dataserver-gui";

                cmdArr = new String[] { "/bin/sh", exec };
            } else if (os.contains("win")) {
                exec = this.paPath.getAbsolutePath() + "\\bin\\pa-dataserver-gui.bat";
                if (!new File(exec).exists())
                    exec = this.paPath.getAbsolutePath() + "\\bin\\windows\\pa-dataserver-gui.bat";

                cmdArr = new String[] { exec };
            }

            try {
                if (!new File(exec).exists()) {
                    throw new Exception("Could not find DataServer launcher...<br>" +
                                        "Is the provided directory a valid ProActive Programming or Scheduling distribution?");
                }
                ProcessBuilder pb = new ProcessBuilder(cmdArr);
                pb.redirectErrorStream(true);
                final Process p = pb.start();

                new Thread(new Runnable() {
                    public void run() {
                        BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
                        int ret = Integer.MIN_VALUE;

                        while (ret == Integer.MIN_VALUE) {
                            try {
                                ret = p.exitValue();
                            } catch (IllegalThreadStateException exc) {
                                String line = null;
                                try {
                                    while ((line = br.readLine()) != null) {
                                        System.out.println(line);
                                    }
                                } catch (IOException e) {
                                    e.printStackTrace();
                                    System.exit(0);
                                }
                            }
                            try {
                                Thread.sleep(500);
                            } catch (InterruptedException e) {
                                System.exit(0);
                            }
                        }
                        System.exit(ret);
                    }
                }).start();

                // hide the frame, keep the VM up to peek in the output
                // the process should not return upon success so there no real way
                // to know if it worked
                this.frame.setVisible(false);

            } catch (Exception e1) {
                JOptionPane.showMessageDialog(null,
                                              "<html>Failed to launch application:<br>" + e1.getMessage() + "</html>",
                                              "Data Servers error",
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
