/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
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
import java.io.File;
import java.io.IOException;
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
 * Simple Swing application
 * that prompts for a binary, associates it with a given type in the local preferences store,
 * and executes it with a given argument
 * 
 * @author mschnoor
 *
 */
public class RemoteViewer implements ActionListener {

	/**
	 * Entry point
	 * 
	 * @param args [0] : name of the application type, eg 'vnc'
	 *              [1] : argument for the application, eg 'localhost:1'
	 */
	public static void main(String[] args) {
		setLF();

		if (args.length < 1) {
			JOptionPane.showMessageDialog(null, "Application type must be provided as argument",
					"Visualization error", JOptionPane.ERROR_MESSAGE);
		} else if (args.length < 2) {
			JOptionPane.showMessageDialog(null, "Application argument must be provided as argument",
					"Visualization error", JOptionPane.ERROR_MESSAGE);
		} else {
			String appType = args[0];
			String argument = args[1];
			RemoteViewer r = new RemoteViewer(appType, argument);
			r.buildAndShow();
		}
	}

	/** name of the user preferences node */
	private static String PREF_NODE = "org.ow2.proactive_grid_cloud_portal.client.RemoteViewer";
	/** preferences node referenced by PREF_NODE */
	private Preferences prefs = null;
	/** type of the application to launch */
	private String appType = "";
	/** argument for the application to launch */
	private String argument = "";
	/** name of the associated app retrieved in the preferences, or "" */
	private String userApp = "";
	/** displays this.userApp */
	private JTextField pathField = null;
	/** root window */
	private JFrame frame = null;
	/** button that triggers visualization */
	private JButton okButton = null;

	private RemoteViewer(String appType, String argument) {
		this.prefs = Preferences.userRoot().node(PREF_NODE);
		this.appType = appType;
		this.argument = argument;
		this.userApp = prefs.get(appType, "");
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
		header
				.add(new JLabel(
					"<html><b><font size=+1>Visualization</font></b><br>"
						+ "<font size=-1>Connect to a remote host to view<br>the result of a computation.</font></html>"));

		c.gridwidth = 3;
		c.weightx = 1.0;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.insets = new Insets(0, 0, 15, 0);
		centerPane.add(header, c);

		c.gridwidth = 1;
		c.weightx = 0;
		c.fill = GridBagConstraints.NONE;
		c.gridy = 1;
		c.gridx = 0;
		c.insets = new Insets(5, 5, 0, 5);
		c.anchor = GridBagConstraints.LINE_END;
		centerPane.add(new JLabel("Type"), c);
		c.gridx = 1;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.LINE_START;
		centerPane.add(new JLabel("<html><b>" + this.appType + "</b></html>"), c);

		c.gridy = 2;
		c.gridx = 0;
		c.gridwidth = 1;
		c.anchor = GridBagConstraints.LINE_END;
		centerPane.add(new JLabel("Argument"), c);
		c.gridx = 1;
		c.gridwidth = 2;
		c.weightx = 1;
		c.fill = GridBagConstraints.HORIZONTAL;
		c.anchor = GridBagConstraints.LINE_START;
		centerPane.add(new JLabel("<html><b>" + this.argument + "</b></html>"), c);

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
		this.pathField = new JTextField(userApp);
		pathField.setEditable(false);
		centerPane.add(pathField, c);

		c.gridx = 2;
		c.fill = GridBagConstraints.NONE;
		c.weightx = 0;
		JButton browseButton = new JButton("Open", new ImageIcon(this.getClass().getResource("folder.png")));
		browseButton.setActionCommand("browse");
		browseButton.addActionListener(this);
		centerPane.add(browseButton, c);

		this.okButton = new JButton("Launch visualization", new ImageIcon(this.getClass().getResource(
				"visu.png")));
		okButton.setEnabled(!userApp.equals(""));
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
		Image img = ImageIO.read(url);
		return img;
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
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals("browse")) {
			JFileChooser fc = new JFileChooser();
			int ret = fc.showOpenDialog(this.frame);
			if (ret == JFileChooser.APPROVE_OPTION) {
				File f = fc.getSelectedFile();
				if (f != null && f.exists()) {
					this.userApp = f.getAbsolutePath();
					this.prefs.put(this.appType, this.userApp);
					this.pathField.setText(this.userApp);
					this.okButton.setEnabled(true);
				}
			}
		} else if (e.getActionCommand().equals("ok")) {
			try {
				Runtime.getRuntime().exec(new String[] { this.userApp, this.argument });
				System.exit(0);
			} catch (IOException e1) {
				JOptionPane.showMessageDialog(null, "<html>Failed to launch application:<br>" +
					e1.getMessage() + "</html>", "Visualization error", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
