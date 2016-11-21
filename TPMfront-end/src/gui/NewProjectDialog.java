/* Copyright (c) 2005-2016 Biomathematics and Statistics Scotland
 * http://www.bioss.ac.uk/ 
 * 
 * This file is part of TetraploidMap.
 *
 *    TetraploidMap is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    TetraploidMap is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with TetraploidMap.  If not, see <http://www.gnu.org/licenses/>.
 */

package gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import doe.DoeLayout;
import doe.MsgBox;

// Dialog class that presents a "New Project" dialog to the user, allowing them
// to select a name and a location for a new project. This project is then
// created and saved to disk ready for use.

class NewProjectDialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = -4394927069449552025L;

	private Project project = null;

	private JTextField name, location;
	private JButton bOK, bCancel, bBrowse;

	NewProjectDialog(AppFrame appFrame) {
		super(appFrame, "New Project", true);

		add(new GradientPanel("New Project"), BorderLayout.NORTH);
		add(createControls());
		add(createButtons(), BorderLayout.SOUTH);

		pack();
		getRootPane().setDefaultButton(bOK);
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}

	Project getProject() {
		return project;
	}

	private JPanel createControls() {
		name = new JTextField("MyProject", 25);
		name.setToolTipText("Name of the new project");
		location = new JTextField(Prefs.gui_dir, 25);
		location.setToolTipText("Location on disk where the project will " + "be stored");
		bBrowse = new JButton("Browse...");
		bBrowse.setMnemonic(KeyEvent.VK_B);
		bBrowse.addActionListener(this);
		bBrowse.setToolTipText("Browse for and select a project location");
		final JLabel label1 = new JLabel("Please select a name and a location for the " + "project:");
		final JLabel label2 = new JLabel("Name: ");
		label2.setDisplayedMnemonic('N');
		label2.setLabelFor(name);
		final JLabel label3 = new JLabel("Location: ");
		label3.setDisplayedMnemonic('L');
		label3.setLabelFor(location);

		DoeLayout layout = new DoeLayout();
		layout.add(label1, 0, 0, 1, 3, new Insets(5, 5, 5, 5));
		layout.add(label2, 0, 1, 1, 1, new Insets(5, 5, 5, 0));
		layout.add(name, 1, 1, 1, 2, new Insets(5, 5, 5, 5));
		layout.add(label3, 0, 2, 1, 1, new Insets(0, 5, 5, 0));
		layout.add(location, 1, 2, 1, 1, new Insets(0, 5, 5, 0));
		layout.add(bBrowse, 2, 2, 1, 1, new Insets(0, 5, 5, 5));

		layout.getPanel().setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		return layout.getPanel();
	}

	private JPanel createButtons() {
		bOK = new JButton("OK");
		bOK.addActionListener(this);
		bCancel = new JButton("Cancel");
		bCancel.addActionListener(this);

		JPanel p1 = new JPanel(new GridLayout(1, 2, 5, 5));
		p1.add(bOK);
		p1.add(bCancel);

		JPanel p2 = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
		p2.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p2.add(p1);

		return p2;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == bCancel) {
			setVisible(false);
		} else if (e.getSource() == bBrowse) {
			browseForLocation();
		} else if (e.getSource() == bOK) {
			createNewProject();
		}
	}

	private void browseForLocation() {
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Browse");
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		if (fc.showDialog(this, "Select") == JFileChooser.APPROVE_OPTION) {
			File filename = fc.getSelectedFile();
			Prefs.gui_dir = "" + fc.getSelectedFile();

			location.setText("" + filename);
		}
	}

	private void createNewProject() {
		// Check that values were entered in all required fields
		if (name.getText().length() == 0 || location.getText().length() == 0) {
			MsgBox.msg("Please ensure all required settings have been " + "completed.", MsgBox.ERR);
			return;
		}

		// Check directory is ok
		String dir = location.getText();
		File dirFile = new File(dir);

		try {
			if (dirFile.exists() && !dirFile.isDirectory()) {
				MsgBox.msg(dir + " is not a valid directory.", MsgBox.ERR);
				return;
			} else if (!dirFile.exists()) {
				if (!dirFile.mkdirs()) {
					MsgBox.msg("The specified project directory cannot be " + "created.", MsgBox.ERR);
					return;
				}
			}
		} catch (Exception e) {
			MsgBox.msg("The project cannot be created due to the following " + "error:\n" + e, MsgBox.ERR);
			return;
		}

		// Check filename is ok
		File filename = new File(dir, name.getText() + ".proj");
		if (filename.exists()) {
			String msg = filename.getAbsolutePath() + " already exists.\n" + "Do you want to replace it?";
			if (MsgBox.yesno(msg, 1) != JOptionPane.YES_OPTION) return;
		}

		project = new Project(name.getText(), filename);
		setVisible(false);
	}
}