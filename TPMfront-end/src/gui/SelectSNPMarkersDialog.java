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

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import data.LinkageGroup;
import data.Marker;
import doe.DoeLayout;

/*
 * TV: cloned from SelectMarkersDialog class
 * This class is used as an interface in order to change the parameters
 * set in the Prefs.java file. After this is done and "OK" button is pressed
 * then the SelectSNPMarkers.doSelection is called to do the actual filtering
 */

public class SelectSNPMarkersDialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = -6709643404270492559L;
	private LinkageGroup lGroup;
	private JCheckBox cParent, cRatio;
	private JCheckBox cSelectedOnly;
	private JComboBox<String> parent, ratio;
	private JButton bOK, bCancel;
	private SpinnerNumberModel ratioModel;
	private JSpinner ratioSpin;
	private JLabel ratioLabel;

	private boolean isOK = false;

	SelectSNPMarkersDialog(AppFrame appFrame, LinkageGroup lGroup) {
		super(appFrame, "Select Markers", true);
		this.lGroup = lGroup;

		setLayout(new BorderLayout());
		add(createControls(), BorderLayout.CENTER);
		add(createButtons(), BorderLayout.SOUTH);

		pack();
		getRootPane().setDefaultButton(bOK);
		setResizable(false);
		setLocationRelativeTo(appFrame);
		setVisible(true);
	}

	private JPanel createControls() {
		cParent = new JCheckBox("The marker is present in: ", Prefs.gui_select_on_parent);
		cParent.addActionListener(this);
		parent = new JComboBox<String>();
		parent.addItem("Parent 1 Only");
		parent.addItem("Parent 2 Only");
		parent.addItem("Parent 1 OR Parent 2");
		parent.setSelectedIndex(Prefs.gui_select_parent - 1);
		cRatio = new JCheckBox("Segregating with a ratio of: ", Prefs.gui_select_on_ratio);
		cRatio.addActionListener(this);
		ratio = new JComboBox<String>();
		ratio.addItem("1:1");
		ratio.addItem("higher");
		switch (Prefs.gui_select_ratio) {
			case Marker.R1_1:
				ratio.setSelectedIndex(0);
				break;
			case Marker.R_h:
				ratio.setSelectedIndex(1);
				break;
			default:
				break;
		}

		ratioModel = new SpinnerNumberModel(Prefs.gui_select_ratio_sig, 0, 1, 0.01);
		ratioSpin = new JSpinner(ratioModel);
		((JSpinner.NumberEditor) ratioSpin.getEditor()).getTextField().setToolTipText(
				"Minimum significance value that a marker will be " + "considered to have this ratio at");
		ratioLabel = new JLabel("significance threshold: ");

		cSelectedOnly = new JCheckBox("Only match these settings against " 
				+ "markers that are not yet selected",
				Prefs.gui_select_match_selected);

		DoeLayout layout = new DoeLayout();
		layout.add(cParent, 0, 1, 0, 1, new Insets(5, 0, 0, 0));
		layout.add(parent, 1, 1, 1, 2, new Insets(5, 5, 0, 0));
		layout.add(cRatio, 0, 2, 0, 1, new Insets(5, 0, 0, 0));
		layout.add(ratio, 1, 2, 1, 2, new Insets(5, 5, 0, 0));
		layout.add(ratioLabel, 1, 3, 0, 1, new Insets(0, 5, 0, 0));
		layout.add(ratioSpin, 2, 3, 0, 1, new Insets(0, 5, 0, 0));
		layout.add(cSelectedOnly, 0, 6, 1, 4, new Insets(10, 0, 0, 0));

		layout.getPanel().setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createEmptyBorder(5, 5, 5, 5),
				BorderFactory.createTitledBorder("Select only markers:")));

		setControlStates();
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

	boolean isOK() {
		return isOK;
	}

	/** listens to the UI controls.
	 * 
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == bCancel) {
			setVisible(false);
		} else if (e.getSource() == bOK) {
			selectMarkers();
		} else if (e.getSource() instanceof JCheckBox) {
			setControlStates();
		}
	}

	private void selectMarkers() {
		Prefs.gui_select_on_parent = cParent.isSelected();
		Prefs.gui_select_on_ratio = cRatio.isSelected();
		Prefs.gui_select_ratio_sig = ratioModel.getNumber().floatValue();
		Prefs.gui_select_match_selected = cSelectedOnly.isSelected();

		Prefs.gui_select_type = Marker.SNP;

		switch (ratio.getSelectedIndex()) {
			case 0:
				Prefs.gui_select_ratio = Marker.R1_1;
				break;
			case 1:
				Prefs.gui_select_ratio = Marker.R_h;
				break;
			default:
				break;
		}
		Prefs.gui_select_parent = parent.getSelectedIndex() + 1;

		SelectSNPMarkers.doSelection(lGroup);
		isOK = true;
		setVisible(false);
	}

	private void setControlStates() {
		parent.setEnabled(cParent.isSelected());
		ratio.setEnabled(cRatio.isSelected());
		ratioLabel.setEnabled(cRatio.isSelected());
		ratioSpin.setEnabled(cRatio.isSelected());
	}
}
