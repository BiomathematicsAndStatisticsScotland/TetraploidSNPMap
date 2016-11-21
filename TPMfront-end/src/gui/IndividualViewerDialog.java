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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import data.AlleleState;
import data.Marker;

class IndividualViewerDialog extends JDialog {
	private static final long serialVersionUID = -1744220607999991322L;
	private Marker marker;
	private JTextArea text;

	IndividualViewerDialog(JFrame parent, Marker marker) {
		super(parent, marker.getName(), true);
		this.marker = marker;

		add(createControls());

		setPreferredSize(new Dimension(text.getPreferredSize().width + 50, 300));
		pack();
		setLocationRelativeTo(parent);
		setResizable(false);
		setVisible(true);
	}

	private JPanel createControls() {
		text = new JTextArea(getData());
		text.setEditable(false);
		text.setFont(new Font("Monospaced", Font.PLAIN, 11));
		text.setMargin(new Insets(2, 5, 2, 5));

		JScrollPane sp = new JScrollPane(text);
		sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		JLabel label = new JLabel((marker.getAllele(0).getStateCount() - 2)
				+ " individuals per allele (plus two parents). " 
				+ marker.getMissingPercentage() + "% unknown.");
		p1.add(label, BorderLayout.NORTH);
		p1.add(sp);

		return p1;
	}

	private String getData() {
		String str = new String();

		for (int a = 0; a < marker.getAlleleCount(); a++) {
			str += "Allele " + (a + 1) + "\n";
			int count = 0;
			for (AlleleState state : marker.getAllele(a).getStates()) {
				str += " " + state.getState();
				if (++count == 40) {
					str += "\n";
					count = 0;
				}
			}
			if (a < marker.getAlleleCount() - 1) {
				str += "\n\n";
			}
		}

		return str;
	}
}