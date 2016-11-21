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
import data.AlleleDosage;
import data.Marker;

public class IndividualSNPViewerDialog extends JDialog {
	private static final long serialVersionUID = -3021877594991985881L;

	private Marker marker;

	private JTextArea text;

	IndividualSNPViewerDialog(JFrame parent, Marker marker) {
		super(parent, marker.getName(), true);
		this.marker = marker;

		this.add(createControls());

		this.setPreferredSize(new Dimension(text.getPreferredSize().width + 50, 300));
		this.pack();
		this.setLocationRelativeTo(parent);
		this.setResizable(false);
		this.setVisible(true);
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
		JLabel label = new JLabel((marker.getAllele(0).getDosageCount() - 2)
				+ " individuals per allele (plus two parents). " + marker.getNMiss() + " missing.");
		p1.add(label, BorderLayout.NORTH);
		p1.add(sp);

		return p1;
	}

	private String getData() {
		String str = new String();
		int count = 0;
		for (AlleleDosage dosage : marker.getAllele(0).getDosages()) {
			str += " " + dosage.getDosage();
			if (++count == 40) {
				str += "\n";
				count = 0;
			}
		}
		return str;
	}
}