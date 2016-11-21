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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import data.AnovaResult;
import data.AnovaTraitResult;

public class AnovaResultsPanel extends JPanel implements ListSelectionListener {
	private AnovaResult results;
	static final long serialVersionUID = 5683281665309133L;

	@SuppressWarnings("rawtypes")
	private JList traitList;
	@SuppressWarnings("rawtypes")
	private DefaultListModel traitModel;
	private JTextArea details;

	/** AnovaResultsPanel(). NONSNP only. 
	 * 
	 * @param ar = AnovaResult. 
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public AnovaResultsPanel(AnovaResult ar) {
		results = ar;

		// Trait listbox
		traitModel = new DefaultListModel();
		for (AnovaTraitResult atr : results.getResults()) {
			traitModel.addElement(atr);
		}
		traitList = new JList(traitModel);
		traitList.addListSelectionListener(this);
		traitList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		JScrollPane sp1 = new JScrollPane(traitList);
		sp1.setPreferredSize(new Dimension(150, 150));

		// Details text box
		details = new JTextArea();
		details.setFont(new Font("Monospaced", Font.PLAIN, 11));
		details.setMargin(new Insets(2, 5, 2, 5));
		details.setEditable(false);
		details.setTabSize(6);
		details.setBackground(Color.white);

		JPanel p1 = new JPanel(new BorderLayout());
		// sp1.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		p1.setBorder(BorderFactory.createTitledBorder("Select trait:"));
		p1.add(sp1);

		JPanel p2 = new JPanel(new BorderLayout());
		// sp2.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		p2.setBorder(BorderFactory.createTitledBorder("ANOVA Results:"));
		JScrollPane sp2 = new JScrollPane(details);
		p2.add(sp2);

		JPanel p3 = new JPanel(new BorderLayout(5, 5));
		p3.add(p1, BorderLayout.NORTH);
		p3.add(p2);

		setLayout(new BorderLayout());
		add(new GradientPanel("ANOVA Results"), BorderLayout.NORTH);
		add(p3);
	}

	/** listens to the selected trait changes.
	 * 
	 */
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting()) return;

		AnovaTraitResult atr = (AnovaTraitResult) traitList.getSelectedValue();
		if (atr != null) {
			displayTrait(atr);
		} else {
			details.setText("");
		}
	}

	private void displayTrait(AnovaTraitResult atr) {
		String str = "";

		str = "Prefix      Marker               KWSig.   AVSig.     Mean(0)  "
				+ "Count(0)     Mean(1)  Count(1)         SED" + "\n";

		for (int i = 0; i < atr.getResultCount(); i++) {
			str += "\n" + format(atr.getMarker(i).getPrefix(), 12);
			str += atr.getMarker(i).getName(Prefs.nonsnpmarkername_maxlen);
			str += atr.getData(i);
		}

		details.setText(str);
		details.setCaretPosition(0);
	}

	private String format(String str, int length) {
		if (str.length() > length) {
			return str.substring(0, length);
		}

		for (int i = str.length(); i < length; i++) {
			str += " ";
		}
		return str;
	}
}