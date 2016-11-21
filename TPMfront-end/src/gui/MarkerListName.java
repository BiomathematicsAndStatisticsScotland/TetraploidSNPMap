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
import java.awt.Component;
import java.awt.Font;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;

class MarkerListName extends JPanel {
	private static final long serialVersionUID = -5722080857415398135L;
	@SuppressWarnings("rawtypes")
	private JList list;
	@SuppressWarnings("rawtypes")
	private DefaultListModel model;

	@SuppressWarnings({ "rawtypes", "unchecked" })
	MarkerListName() {
		model = new DefaultListModel();
		list = new JList(model);
		list.setCellRenderer(new MarkerRenderer());
		list.setEnabled(false);

		setLayout(new BorderLayout());
		add(list, BorderLayout.CENTER);
	}

	@SuppressWarnings("unchecked")
	void setLinkageGroup(LinkageGroup lGroup) {
		model.clear();

		for (CMarker cm : lGroup.getMarkers()) {
			Marker m = cm.marker;

			if (m.getAlleleCount() == 1) {
				model.addElement(m.getName());
			} else {
				for (int a = 1; a <= m.getAlleleCount(); a++) {
					model.addElement(m.getName() + "_AL_" + a);
				}
			}
		}
	}

	@SuppressWarnings("rawtypes")
	private class MarkerRenderer extends JLabel implements ListCellRenderer {
		private static final long serialVersionUID = -4916139658782956988L;
		String name = "";

		public MarkerRenderer() {
			// Don't paint behind the component
			setOpaque(true);
		}

		// Set the attributes of the class and return a reference
		public Component getListCellRendererComponent(JList list, Object obj, 
				int i, boolean iss, boolean chf) {
			name = " " + (String) obj;

			// Set the font
			setFont(new Font("Monospaced", Font.PLAIN, Prefs.gui_mSize));
			setText(name);

			// Set background/foreground colours
			if (iss) {
				setBackground(list.getSelectionBackground());
				setForeground(list.getSelectionForeground());
			} else {
				setBackground(list.getBackground());
				setForeground(list.getForeground());
			}

			return this;
		}
	}
}