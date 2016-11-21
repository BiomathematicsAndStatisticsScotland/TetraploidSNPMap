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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.MouseEvent;
import java.util.Vector;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import data.CMarker;
import data.OrderedResult;

class TwopointQTLPanel extends JPanel {
	private static final long serialVersionUID = 2804535716921891595L;
	private OrderedResult order;
	private JTable markerTable;
	private QTLTableModel markerModel;
	private JLabel m1Label, m2Label;

	TwopointQTLPanel(OrderedResult order) {
		this.order = order;
		setLayout(new BorderLayout(5, 5));
		add(createControls());
	}

	private JPanel createControls() {
		// Marker model (for ordered list of markers)
		markerModel = new QTLTableModel();

		// Populate the marker table
		markerTable = new JTable(markerModel) {
			private static final long serialVersionUID = -7932722588196427665L;

			public String getToolTipText(MouseEvent e) {
				CMarker cm;
				java.awt.Point p = e.getPoint();
				int rowIndex = rowAtPoint(p);
				cm = (CMarker) markerModel.getValueAt(rowIndex, 2);
				String errors = cm.marker.getErrors();
				if (!"".equals(errors)) return errors;
				return null;
			}
		};
		markerTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		markerTable.getColumnModel().getColumn(0).setPreferredWidth(30);
		markerTable.getColumnModel().getColumn(1).setPreferredWidth(110);
		markerTable.getColumnModel().getColumn(2).setPreferredWidth(170);
		markerTable.getColumnModel().getColumn(3).setPreferredWidth(70);
		markerTable.getColumnModel().getColumn(4).setPreferredWidth(50);
		markerTable.getColumnModel().getColumn(5).setPreferredWidth(70);
		markerTable.getColumnModel().getColumn(6).setPreferredWidth(50);
		markerTable.getColumnModel().getColumn(7).setPreferredWidth(70);
		markerTable.getColumnModel().getColumn(8).setPreferredWidth(50);
		markerTable.getColumnModel().getColumn(9).setPreferredWidth(50);
		markerTable.getColumnModel().getColumn(10).setPreferredWidth(50);
		CMRenderer cmrenderer = new CMRenderer();
		markerTable.getColumnModel().getColumn(2).setCellRenderer(cmrenderer);

		int i = 0;
		Vector<Float> distances = order.getDistances();
		float prevdist = 0;
		for (CMarker cm : order.getLinkageGroup().getMarkers()) {
			markerModel.addRow(new Object[] { i + 1, cm.marker.getPrefix(), cm, Prefs.d3.format(prevdist),
					cm.marker.getParentDosage(1), cm.marker.getPhenotypeInfo(1), cm.marker.getParentDosage(2),
					cm.marker.getPhenotypeInfo(2), cm.marker.getSNPRatio(), cm.marker.getChiSig(),
					cm.marker.getStatus() });
			prevdist += distances.elementAt(i);
			i++;
		}
		m1Label = new JLabel("");
		m1Label.setFont(new Font("Monospaced", Font.PLAIN, 11));
		m2Label = new JLabel("");
		m2Label.setFont(new Font("Monospaced", Font.PLAIN, 11));

		JScrollPane mSP = new JScrollPane(markerTable);
		mSP.setPreferredSize(new Dimension(300, 10));

		JPanel p1 = new JPanel(new BorderLayout(5, 0));
		p1.add(mSP);

		return p1;
	}

	private class QTLTableModel extends DefaultTableModel {
		private static final long serialVersionUID = -5312705025614875968L;

		String[] titles = new String[] { "#", "SC Group", "Name", "Location", "P1 dosage", "P1 phase",
			"P2 dosage","P2 phase", "Ratio", "Chi_sig", "DR code" };

		QTLTableModel() {
			for (int i = 0; i < titles.length; i++) {
				addColumn(titles[i]);
			}

		}

		public boolean isCellEditable(int r, int c) {
			return false;
		}
	}

	private class CMRenderer extends DefaultTableCellRenderer {
		private static final long serialVersionUID = 5078779574119614438L;

		public Component getTableCellRendererComponent(JTable t, Object o, boolean isSelected, 
				boolean hasFocus, int r, int c) throws NullPointerException {
			JLabel label = (JLabel) super.getTableCellRendererComponent(t, o, false, hasFocus, r, c);
			CMarker cm = (CMarker) markerTable.getValueAt(r, 2);
			if (cm.marker.getErrors().isEmpty() || cm.marker.getErrors() == null) {
				label.setForeground(Color.black);
				return label;
			}
			label.setForeground(Color.red);
			return label;
		}
	}

}
