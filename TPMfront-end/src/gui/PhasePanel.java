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
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Vector;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.border.MatteBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import data.CMarker;
import data.OrderedResult;
import doe.MsgBox;

class PhasePanel extends JPanel implements ListSelectionListener, MouseListener {
	private static final long serialVersionUID = -5447140484227817578L;
	private OrderedResult order;
	private JTable phaseTable;
	private PhaseTableModel phaseModel;
	private IconHeaderRenderer headerRenderer;
	private boolean editingTable = false;
	private String lastEdit;

	PhasePanel(OrderedResult order) {
		this.order = order;
		setLayout(new BorderLayout(5, 5));
		add(createControls());
	}

	private JPanel createControls() {
		// Marker model (for ordered list of markers)
		phaseModel = new PhaseTableModel();
		headerRenderer = new IconHeaderRenderer();

		// Populate the marker table
		phaseTable = new JTable(phaseModel);
		phaseTable.getColumnModel().getColumn(0).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(1).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(2).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(3).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(4).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(5).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(6).setHeaderRenderer(headerRenderer);
		phaseTable.getColumnModel().getColumn(7).setHeaderRenderer(headerRenderer);
		phaseTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		phaseTable.getColumnModel().getColumn(0).setPreferredWidth(80); // SC
		phaseTable.getColumnModel().getColumn(1).setPreferredWidth(150); // name
		phaseTable.getColumnModel().getColumn(2).setPreferredWidth(50); // ratio
		phaseTable.getColumnModel().getColumn(3).setPreferredWidth(70); // location
		phaseTable.getColumnModel().getColumn(4).setPreferredWidth(50); // P1
		phaseTable.getColumnModel().getColumn(5).setPreferredWidth(50); // P2
		phaseTable.getColumnModel().getColumn(6).setPreferredWidth(40); // P1d
		phaseTable.getColumnModel().getColumn(7).setPreferredWidth(40); // P2d
		phasePopulate();
		phaseTable.getSelectionModel().addListSelectionListener(this);
		phaseTable.getTableHeader().addMouseListener(this);
		JScrollPane mSP = new JScrollPane(phaseTable);
		mSP.setPreferredSize(new Dimension(300, 10));

		JPanel p1 = new JPanel(new BorderLayout(5, 0));
		p1.add(new JLabel("Ordered Markers:"), BorderLayout.NORTH);
		p1.add(mSP);

		return p1;
	}

	private void phasePopulate() {
		Vector<Float> distances = order.getDistances();
		float dist = 0;
		int i;
		Vector<CMarker> ms = order.getLinkageGroup().getMarkers();
		for (int j = 0; j < ms.size(); j++) {
			if (order.flip) {
				i = ms.size() - j - 1;
			} else {
				i = j;
			}
			if (!order.flip) {
				dist += distances.get(i);
			}
			CMarker cm = ms.elementAt(i);
			phaseModel.addRow(new Object[] { cm.marker.getPrefix(), cm, cm.marker.getRatio(), dist,
				cm.marker.getPhenotypeInfo(1), cm.marker.getPhenotypeInfo(2), 
				cm.marker.getParentDosage(1), cm.marker.getParentDosage(2) });
			if (order.flip) {
				dist += distances.get(i);
			}
		}
	}

	public void mouseReleased(MouseEvent e) {
		return;
	}

	public void mouseEntered(MouseEvent e) {
		return;
	}

	public void mouseExited(MouseEvent e) {
		return;
	}

	public void mousePressed(MouseEvent e) {
		return;
	}

	public void mouseClicked(MouseEvent e) {
		int col = phaseTable.columnAtPoint(e.getPoint());
		String cn = phaseTable.getColumnName(col);
		if (cn.equals("Location")) {
			flipme();
		} else if (cn.equals("P1")) {
			swapP(0);
		} else if (cn.equals("P2")) {
			swapP(1);
		}
	}

	private void flipme() {
		order.flip = !order.flip;
		phaseModel.setRowCount(0);
		phasePopulate();
	}

	private void swapP(int p) {
		boolean welldone = false;
		while (!welldone) {
			String neworder = (String) JOptionPane.showInputDialog(this,
					"Please enter the P" + (p + 1)
							+ " reordering sequence (e.g. 4321 for a reversal left-to-right; "
							+ "1234 for unchanged; 4123 to shift left; 2341 to shift right)",
					"Enter P" + (p + 1) + " reorder sequence", JOptionPane.PLAIN_MESSAGE, null, null, "4321");

			if (neworder != null && neworder.length() == 4 
					&& neworder.indexOf('1') != -1 && neworder.indexOf('2') != -1
					&& neworder.indexOf('3') != -1 && neworder.indexOf('4') != -1) {

				order.reorderpheno(p, neworder);
				welldone = true;
			} else {
				if (neworder == null) {
					welldone = true;
				} else {
					MsgBox.msg("Please give the reorder sequence as 4 numbers of 1-4. Your input '" + neworder
							+ "' is invalid.", MsgBox.ERR);
				}
			}
		}
		phaseModel.setRowCount(0);
		phasePopulate();
	}

	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting()) return;
	}

	@SuppressWarnings("serial")
	private class IconHeaderRenderer extends DefaultTableCellRenderer {
		private IconHeaderRenderer() {
			setHorizontalAlignment(JLabel.CENTER);
		}

		@Override
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
				boolean hasFocus, int row, int column) {
			Component renderer = super.getTableCellRendererComponent(table, value, isSelected, 
					hasFocus, row, column);
			renderer.setBackground(Color.WHITE);
			setText(value.toString());
			if (value.toString().equals("Location")) {
				setToolTipText("Flip locations back to front.");
				setIcon(Icons.FLIP2);
			} else if (value.toString().equals("P1") || value.toString().equals("P2")) {
				setIcon(Icons.FLIP2);
				setToolTipText("Click to change the H1, H2, H3, H4 ordering.");
			} else {
				setIcon(null);
				setToolTipText(null);
			}
			setBorder(new MatteBorder(0, 0, 1, 1, Color.LIGHT_GRAY));
			return this;
		}
	}

	@SuppressWarnings("serial")
	private class PhaseTableModel extends DefaultTableModel {
		String[] titles = new String[] { "SC Group", "Marker Name", "Ratio", "Location", "P1", "P2", 
			"P1d", "P2d" };

		@SuppressWarnings("rawtypes")
		Class[] types = new Class[] { String.class, data.Marker.class, String.class, Double.class, 
			String.class, String.class, String.class, String.class };

		PhaseTableModel() {
			for (int i = 0; i < titles.length; i++) addColumn(titles[i]);

			this.addTableModelListener(new TableModelListener() {
				public void tableChanged(TableModelEvent e) {
					editingTable = false;
					if (editingTable) {
						int r = e.getFirstRow();
						int c = e.getColumn();

						if (error((String) getValueAt(r, c)) == false) {
							AppFrameMenuBar.aFileSave.setEnabled(true);
						} else {
							setValueAt(lastEdit, r, c);
						}

						editingTable = false;
					}
				}
			});
		}

		private boolean error(String str) {
			boolean error = false;

			if (str.length() != 4) {
				error = true;
			}

			try {
				Integer.parseInt(str);
			} catch (NumberFormatException e) {
				error = true;
			}

			if (error) {
				MsgBox.msg("String formatting should be in the form 0000, " 
						+ "where each digit is in the range 0-9.",
						MsgBox.ERR);
			}
			return error;
		}

		public String getColumnName(int c) {
			return titles[c];
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Class getColumnClass(int c) {
			return types[c];
		}

		public boolean isCellEditable(int r, int c) {
			/*
			 * if ((c > 3)&&(c < 6)) { editingTable = true; lastEdit = (String)
			 * getValueAt(r, c); return true; } else
			 */
			return false;
		}
	}
}