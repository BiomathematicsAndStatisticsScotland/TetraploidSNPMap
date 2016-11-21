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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import data.CMarker;
import data.Cluster;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;
import gui.nav.NavPanel;

public class MarkerSNPDetails extends JPanel implements ListSelectionListener, TableModelListener {
	private static final long serialVersionUID = 5829007559639980758L;
	private MarkerSNPTableModel model;
	private NavPanel navPanel;
	private TableSorter sorter;
	private JTable table;
	private JScrollPane sp;

	private MarkerSNPSummaryPanel infoPanel = new MarkerSNPSummaryPanel();
	private GradientPanel gP;
	// Tracks how many markers have been selected
	private int totalCount = 0, selectedCount = 0;
	// True when the table is being auto-updated with new selection states
	boolean selectingMarkers = false;

	MarkerSNPDetails(NavPanel navPanel) {
		this.createTable();
		sp = new JScrollPane(table);
		this.navPanel = navPanel;

		JSplitPane splits = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		splits.setTopComponent(sp);
		splits.setBottomComponent(infoPanel);
		splits.setDividerLocation(300);
		gP = new GradientPanel("Marker Details (markersnpdetails)");
		setLayout(new BorderLayout());
		add(gP, BorderLayout.NORTH);
		add(splits);
	}

	/** Called when a new LinkageGroup is to be passed to this component for display.
	 * 
	 */
	public void displayLinkageGroup(LinkageGroup lGroup) {
		model.populateTable(lGroup);
		infoPanel.setMarker(null);

		selectedCount = lGroup.getSelectedMarkerCount();
		totalCount = lGroup.getMarkerCount();
		updateCountLabel();
	}

	/** Called when auto-selection of markers has taken place. 
	 * 
	 * <p>The model must be updated to reflect changes in the markers' states.
	 */ 
	public void selectedMarkersChanged(LinkageGroup lGroup) {
		selectingMarkers = true;
		model.selectedMarkersChanged(lGroup);
		selectingMarkers = false;
		selectedCount = 0;
		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.checked) {
				selectedCount++;
			}
		}

		// Need the repaint() call otherwise the cell renderers don't know to
		// update those rows that have now changed state
		updateCountLabel();
		table.repaint();
	}

	public void myrepaint() {
		table.repaint();
	}

	@SuppressWarnings("serial")
	private void createTable() {
		model = new MarkerSNPTableModel();
		model.addTableModelListener(this);

		table = new JTable(model) {
			public String getToolTipText(MouseEvent e) {
				return "";
			}
		};
		sorter = new TableSorter(model);
		sorter.setTableHeader(table.getTableHeader());
		model.setTableSorter(sorter);

		table.setModel(sorter);
		createClickListener();
		table.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		ListSelectionModel rowSM = table.getSelectionModel();
		rowSM.addListSelectionListener(this);

		StringRenderer sr = new StringRenderer();
		StringRenderer fl = new StringRenderer();

		table.setDefaultRenderer(String.class, sr);
		table.setDefaultRenderer(Double.class, fl);
		table.setDefaultRenderer(Marker.class, sr);
		table.setDefaultRenderer(Integer.class, sr);

		table.getColumnModel().getColumn(0).setPreferredWidth(40);
		table.getColumnModel().getColumn(2).setPreferredWidth(180);
	}

	CMarker getSelectedMarker() {
		return (CMarker) sorter.getValueAt(table.getSelectedRow(), 2);
	}

	/** Listens for double-clicks on the table.
	 * 
	 * <p>And opens a dialog that shows information on a marker's individuals when detected
	 */
	public void createClickListener() {
		table.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() != 2) return;

				int row = table.rowAtPoint(e.getPoint());
				Object obj = sorter.getValueAt(row, 2);
				if (obj != null) {
					Marker mkr = ((CMarker) obj).marker;

					if (e.isControlDown()) {
						Cluster cluster = navPanel.getCurrentClusterHeadNode();
						if (cluster != null) {
							LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
							new MoveMarker((CMarker) obj, lGroup, cluster);
							navPanel.refreshTree();
							displayLinkageGroup(lGroup);
						}
					} else {
						new IndividualSNPViewerDialog(MsgBox.frm, mkr);
					}
				}
			}
		});
	}

	/** Listens for changes in the selection of the table.
	 * 
	 */
	public void valueChanged(ListSelectionEvent e) {
		// Ignore extra messages.
		if (e.getValueIsAdjusting()) return;

		if (!navPanel.appFrame.tpmModeQTL()) AppFrameMenuBar.aMove.setEnabled(false);

		ListSelectionModel lsm = (ListSelectionModel) e.getSource();

		if (lsm.isSelectionEmpty() == false) {
			int selectedRow = lsm.getMinSelectionIndex();
			Object o = sorter.getValueAt(selectedRow, 2);
			infoPanel.setMarker(((CMarker) o).marker);
		}
	}

	/** Listens for changes in the table's data model.
	 * 
	 */
	public void tableChanged(TableModelEvent e) {
		if (table == null || selectingMarkers) return;
		if (e.getType() != TableModelEvent.UPDATE) return;
		if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) return;
		int row = e.getFirstRow();
		if (row == -1) return;
		CMarker cm = (CMarker) model.getValueAt(row, 2);

		boolean newState = ((Boolean) model.getValueAt(row, 3)).booleanValue();
		if (cm.checked != newState) {
			if (cm.checked) {
				selectedCount--;
			} else {
				selectedCount++;
			}
		}
		cm.checked = newState;

		updateCountLabel();
		table.repaint();

		AppFrameMenuBar.aFileSave.setEnabled(true);
	}

	private void updateCountLabel() {
		gP.setTitle("Marker Details (" + selectedCount + "/" + totalCount + " selected)");
	}

	private static Color bgClr = (Color) UIManager.get("Table.background");
	private static Color bgSel = (Color) UIManager.get("Table.selectionBackground");
	private static Color fgSel = (Color) UIManager.get("Table.selectionForeground");
	private static Color fgNor = (Color) UIManager.get("Table.foreground");
	private static Color fgLht = new Color(200, 200, 200);
	private static DecimalFormat d = new DecimalFormat("0.0000");

	private class StringRenderer extends DefaultTableCellRenderer {
		private static final long serialVersionUID = -9050614258880771966L;

		public Component getTableCellRendererComponent(JTable t, Object o, boolean isSelected, 
				boolean hasFocus, int r, int c) throws NullPointerException {
			JLabel label = (JLabel) super.getTableCellRendererComponent(t, o, false, hasFocus, r, c);
			try {
				handleLabel(label, o);
			} catch (NullPointerException e) {
				System.out.println("NullPointerException cought in  "
						+ "MarkerSNPDetails.StringRenderer - handleLabel()");
			}

			CMarker cm = (CMarker) sorter.getValueAt(r, 2);
			if (cm.marker.canEnable()) {
				label.setForeground(fgNor);
			} else {
				label.setForeground(fgLht);
			}

			label.setOpaque(isSelected);
			if (isSelected) {
				label.setBackground(bgSel);
				label.setForeground(fgSel);
			} else {
				label.setBackground(bgClr);
			}

			return label;
		}
	}

	private void handleLabel(JLabel label, Object o) throws NullPointerException {
		if (o == null) return;
		if (o.getClass().getSuperclass() == Number.class) {
			label.setHorizontalAlignment(JLabel.RIGHT);
			label.setHorizontalTextPosition(SwingConstants.RIGHT);
			Number n = (Number) o;
			if (n.doubleValue() % 1 != 0) {
				label.setText(d.format(n.doubleValue()));
				// System.out.println("formatted double: " +
				// d.format(n.doubleValue()) );
			}
		} else {
			label.setHorizontalAlignment(JLabel.LEFT);
			label.setHorizontalTextPosition(SwingConstants.LEFT);
		}
	}
}