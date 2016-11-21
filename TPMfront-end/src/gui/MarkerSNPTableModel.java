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

import javax.swing.table.DefaultTableModel;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;

class MarkerSNPTableModel extends DefaultTableModel {
	private static final long serialVersionUID = 5021077977264755592L;
	private TableSorter sorter = null;
	private String[] titles = new String[] { "#", "SC Group", "Name", "Selected", "P1 dosage", 
		"P2 dosage", "Ratio", "Chi_sig", "Alpha", "DR code" };
	private String[] qtltitles = new String[] { "#", "SC Group", "Name", "P1 dosage", "P2 dosage",
		"Ratio", "Chi_sig", "Alpha", "DR code" };
	@SuppressWarnings("rawtypes")
	private Class[] types = new Class[] { Integer.class, String.class, Marker.class, Boolean.class,
		String.class,String.class, String.class, Double.class, Double.class, String.class };
	@SuppressWarnings("rawtypes")
	private Class[] qtltypes = new Class[] { Integer.class, String.class, Marker.class, String.class,
		String.class, String.class, Double.class, Double.class, String.class };

	MarkerSNPTableModel() {
		if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
			this.types = this.qtltypes;
			this.titles = this.qtltitles;
		}
		for (int i = 0; i < titles.length; i++) {
			addColumn(titles[i]);
		}
	}

	void setTableSorter(TableSorter sorter) {
		this.sorter = sorter;
	}

	public String getColumnName(int c) {
		return titles[c];
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public Class getColumnClass(int c) {
		return types[c];
	}

	public boolean isCellEditable(int r, int c) {
		if (c == 3) {
			if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) return false;
			CMarker cm = (CMarker) getValueAt(r, 2);
			if (cm.marker.canEnable()) {
				return true;
			} else if (cm.marker.getChiSig() < 0.001) {
				MsgBox.msg("Markers with Chi_sig < 0.001 are not selectable.", MsgBox.WAR);
			} else {
				MsgBox.msg("Markers not selectable.", MsgBox.WAR);
			}
		}

		return false;
	}

	void populateTable(LinkageGroup lGroup) {
		// Clear the table
		while (getRowCount() > 0) removeRow(0);

		for (CMarker cm : lGroup.getMarkers()) {
			Object[] tableData;
			if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
				tableData = getTabularDataQTL(cm, cm.getSafeNameSuffix());
			} else {
				tableData = getTabularData(cm, cm.getSafeNameSuffix());
			}
			addRow(tableData);
		}

		sorter.setTableModel(this);
	}

	// Called when auto-selection of markers has taken place. The model must be
	// updated to reflect changes in the markers' states.
	void selectedMarkersChanged(LinkageGroup lGroup) {
		int i = 0;
		for (CMarker cm : lGroup.getMarkers()) {
			setValueAt(new Boolean(cm.checked), i++, 3);
		}
	}

	public Object[] getTabularData(CMarker cm, int num) {
		Object[] data = new Object[10];
		Marker m = cm.marker;

		data[0] = new Integer(num);
		data[1] = m.getPrefix();
		data[2] = cm;
		data[3] = new Boolean(cm.checked);
		data[4] = m.getParentDosage(1);
		data[5] = m.getParentDosage(2);
		data[6] = m.getSNPRatio();
		data[7] = new Double(m.getChiSig());
		data[8] = new Double(m.getAlpha());
		data[9] = new String(m.getStatus());

		return data;
	}

	public Object[] getTabularDataQTL(CMarker cm, int num) {
		Object[] data = new Object[9];
		Marker m = cm.marker;

		data[0] = new Integer(num);
		data[1] = m.getPrefix();
		data[2] = cm;
		data[3] = m.getParentDosage(1);
		data[4] = m.getParentDosage(2);
		data[5] = m.getSNPRatio();
		data[6] = new Double(m.getChiSig());
		data[7] = new Double(m.getAlpha());
		data[8] = new String(m.getStatus());

		return data;
	}

}
