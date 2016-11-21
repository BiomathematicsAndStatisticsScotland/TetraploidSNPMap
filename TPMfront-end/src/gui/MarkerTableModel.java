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

class MarkerTableModel extends DefaultTableModel {
	private static final long serialVersionUID = 4512603380774989078L;
	private TableSorter sorter = null;
	static String[] titles = new String[] { "#", "SC Group", "Name", "Selected", "Type", "# of Alleles", 
		"Phenotype 1","Phenotype 2", "Genotype", "Ratio", "Ratio_Sig", "Alpha", "DR Sig" };

	@SuppressWarnings("rawtypes")
	static Class[] types = new Class[] { Integer.class, String.class, Marker.class, Boolean.class,
		String.class,Integer.class, String.class, String.class, String.class, String.class, 
		Double.class, Double.class,Double.class };

	MarkerTableModel() {
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
			CMarker cm = (CMarker) sorter.getValueAt(r, 2);
			if (cm.marker.canEnable()) {
				return true;
			} else {
				MsgBox.msg("Markers where the best genotype cannot be "
						+ "determined or markers with no data on absense of double " 
						+ "reduction are not selectable.",
						MsgBox.WAR);
			}
		}
		return false;
	}

	void populateTable(LinkageGroup lGroup) {
		// Clear the table
		while (getRowCount() > 0) removeRow(0);

		int markerNum = 1;
		for (CMarker cm : lGroup.getMarkers()) {
			Object[] tableData = getTabularData(cm, markerNum++);
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

	// This could/should really be in the Marker class, but it's easier to keep
	// an eye on the table and its columns if it's all kept here
	public Object[] getTabularData(CMarker cm, int num) {
		Object[] data = new Object[13];
		Marker m = cm.marker;

		data[0] = new Integer(num);
		data[1] = m.getPrefix();
		data[2] = cm;
		data[3] = new Boolean(cm.checked);
		data[4] = m.getTypeDescription(true);
		data[5] = new Integer(m.getAlleleCount());
		data[6] = m.getPhenotypeInfo(1).substring(0, 4);
		data[7] = m.getPhenotypeInfo(2).substring(0, 4);
		data[8] = m.getRatioGenotypes();
		data[9] = m.getRatio();
		data[10] = new Double(m.getRatioSignificance());
		data[11] = new Double(m.getAlpha());
		data[12] = new Double(m.getDRSignificance());

		return data;
	}
}
