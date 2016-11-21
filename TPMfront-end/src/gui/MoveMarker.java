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

import javax.swing.JOptionPane;

import data.CMarker;
import data.Cluster;
import data.LinkageGroup;
import doe.MsgBox;

class MoveMarker {
	MoveMarker(CMarker cm, LinkageGroup currentGroup, Cluster cluster) {
		int groupCount = cluster.getGroups().size();
		Object[] values = new Object[groupCount - 1];
		int i = 0;
		for (LinkageGroup lGroup : cluster.getGroups()) {
			if (lGroup != currentGroup) {
				values[i++] = lGroup.getName();
			}
		}

		Object selectedValue = JOptionPane.showInputDialog(MsgBox.frm, 
				"Select which group to move to:", "Move Marker",
				JOptionPane.QUESTION_MESSAGE, null, values, values[0]);

		if (selectedValue == null) return;

		// System.out.println("Selected " + selectedValue);
		for (LinkageGroup lGroup : cluster.getGroups()) {
			if (lGroup.getName().equals(selectedValue) == false) {
				continue;
			}
			if (AppFrame.tpmmode == AppFrame.TPMMODE_SNP) {
				lGroup.freezeSafeNames();
				currentGroup.freezeSafeNames();
				lGroup.addMarker(cm);
				lGroup.sortMarkers();
			} else {
				lGroup.addMarker(cm.marker);
			}

			currentGroup.removeMarker(cm);

			break;
		}

		AppFrameMenuBar.aFileSave.setEnabled(true);
	}
}