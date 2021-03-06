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

import data.CMarker;
import data.LinkageGroup;
import data.Marker;

/**
 * "Select Markers" filter applied every time the "OK" button is pressed in the SelectSNPMarkersDialog. 
 */
public class SelectSNPMarkers {
	static void doSelection(LinkageGroup lGroup) {
		for (CMarker cm : lGroup.getMarkers()) {
			// If we're only matching against markers that are not yet
			// selected, and this one is, then go no further
			if (Prefs.gui_select_match_selected && cm.checked) continue;

			if (cm.marker.canEnable() == false) continue;

			// Start by setting the marker to selected
			cm.checked = true;

			// Now analyse the marker itself
			Marker marker = cm.marker;

			// TV : Left for historical reasons
			// Select on TYPE
			if (Prefs.gui_select_on_type) {
				if (marker.getType() != Prefs.gui_select_type) {
					cm.checked = false;
				}
			}

			// TV TODO
			// Select on PARENT | when cParent checkbox selected
			if (Prefs.gui_select_on_parent) {
				// Is it present in parent 1?
				if (Prefs.gui_select_parent == 1) {
					if (Integer.parseInt(marker.getParentDosage(2)) != 0) {
						cm.checked = false;
					}
				} else if (Prefs.gui_select_parent == 2) {
					if (Integer.parseInt(marker.getParentDosage(1)) != 0) {
						cm.checked = false;
					}
				} else if (Prefs.gui_select_parent == 3) {
					if (Integer.parseInt(marker.getParentDosage(1)) == 0
							&& Integer.parseInt(marker.getParentDosage(2)) == 0) {
						cm.checked = false;
					}
				}
			}

			// Select on (SNP)RATIO | when cRatio checkbox is selected
			if (Prefs.gui_select_on_ratio) {
				if (marker.getRatioCode() != Prefs.gui_select_ratio
						|| marker.getChiSig() < Prefs.gui_select_ratio_sig) {
					cm.checked = false;
				}
			}

			// Select on DOUBLE REDUCTION
			if (Prefs.gui_select_on_dr) {
				// Looking for YES on DR
				if (Prefs.gui_select_dr 
						&& marker.getDRSignificance() > Prefs.gui_select_dr_sig) {
					cm.checked = false;
				} else if (Prefs.gui_select_dr == false 
						&& marker.getDRSignificance() <= Prefs.gui_select_dr_sig) {
					cm.checked = false;
				}
			}
		}
	}

	// TV TODO: implement for SNP
	static void selectAll(LinkageGroup lGroup) {
		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.marker.canEnable()) {
				cm.checked = true;
			}
		}
	}

	static void selectNone(LinkageGroup lGroup) {
		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.marker.canEnable()) {
				cm.checked = false;
			}
		}
	}

	static void selectInvt(LinkageGroup lGroup) {
		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.marker.canEnable()) {
				cm.checked = !cm.checked;
			}
		}
	}
}
