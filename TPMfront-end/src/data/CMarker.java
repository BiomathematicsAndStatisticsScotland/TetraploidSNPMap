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

package data;

import java.io.Serializable;

import gui.Prefs;

/** CMarker. SNP & NONSNP.
 * 
 * <p>Wrapper class for Marker that provides the ability for the same Marker within
 * different LinkageGroups (possible from different analyses results) to have a
 * different checked state
 *
 */
public class CMarker implements Serializable, Comparable<CMarker> {
	static final long serialVersionUID = -5022081160204683415L;

	public boolean checked = true;
	public Marker marker = null;
	public String safeName;

	/** CMarker(). SNP & NONSNP.
	 * 
	 * @param marker = the marker
	 * @param safeNameSuffix = the number for the 'safename'
	 */
	public CMarker(Marker marker, int safeNameSuffix) {
		this.marker = marker;

		checked = marker.canEnable();
		setSafeName(safeNameSuffix);
	}

	/**
	 * compareTo(CMarker cm2).
	 * to help order CMarkers by safenameSuffix.
	 */
	public int compareTo(CMarker cm2) {
		if (this.getSafeNameSuffix() < cm2.getSafeNameSuffix()) return -1;
		if (this.getSafeNameSuffix() > cm2.getSafeNameSuffix()) return 1;
		return 0;
	}

	/** SetSafeName.
	 * This is where the "safe" name is given to each CMarker depending on the
	 * LENGTH of the Vector&lt;CMarker&gt; when the Markers are first read and loaded
	 * to the LinkageGroup See: LinkageGroup.addMarker(marker)
	 * 
	 * <p>If the marker is of type (4) SNP then a longer "safe" name suffix is
	 * given.
	 */
	public void setSafeName(int safeNameSuffix) {
		if (this.marker.getType() == 4) {
			safeName = "mkr" + Prefs.i6.format(safeNameSuffix);
		} else {
			safeName = "mkr" + Prefs.i3.format(safeNameSuffix);
		}
	}

	public String toString() {
		return marker.getName();
	}

	/*
	 * getting the index should be done via
	 * LinkageGroup.getMarkerIndexBySafeName
	 * 
	 * public int getIndex() { return Integer.parseInt(safeName.substring(3));
	 * //return Integer.parseInt(safeName.substring(3))-1; }
	 */
	public int getSafeNameSuffix() {
		return Integer.parseInt(safeName.substring(3));
	}

	/** getClone().
	 * creates a clone with the same safename.
	 * 
	 */
	public CMarker getClone() {
		CMarker clone = new CMarker(marker, getSafeNameSuffix());
		// clone.prefix = prefix;

		return clone;
	}
}