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
import java.util.Vector;

/** NONSNP only.
 * 
 * 
 *
 */
public class AnovaTraitResult implements Serializable {
	private static final long serialVersionUID = 5178486695080009861L;
	private String traitName;
	private Vector<Marker> markers = new Vector<Marker>();
	private Vector<String> data = new Vector<String>();

	public AnovaTraitResult(String name) {
		traitName = name;
	}

	public void addMarker(Marker m, String d) {
		markers.add(m);
		data.add(d);
	}

	public String toString() {
		return "(" + markers.size() + ") " + traitName;
	}

	public String getTraitName() {
		return traitName;
	}

	public int getResultCount() {
		return markers.size();
	}

	public Marker getMarker(int index) {
		return markers.get(index);
	}

	public String getData(int index) {
		return data.get(index);
	}
}