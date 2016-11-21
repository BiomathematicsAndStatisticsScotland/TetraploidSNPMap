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

package gui.map;

import java.util.Vector;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import gui.AppFrame;

public class MapCreator {
	private OrderedResult order;
	private LinkageGroup[] lGroups;

	float totalDistance;

	GMarker[] chromoAll = null;
	Vector<GMarker[]> chromosomes = null;

	public MapCreator(OrderedResult order, LinkageGroup[] lGroups) {
		this.order = order;
		this.lGroups = lGroups;

		calculateDistances();
		populateChromosomes();

		for (int i = 0; i < chromosomes.size(); i++) {
			GMarker[] markers = chromosomes.get(i);
			setupInternalReferences(markers);
		}
	}

	private void calculateDistances() {
		totalDistance = order.getDistanceTotal();
		LinkageGroup lGroup = order.getLinkageGroup();
		chromoAll = new GMarker[lGroup.getMarkerCount()];

		Vector<CMarker> markers = lGroup.getMarkers();
		Vector<Float> distances = order.getDistances();

		float cmValue = 0, aValue = 0;
		for (int i = 0; i < chromoAll.length; i++) {
			CMarker mkr = markers.get(i);
			chromoAll[i] = new GMarker(mkr, "" + mkr);

			if ((i > 0) && (AppFrame.tpmmode != AppFrame.TPMMODE_SNP)) {
				float dist = distances.get(i - 1);
				float aPos = dist / totalDistance;

				aValue += aPos;
				cmValue += dist;
			} else if (AppFrame.tpmmode == AppFrame.TPMMODE_SNP) {
				float dist = distances.get(i);
				float aPos = dist / totalDistance;

				aValue += aPos;
				cmValue += dist;
			}

			chromoAll[i].aPos = aValue;
			chromoAll[i].cm = cmValue;

		}
	}

	private void populateChromosomes() {
		// Take the markers from each of the chromosomes produced by the
		// analysis
		// and put them into the seperate GMarker arrays (one for each
		// chromosome)
		chromosomes = new Vector<GMarker[]>(lGroups.length + 1);
		chromosomes.add(chromoAll);

		// For each chromosome
		for (int i = 0; i < lGroups.length; i++) {
			// Create it...
			GMarker[] chromo = new GMarker[lGroups[i].getMarkerCount()];
			chromosomes.add(chromo);

			// Populate it...
			Vector<CMarker> markers = lGroups[i].getMarkers();
			for (int j = 0; j < markers.size(); j++) {
				CMarker cm = markers.get(j);

				// By finding the matching GMarker for this CMarker
				for (int k = 0; k < chromoAll.length; k++) {
					if (cm.marker.getName().equals(chromoAll[k].name)) {
						chromo[j] = chromoAll[k].getClone();
						break;
					}
				}
			}
		}
	}


	private void setupInternalReferences(GMarker[] markers) {
		GMarker prev = null;
		for (int i = 0; i < markers.length; i++) {
			markers[i].prev = prev;
			prev = markers[i];
		}
	}
}