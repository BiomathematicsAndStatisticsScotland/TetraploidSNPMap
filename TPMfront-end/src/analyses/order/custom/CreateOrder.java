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

package analyses.order.custom;

import java.util.Vector;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import data.PhasePair;

// Given a list of markers, creates a new (ordered) list using the following
// algorithm:
//
//  1) Start with the marker M1 at the given index and add it to the list
//  2) Find the marker M2 whose R (recombination fraction) score with M1 is the
//     lowest
//  3) For all remaining markers:
//      Find the next marker Mn whose R is lowest with M1
//      Add it at the start or at the end of the list - determined by which
//      marker at the start or end has the lowest R score with it

class CreateOrder {
	private LinkageGroup lGroup;
	// private OrderedResult result;
	private int num;

	// The unordered list of markers available for adding to the ordered list
	private Vector<CMarker> buffer;

	PhasePair[][] ppData;

	CreateOrder(OrderedResult result, LinkageGroup lGroup, PhasePair[][] ppData) {
		// this.result = result;
		this.lGroup = lGroup;
		this.ppData = ppData;

		num = lGroup.getMarkerCount();
	}

	Vector<CMarker> getOrder(int start) {
		// Storage for the current ordered list
		Vector<CMarker> ordered = new Vector<CMarker>(num);

		// Buffer of markers that are still to be added
		buffer = getNewDataSet();

		// (1) // Start with the marker at the given index
		CMarker cm = buffer.remove(start);
		ordered.add(cm);

		// (2) // And find a marker to place beside it
		ordered.add(getMarkerWithSmallestR(cm));

		// While there are markers left in the buffer
		while (buffer.size() > 0) {
			// (3) // Find next marker with smallest R
			CMarker toAdd = getMarkerWithSmallestR(cm);

			// And decide whether to put it at the start or the end of the order
			CMarker sm = ordered.firstElement();
			CMarker em = ordered.lastElement();

			// float sR = ppData[sm.getIndex()][toAdd.getIndex()].rfq;
			// float eR = ppData[em.getIndex()][toAdd.getIndex()].rfq;
			float sR = ppData[sm.getSafeNameSuffix() - 1][toAdd.getSafeNameSuffix() - 1].rfq;
			float eR = ppData[em.getSafeNameSuffix() - 1][toAdd.getSafeNameSuffix() - 1].rfq;
			// Add it at the start
			if (sR <= eR) {
				ordered.add(0, toAdd);
			// Add it at the end
			} else {
				ordered.add(toAdd);
			}
		}

		if (MatrixHandler.print) {
			System.out.println("\nList " + start + ":");
			for (CMarker mkr : ordered) {
				System.out.println(mkr);
			}
		}

		return ordered;
	}

	// Searches the list of markers (and phasepairs) to find the marker M2 with
	// the lowest recombination (R) value with the given marker M1
	private CMarker getMarkerWithSmallestR(CMarker m1) {
		// The marker we hope to find...
		CMarker cm = null;
		// And the lowest R found so far
		float lR = -1;
		// System.out.println("m1: " + m1.marker.getName() + " index: " +
		// m1.getIndex());
		// For each marker...
		for (CMarker m2 : buffer) {
			// System.out.println("m2: " + m2.marker.getName() + " index: " +
			// m2.getIndex());

			if (m1 == m2) continue;

			// ... check its r value with m1
			// PhasePair pp = findPhasePair(m1, m2);
			PhasePair pp = ppData[m1.getSafeNameSuffix() - 1][m2.getSafeNameSuffix() - 1];

			if (pp == null) {
				System.out.println(
						"PP NULL with " + (m1.getSafeNameSuffix() - 1) 
						+ " and " + (m2.getSafeNameSuffix() - 1));
				System.out.println(m1 + " and " + m2);
			}

			if (lR == -1 || pp.rfq < lR) {
				cm = m2;
				lR = pp.rfq;
			}
		}

		buffer.remove(cm);
		return cm;
	}

	// Creates a temporary list of markers that can be used as a buffer for
	// holding them until they are moved into the ordered list
	private Vector<CMarker> getNewDataSet() {
		LinkageGroup newGroup = lGroup.getClonedLinkageGroup(false, true);

		return newGroup.getMarkers();
	}

	/*
	 * // Finds the PhasePair that contains the given two markers private
	 * PhasePair findPhasePair(CMarker m1, CMarker m2) { for (PhasePair pp:
	 * phasePairs) if (pp.hasMarkers(m1, m2)) return pp;
	 * 
	 * return null; }
	 */
}