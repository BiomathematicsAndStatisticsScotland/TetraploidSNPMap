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

package analyses.order;

import java.util.Vector;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;


/** LinkageAnalysis. Used for SNP, QTL or NONSNP mode.
 *  it copies the markers into 4 (NONSNP) or 8 (SNP) linkage groups.
 * 
 *
 */
public class LinkageAnalysis {
	private OrderedResult order;
	private Vector<String[]> rows;
	private int parent = -1;

	private LinkageGroup[] groups;

	/** LinkageAnalysis(order). SNP only. no parent given. create 8 linkage groups.
	 * 
	 * @param order =  OrderedResult 
	 */
	public LinkageAnalysis(OrderedResult order) {
		// TPMMODE_QTL - no parent given (SNP only)
		this.order = order;
		rows = order.rows;
		run();
	}

	/** LinkageAnalysis(order). NONSNP only.  parent given. create 4 linkage groups.
	 * 
	 * @param order = OrderedResult
	 * @param p = the chosen parent (1 or 2)
	 */
	public LinkageAnalysis(OrderedResult order, int p) {
		// TPMMODE_NONSNP
		this.order = order;
		rows = order.rows;
		parent = p;
		run();
	}

	private void run() {
		if (this.parent == -1) {
			runQTL();
		} else {
			runNonQTL();
		}
	}

	private void runQTL() {
		// Contains the set of ordered markers
		LinkageGroup lGroup = order.getLinkageGroup().getClonedLinkageGroup(false, true);

		// The four groups we want to split it up into
		groups = new LinkageGroup[8];
		for (int i = 0; i < groups.length; i++) {
			groups[i] = new LinkageGroup("Chromosome " + (i + 1));
		}
		for (CMarker cm : lGroup.getMarkers()) {
			for (int c = 0; c < 4; c++) {
				if (cm.marker.getPhenotypeInfo(1).charAt(c) == '2') {
					groups[c].addMarker(cm.marker);
				}
				if (cm.marker.getPhenotypeInfo(2).charAt(c) == '2') {
					groups[4 + c].addMarker(cm.marker);
				}
			}
		}
	}

	private void runNonQTL() {
		// Contains the set of ordered markers
		LinkageGroup lGroup = order.getLinkageGroup().getClonedLinkageGroup(false, true);

		// The four groups we want to split it up into
		groups = new LinkageGroup[4];
		for (int i = 0; i < groups.length; i++) {
			groups[i] = new LinkageGroup("Chromosome " + (i + 1));
		}

		for (int i = 0; i < rows.size(); i++) {
			String[] str = rows.get(i);
			String data = str[parent - 1];

			CMarker cm = lGroup.getMarkers().get(i);

			for (int c = 0; c < 4; c++) {
				if (data.charAt(c) != '0') {
					groups[c].addMarker(cm.marker);
				}
			}
		}
	}

	public LinkageGroup[] getGroups() {
		return groups;
	}
}
