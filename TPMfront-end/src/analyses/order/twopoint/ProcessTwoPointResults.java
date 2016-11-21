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

package analyses.order.twopoint;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.StringTokenizer;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import data.PhasePair;
import doe.MsgBox;
import gui.Prefs;

class ProcessTwoPointResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private LinkageGroup lGroup;
	private OrderedResult order;
	private File twoOut, twoPwd;

	ProcessTwoPointResults(LinkageGroup lGroup, OrderedResult order, File twoOut, File twoPwd) {
		this.lGroup = lGroup;
		this.order = order;
		this.twoOut = twoOut;
		this.twoPwd = twoPwd;

		start();
	}

	public void run() {
		BufferedReader in = null;

		try {
			// Process the results from twopoint.out
			in = new BufferedReader(new FileReader(twoOut));
			processTwoOut(in);
			in.close();

			// Process the results from twopoint.pwd
			in = new BufferedReader(new FileReader(twoPwd));
			processTwoPwd(in);
			in.close();
		} catch (Exception e) {
			error = true;
			e.printStackTrace(System.out);
			MsgBox.msg("Unable to complete ordering due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void processTwoOut(BufferedReader in) throws Exception {
		String str = in.readLine();
		while (str != null) {
			if (str.startsWith(" The most likely phase")) {
				readPhase(in);
			}
			str = in.readLine();
		}
	}

	private void readPhase(BufferedReader in) throws Exception {
		StringTokenizer st1 = new StringTokenizer(in.readLine());
		StringTokenizer st2 = new StringTokenizer(in.readLine());

		CMarker marker1;
		CMarker marker2;
		String safename1 = st1.nextToken();
		String safename2 = st2.nextToken();
		marker1 = lGroup.getMarkerBySafeName(safename1);
		marker2 = lGroup.getMarkerBySafeName(safename2);
		// Read the phase strings
		String phase1 = st1.nextToken() + " " + st1.nextToken();
		String phase2 = st2.nextToken() + " " + st2.nextToken();
		order.addPhasePair(marker1, marker2, phase1, phase2);
	}

	private void processTwoPwd(BufferedReader in) throws Exception {
		in.readLine();
		String str = null;

		while ((str = in.readLine()) != null && str.length() > 0) {
			StringTokenizer st = new StringTokenizer(str);

			// Read (and find) the two marker names
			// TV edit
			CMarker marker1;
			CMarker marker2;
			marker1 = lGroup.getMarkerBySafeName(st.nextToken());
			marker2 = lGroup.getMarkerBySafeName(st.nextToken());

			// Read the numerical values
			float rfq = Float.parseFloat(st.nextToken());
			float lod = Float.parseFloat(st.nextToken());
			order.setPhasePairValues(marker1, marker2, rfq, lod);
		}
	}

	boolean verifyPhaseData(PhasePair[][] ppData, File file) {
		String failed = new String();

		for (int i = 0; i < lGroup.getMarkerCount(); i++) {
			for (int j = i + 1; j < lGroup.getMarkerCount(); j++) {
				if (i != j && ppData[i][j] == null) {
					failed += lGroup.getMarkerBySafeName("mkr" + Prefs.i3.format(i)) + " and "
							+ lGroup.getMarkerBySafeName("mkr" + Prefs.i3.format(j)) + Prefs.sepL;
				}
			}
		}

		if (failed.length() > 0) {
			try {
				BufferedWriter out = new BufferedWriter(new FileWriter(file));
				out.write(failed);
				out.close();
			} catch (Exception e) {
				System.out.println("ProcessTwopointResults.verifyPhaseData() failed write.");
			}

			MsgBox.msg("The two-point analysis failed because one or more "
					+ "pairs of markers could not be processed. A list of these\n" 
					+ "markers has been written to "
					+ file + "\nYou may wish to " + "remove them from future analyses.", MsgBox.ERR);

			return false;
		} else {
			return true;
		}
	}

	// Performs a pre-TwoPoint check on the linkage group to ensure that only
	// markers present in the same parent (or both parents) have been selected
	static boolean verifyForTwoPoint(LinkageGroup lGroup) {
		int parent = 0;

		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.marker.isPresentInParent(1) && cm.marker.isPresentInParent(2)) {
				continue;
			}

			int isPresentIn = (cm.marker.isPresentInParent(1) ? 1 : 2);
			if (parent == 0) {
				parent = isPresentIn;
			}

			if (isPresentIn != parent) {
				MsgBox.msg("This analysis can only be run on markers whose "
						+ "alleles are present in the same parent. Please select "
						+ "markers\nthat are either present in both parents, or are "
						+ "only present in parent 1 OR parent 2.", MsgBox.ERR);
				return false;
			}
		}

		return true;
	}
}