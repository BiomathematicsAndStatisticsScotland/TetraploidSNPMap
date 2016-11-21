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

package analyses.order.sim;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;

class ProcessSimResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private LinkageGroup lGroup;
	private OrderedResult order;
	private File simOut;

	ProcessSimResults(LinkageGroup lGroup, OrderedResult order, File simOut) {
		this.lGroup = lGroup;
		this.order = order;
		this.simOut = simOut;

		start();
	}

	public void run() {
		BufferedReader in = null;

		try {
			// Process the results from sim.out
			//try {
			in = new BufferedReader(new FileReader(simOut));
			processSimOut(in);
			in.close();
			order.setSimOK();
			//} catch (Exception e) {
			// for (CMarker cm: lGroup.getMarkers())
			// order.addMarker(cm);
			// error = true;
			//}
		} catch (Exception e) {
			error = true;
			e.printStackTrace(System.out);
			MsgBox.msg("Unable to complete ordering due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void processSimOut(BufferedReader in) throws Exception {
		String str = in.readLine();

		// Read down the file until the final ordering is found
		while (str.startsWith("  ****   RESULTS AFTER SA   ****") == false) {
			str = in.readLine();
		}
		str = in.readLine();

		while (str.length() > 0) {
			str = in.readLine();

			// Add this marker to the OrderedResult's list
			if (str.length() > 0) {
				order.addMarker(lGroup.getMarkerBySafeName(str.trim()));
			}
		}

		// Now find the distances
		while (str.startsWith("  DISTANCE") == false) {
			str = in.readLine();
		}
		str = in.readLine();

		while (str.length() > 0) {
			StringTokenizer st = new StringTokenizer(str);
			float distance = 100 * Float.parseFloat(st.nextToken());

			order.addDistance(distance);

			str = in.readLine();
		}
	}
}