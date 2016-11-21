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

package analyses.order.ripple;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;

class ProcessRippleResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private LinkageGroup lGroup;
	private OrderedResult order;
	private File rippler;

	ProcessRippleResults(LinkageGroup lGroup, OrderedResult order, File rippler) {
		this.lGroup = lGroup;
		this.order = order;
		this.rippler = rippler;

		start();
	}

	public void run() {
		BufferedReader in = null;

		// Process the results from rippler.txt
		try {
			in = new BufferedReader(new FileReader(rippler));
			process(in);
			in.close();
		} catch (Exception e) {
			error = true;
			e.printStackTrace(System.out);
			MsgBox.msg("Unable to complete ordering due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void process(BufferedReader in) throws Exception {
		String str = in.readLine();

		// Read down the file until the final ordering is found
		while (str.startsWith(" MARKER") == false) {
			str = in.readLine();
		}

		str = in.readLine();

		// Now for each remaining line
		for (int count = 0; str != null; count++, str = in.readLine()) {
			StringTokenizer st = new StringTokenizer(str);

			String name = st.nextToken();
			order.addMarker(lGroup.getMarkerBySafeName(name));
			st.nextToken();

			// Distances are intermarker, so we don't need the first one
			if (count > 0) {
				// float distance = Float.parseFloat(st.nextToken());
				float distance = 100 * Float.parseFloat(st.nextToken());
				order.addDistance(distance);
			}
		}
	}
}