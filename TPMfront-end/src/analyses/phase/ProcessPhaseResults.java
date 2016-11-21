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

package analyses.phase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;

import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;

class ProcessPhaseResults extends Thread {
	private OrderedResult mdsResult;
	private File file;
	private OrderedResult myresult;
	public boolean error = false;
	public boolean isRunning = true;

	ProcessPhaseResults(OrderedResult mdsResult, File file) {
		this.mdsResult = mdsResult;
		this.file = file;
	}

	public void run() {
		isRunning = true;
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));

			process(in);
			in.close();

		} catch (Exception e) {
			e.printStackTrace();

			error = true;
			MsgBox.msg("Phase was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	public boolean process(BufferedReader in) {
		float location, prevlocation = 0;
		String p1, p2;// , p1dosage, p2dosage;
		try {
			String str = in.readLine();
			StringTokenizer st = new StringTokenizer(str);
			int numMarkers = Integer.parseInt(st.nextToken());
			myresult = new OrderedResult(numMarkers, OrderedResult.ORT_PHASE);
			LinkageGroup lgrp = mdsResult.getLinkageGroup();
			lgrp.freezeSafeNames();
			CMarker cm;
			while ((str = in.readLine()) != null && str.length() > 0) {
				st = new StringTokenizer(str);
				cm = lgrp.getMarkerBySafeNameNr(Integer.parseInt(st.nextToken()));

				location = Float.parseFloat(st.nextToken());
				p1 = st.nextToken();
				p2 = st.nextToken();
				st.nextToken();
				st.nextToken();
				cm.marker.setPhenotypeInfo(p1, p2);
				myresult.addMarker(cm);
				myresult.addDistance(location - prevlocation);
				prevlocation = location;
			}
		} catch (Exception e) {
			e.printStackTrace();
			MsgBox.msg("Phase was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
			error = true;
			return false;
		}
		return true;
	}

	public OrderedResult getResult() {
		return myresult;
	}
}
