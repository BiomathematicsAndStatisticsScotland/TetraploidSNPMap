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

package analyses.order.mds;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import data.PhasePair;
import doe.MsgBox;

class ProcessMDSResults extends Thread {
	private File file;
	private File imageFile;
	private OrderedResult twopointresult;
	private OrderedResult myresult;
	public boolean error = false;
	public boolean isRunning = true;

	ProcessMDSResults(File file, OrderedResult twopointresult, File imageFile) {
		this.file = file;
		this.twopointresult = twopointresult;
		this.imageFile = imageFile;
	}

	public void run() {
		isRunning = true;
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			process(in);
			in.close();
			myresult.MDSimage = new ImageIcon(ImageIO.read(imageFile));

		} catch (Exception e) {
			e.printStackTrace(System.out);
			error = true;
			MsgBox.msg("MDS was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	public boolean process(BufferedReader in) {
		float location = 0;
		float prevlocation = 0;

		try {
			String str = in.readLine();
			int numMarkers = Integer.parseInt(str);
			myresult = new OrderedResult(numMarkers, OrderedResult.ORT_MDS);
			LinkageGroup lgrp = twopointresult.getLinkageGroup();
			lgrp.freezeSafeNames();
			myresult.getLinkageGroup().freezeSafeNames();
			myresult.translateIndexes = new int[lgrp.getMarkerCount()];
			myresult.translatePPIndexes = new int[lgrp.getMarkerCount()];
			int i = 0;
			while ((str = in.readLine()) != null && str.length() > 0) {
				String[] st = str.split(",");
				int themarkerIndex = Integer.parseInt(st[0]);
				CMarker themarker = lgrp.getMarkerBySafeNameNr(themarkerIndex);
				myresult.addMarker(themarker);

				myresult.translateIndexes[i] = themarkerIndex;
				myresult.translatePPIndexes[i] = find_marker_in_pp(twopointresult.getPhasePairArray(),
						lgrp.getMarkerBySafeNameNr(themarkerIndex).marker.getName());
				location = Float.parseFloat(st[1]);
				myresult.addDistance(location - prevlocation);
				prevlocation = location;
				i++;
			}
			myresult.setPhasePairArray(twopointresult.getPhasePairArray());
		} catch (Exception e) {
			e.printStackTrace(System.out);
			MsgBox.msg("MDS was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
			error = true;
			return false;
		}
		return true;
	}

	private int find_marker_in_pp(PhasePair[][] ppa, String markername) {
		for (int i = 0; i < ppa[0].length; i++) {
			if (i == 0) {
				if (ppa[1][0].cm1.marker.getName().equals(markername)) {
					return 0;
				}
			}
			if (ppa[0][i] != null) {
				if (ppa[0][i].cm2.marker.getName().equals(markername)) {
					return i;
				}
			}
		}
		return -1;
	}

	public OrderedResult getResult() {
		return myresult;
	}
}
