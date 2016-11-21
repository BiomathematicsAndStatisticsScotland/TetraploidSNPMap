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

package analyses.simmatch;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;
import data.LinkageGroup;
import data.Marker;
import data.SigLinkage;
import doe.MsgBox;

class ProcessResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private LinkageGroup lGroup;
	private File nameFile, parentFile;

	ProcessResults(LinkageGroup lGroup, File nameFile, File parentFile) {
		this.lGroup = lGroup;
		this.nameFile = nameFile;
		this.parentFile = parentFile;

		start();
	}

	public void run() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(nameFile));

			processNames(in);
			in.close();

			in = new BufferedReader(new FileReader(parentFile));
			processParentData(in);
			in.close();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("SimMatch was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void processNames(BufferedReader in) throws Exception {
		String str = in.readLine();
		while (str != null && str.length() > 0) {
			String name = str.trim();

			String markerName = name.substring(name.indexOf("mkr"));
			String prefix = name.substring(0, name.lastIndexOf("_"));

			Marker marker = lGroup.getMarkerBySafeName(markerName).marker;
			marker.setPrefix(prefix);

			str = in.readLine();
		}
	}

	private void processParentData(BufferedReader in) throws Exception {
		int dataCount = 0;

		String str = in.readLine();
		while (str != null) {
			if (str.equals(" Summary of significant linkages")) {
				processLinkages(++dataCount, in);
			}

			str = in.readLine();
		}
	}

	private void processLinkages(int dataCount, BufferedReader in) throws Exception {
		String str = in.readLine();
		while (str != null && str.length() > 0) {
			StringTokenizer st = new StringTokenizer(str);

			Marker m1 = getMarker(st.nextToken());
			Marker m2 = getMarker(st.nextToken());
			float chi = Float.parseFloat(st.nextToken());
			float sig = Float.parseFloat(st.nextToken());
			String phase = null;
			if (st.hasMoreTokens()) phase = st.nextToken();

			// Add the data for marker 1
			SigLinkage s1 = new SigLinkage(m2, chi, sig, phase);
			m1.getSimMatchData().add(getSigLinkage(m1, dataCount), s1);

			// Add the data for marker 2
			SigLinkage s2 = new SigLinkage(m1, chi, sig, phase);
			m2.getSimMatchData().add(getSigLinkage(m2, dataCount), s2);

			str = in.readLine();
		}
	}

	// Finds a marker based on its safename or prefix_safeName
	private Marker getMarker(String name) {
		if (name.startsWith("mkr")) {
			return lGroup.getMarkerBySafeName(name).marker;
		} else {
			name = name.substring(name.indexOf("mkr"));
			return lGroup.getMarkerBySafeName(name).marker;
		}
	}

	// Returns the appropriate vector of data for the given marker and the given
	// group - groups are 1-6
	private Vector<SigLinkage> getSigLinkage(Marker marker, int dataCount) {
		switch (dataCount) {
			case 1:
				return marker.getSimMatchData().p1A;
			case 2:
				return marker.getSimMatchData().p2A;
			case 3:
				return marker.getSimMatchData().p1B;
			case 4:
				return marker.getSimMatchData().p2B;
			case 5:
				return marker.getSimMatchData().p1C;
			case 6:
				return marker.getSimMatchData().p2C;
			default:
				System.out.println("sig linkage group should be 1-6. found" + dataCount);
				return null;
		}
	}
}