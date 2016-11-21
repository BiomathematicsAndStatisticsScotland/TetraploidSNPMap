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

package analyses.readqtl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;

class ProcessReadQTLResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private File maplocfile;
	private LinkageGroup lGroup;
	private OrderedResult result;
	private File outputfile;
	private String outfileerrors = "";

	ProcessReadQTLResults(File maplocfile, File outputfile, LinkageGroup lGroup) {
		this.maplocfile = maplocfile;
		this.lGroup = lGroup;
		this.outputfile = outputfile;

		start();
	}

	public void run() {
		try {
			read_maploc();
			read_outfile();
		} catch (Exception e) {
			e.printStackTrace(System.out);

			error = true;
			MsgBox.msg("ReadQTL was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void read_outfile() throws Exception {
		BufferedReader in = new BufferedReader(new FileReader(outputfile));
		String str, ec, m, error;
		while ((str = in.readLine()) != null) {
			if (str.startsWith(" Q00")) {
				str = str.trim();
				outfileerrors += str;
				error = str;
				ec = str.substring(0, str.indexOf(" "));

				if (str.trim().endsWith("marker")) {
					str = in.readLine();
					if (str != null) {
						outfileerrors += " " + str;
						error += " " + str;
						m = str.trim();
					} else {
						break;
					}
				} else {
					m = str.substring(str.lastIndexOf(" "));
				}
				if (ec.equals("Q0003") || ec.equals("Q0005") || ec.equals("Q0002")) {
					CMarker cm = result.getLinkageGroup().getMarkerByName(m);
					if (cm != null) cm.marker.addError(error);
				}
				outfileerrors += "\n";
			}
		}
		in.close();
		if (!outfileerrors.isEmpty()) {
			MsgBox.msg("ReadQTL problem(s) with marker file:\n\n" + outfileerrors, MsgBox.ERR);
		}
	}

	private void read_maploc() throws Exception {

		BufferedReader in = new BufferedReader(new FileReader(maplocfile));

		float distance;
		String mname, p1ph, p2ph;

		float prevdist = 0;
		String str = in.readLine();
		CMarker m;
		StringTokenizer st = new StringTokenizer(str);
		// int noffspring = Integer.parseInt(st.nextToken());
		st.nextToken();
		int nmarkers = Integer.parseInt(st.nextToken());
		result = new OrderedResult(nmarkers, OrderedResult.ORT_READQTL);
		boolean firstone = true;
		while ((str = in.readLine()) != null && str.length() > 0) {
			st = new StringTokenizer(str);
			mname = st.nextToken();
			distance = Float.parseFloat(st.nextToken());
			p1ph = st.nextToken();
			p2ph = st.nextToken();
			m = lGroup.getMarkerByName(mname);
			m.marker.setPhenotypeInfo(p1ph, p2ph);
			result.addMarker(m);

			if (!firstone) {
				result.addDistance(distance - prevdist);// + prevdist);
			}
			firstone = false;
			// System.out.println("READQTL adding distance: " +
			// (distance-prevdist));
			prevdist = distance;
		}
		result.addDistance(0);
		in.close();
		result.setMaplocResult(maplocfile);
	}

	OrderedResult getResult() {
		return result;
	}

}