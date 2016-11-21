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

package analyses.findsnpgeno;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.CMarker;
import data.LinkageGroup;
import doe.MsgBox;

/** ProcessRecalcResults reads the results from RunFixDRNP to update chi^2 for the fixed markers.
 *
 */
public class ProcessRecalcResults extends Thread {
	// How many lines have been processed?
	int locusCount = 0;
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;
	//private int num_np = 0, num_dr = 0;
	
	private LinkageGroup lGroup;
	private File file;
	//private AppFrame appFrame;
	//private String errors = "";
	
	/** 
	 * 
	 * @param lGroup the lGroup to be updated.
	 * @param file the results file from RunFixDRNP.
	 */
	ProcessRecalcResults(LinkageGroup lGroup, File file) {
		this.lGroup = lGroup;
		this.file = file;
		start();
	}
	
	/**
	 * updates the lGroup with the marker chi^2 data from file.
	 */
	public void run() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			StringTokenizer st;
			String iname, chisig;
			
			String line = in.readLine();
			line = in.readLine();
			while (line != null) {
				st = new StringTokenizer(line);
				iname = st.nextToken();
				CMarker cm = lGroup.getMarkerByName(iname);
				chisig = st.nextToken();
				cm.marker.setChiSig(Double.parseDouble(chisig));
				//System.out.println(cm.marker.getName() + " " +  cm.marker.getChiSig());
				line = in.readLine();
			}
			in.close();
		} catch (Exception e) {
			error = true;
			e.printStackTrace(System.out);
			System.out.println("Exception cought in: ProcessRecalcResults.run() :: " + e);
			MsgBox.msg("error reading result file in ProcessRecalcResult.run().", MsgBox.ERR);
		}
		
		isRunning = false;
	}
}

