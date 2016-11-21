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

package analyses.fastcluster;

import java.io.File;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.InputStream;
import java.util.StringTokenizer;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import gui.Prefs;
import doe.MsgBox;

class RunChimatrix extends FortranRunner {
	// How many lines have been processed?
	int step = 0;
	// How many linkage groups should Cluster try to create
	private float numToSend;
	private int nloci; //, nindividuals; 
	private String command;
	
	RunChimatrix(float numToSend, int nloci, int nindividuals) {
		this.nloci = nloci;
		this.numToSend = numToSend;
		if (numToSend < 0) {
			this.command = Prefs.tools_SNPcluster_path;
		} else {
			this.command = Prefs.tools_cluster_chimatrixonly_path;
		}
	}
	
	public void run() {
		try {
			proc = Runtime.getRuntime().exec(command, null,
				new File(Prefs.tools_scratch));
			
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				proc.getOutputStream()));
			
			new ClusterCatcher(proc.getInputStream());
			new ClusterCatcher(proc.getErrorStream());
			
			writer.println("cluster");
			if (numToSend < 0) writer.println("" + (-1 * numToSend));
			writer.close();
			
			proc.waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("The ChiMatrix process has failed due to the following "
				+ "error:\n" + e, MsgBox.ERR);
		}
		isRunning = false;
	}


	class ClusterCatcher extends StreamCatcher {
		public String errormsg = "";

		ClusterCatcher(InputStream in) {
			super(in);
		}
		
		
		protected void processLine(String line) {	
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);
			} else if (line.startsWith(" pr")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				if (st.hasMoreTokens()) {
					int read = Integer.parseInt(st.nextToken());
					int calc = nloci;
					while (read > 0) {
						calc += (nloci - read);
						read--;
					}
					if (calc > step) step = calc;
				} else {
					step += 1;
				}
			} else if (numToSend < 0 && line.startsWith("position")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				
				step = Integer.parseInt(st.nextToken());
			}
		}
	}
}