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

package analyses.cluster;

import java.io.File;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.InputStream;
import java.util.StringTokenizer;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import gui.Prefs;
import doe.MsgBox;

class RunCluster extends FortranRunner {
	// How many lines have been processed?
	int position = 0, maximum = 0;
	// What are the names of the two markers being processed
	// How many linkage groups should Cluster try to create
	private float numToSend;
	// Clustering method in use (1 or 2)
	private int method;
	
	RunCluster(float numToSend, int method) {
		this.numToSend = numToSend;
		this.method = method;
	}
	
	public void run() {
		try {	
			proc = Runtime.getRuntime().exec(Prefs.tools_cluster_path, null,
				new File(Prefs.tools_scratch));
			
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				proc.getOutputStream()));
			
			new ClusterCatcher(proc.getInputStream());
			new ClusterCatcher(proc.getErrorStream());
			
			writer.println("cluster");
			writer.println("" + method);
			writer.println("" + Math.abs(numToSend));
			writer.close();
			
			proc.waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("The clustering process has failed due to the following "
				+ "error:\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	class ClusterCatcher extends StreamCatcher {
		ClusterCatcher(InputStream in) {
			super(in);
		}
		
		protected void processLine(String line) {	
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);
			} else if (line.startsWith("maximum:")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				maximum = Integer.parseInt(st.nextToken());
			} else if (line.startsWith("position:")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				
				position = Integer.parseInt(st.nextToken());
			}
		}
	}
}