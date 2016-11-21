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

package analyses.findgeno;

import java.io.File;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.InputStream;
import java.io.BufferedReader;
import java.util.StringTokenizer;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import gui.Prefs;
import doe.MsgBox;

class RunFindGeno extends FortranRunner {
	// How many lines have been processed?
	int locusCount = 0;
	// What locus is being read
	String marker = "";
	
	public void run() {
		try {	
			proc = Runtime.getRuntime().exec(Prefs.tools_findgeno_path, null,
				new File(Prefs.tools_scratch));
			
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				proc.getOutputStream()));
			
			new FindGenoCatcher(proc.getInputStream());
			new FindGenoCatcher(proc.getErrorStream());
			
			writer.println("findgeno");
			writer.println("y");
			writer.println("y");
			writer.close();
			
			proc.waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("FindGeno was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	class FindGenoCatcher extends StreamCatcher {
		BufferedReader reader = null;
	
		FindGenoCatcher(InputStream in) {
			super(in);
		}
		
		protected void processLine(String line) {
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);						
			} else if (line.startsWith("Locus")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				
				locusCount = Integer.parseInt(st.nextToken());
				marker = st.nextToken();
			}
		}
	}
}