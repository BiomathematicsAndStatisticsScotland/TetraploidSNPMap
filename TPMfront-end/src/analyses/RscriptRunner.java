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

package analyses;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.util.StringTokenizer;

import doe.MsgBox;
import gui.Prefs;

/** RScriptRunner is very similar to FortranRunner except it runs
 * an R script.
 *
 */
public abstract class RscriptRunner extends FortranRunner {
	protected Process proc = null;
	
	// Is the executable still running?
	public boolean isRunning = true;
	// Have any errors occurred?
	public boolean error = false;
	public String args, scriptname; 
	public int step = 0;

	public RscriptRunner() {
		this.args = "";
	}
	
	public  RscriptRunner(String args) {
		this.args = args;
		this.scriptname = "abstract.R";
	}
	
	/**
	 * the default run() method just runs the 'scriptname'.
	 */
	public void run() {
		try {
			proc = Runtime.getRuntime().exec(
					Prefs.tools_rscript + " --vanilla " + scriptname + "  " 
						+ args,
					Prefs.rlibs_env,
					new File(Prefs.tools_scratch));
			new RscriptCatcher(proc.getInputStream());
			RscriptCatcher rsc = new RscriptCatcher(proc.getErrorStream());
			proc.waitFor();
			if (!rsc.errormsg.isEmpty()) {
				error = true;
			}
		} catch (Exception e) {
			error = true;
			MsgBox.msg(getClass().getName()
				+ " was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		isRunning = false;
	}
	
	/**
	 * exit() destroys the proc if necessary.
	 */
	public void exit() {
		if (proc != null) {
			proc.destroy();
		}
	}

	class RscriptCatcher extends StreamCatcher {
		BufferedReader reader = null;
		
		public String errormsg = "";
	
		RscriptCatcher(InputStream in) {
			super(in);
		}
		
		protected void processLine(String line) {
			//System.out.println("RscriptCatcher: " + line);
			if (line.startsWith("step")) {
				
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				step = Integer.parseInt(st.nextToken());
				//MsgBox.msg("line read, step: " + step,  MsgBox.INF);
			} else {
				errormsg += line + "\n";
			}
		}
	}
}