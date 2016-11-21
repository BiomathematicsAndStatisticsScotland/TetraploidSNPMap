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

package analyses.perm;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.StringTokenizer;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import doe.MsgBox;
import gui.Prefs;

class RunPerm extends FortranRunner {
	boolean fullModel;
	int position;

	public void run() {
		try {
			if (fullModel) {
				proc = Runtime.getRuntime().exec(Prefs.tools_perm_path, 
						null, new File(Prefs.tools_scratch));
			} else {
				proc = Runtime.getRuntime().exec(Prefs.tools_permsimp_path, 
						null, new File(Prefs.tools_scratch));
			}

			PrintWriter writer = new PrintWriter(new OutputStreamWriter(proc.getOutputStream()));

			new PermCatcher(proc.getInputStream());
			new PermCatcher(proc.getErrorStream());

			writer.println("perm");
			if (fullModel) writer.println("0");
			writer.close();

			proc.waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("Perm was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	class PermCatcher extends StreamCatcher {
		BufferedReader reader = null;

		PermCatcher(InputStream in) {
			super(in);
		}

		protected void processLine(String line) {
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);
			} else if (line.startsWith(" CL")) {
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();

				position = (int) Float.parseFloat(st.nextToken());
			}
		}
	}
}