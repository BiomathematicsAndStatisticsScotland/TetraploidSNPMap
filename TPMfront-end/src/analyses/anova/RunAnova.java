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

package analyses.anova;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.File;
import java.io.OutputStreamWriter;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import gui.Prefs;
import doe.MsgBox;

class RunAnova extends FortranRunner {
	public void run() {
		try {
			proc = Runtime.getRuntime().exec(Prefs.tools_anova_path, null,
				new File(Prefs.tools_scratch));
			
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
				proc.getOutputStream()));
			
			new AnovaCatcher(proc.getInputStream());
			new AnovaCatcher(proc.getErrorStream());
			
			writer.println("anova");
			writer.close();
			
			proc.waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("Anova was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	class AnovaCatcher extends StreamCatcher {
		BufferedReader reader = null;
	
		AnovaCatcher(InputStream in) {
			super(in);
		}
		
		protected void processLine(String line) {
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);
			}
		}
	}
}