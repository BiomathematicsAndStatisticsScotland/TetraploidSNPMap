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


package analyses.snpqtlsimple;

import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import data.Trait;
import doe.MsgBox;

class ProcessSnpQtlSimpleResults extends Thread {
	private Trait trait;
	private File file;
	
	ProcessSnpQtlSimpleResults(Trait trait , File file) {
		this.trait = trait;
		this.file = file;
	}

	private void read_for_display(BufferedReader in, String str) {
		try {
			in.readLine(); // skip the header (Model Adj R^2 SIC)
			while ((str = in.readLine()) != null) {
				if (!str.trim().isEmpty()) {
					trait.addBestSimpleModel(str);
				}
			}
		} catch (Exception e) {
			MsgBox.msg("ProcessSnpQtl_Simple was unable to run due to the following error:"
					+ "\n" + e, MsgBox.ERR);
			e.printStackTrace(System.out);
		}
	}

	private void read_coefficient(BufferedReader in, String str) {
		String modelID = str.substring(20).trim();
		String modelinfo = "";
		try {
			while (true) {
				String l = in.readLine();
				if (l.isEmpty()) break;
				if (l.trim().isEmpty()) break;
				modelinfo += l + "\n";
			}
		} catch (Exception e) {
			System.out.println("error reading smpqmm output..");
		}
		trait.setsimple(modelID, modelinfo);
	}
	
	public boolean process() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			String str = null;
			while ((str = in.readLine()) != null) {
				if (str.startsWith("Model coefficients")) {
					read_coefficient(in, str);
				}
				if (str.startsWith(" For display")) {
					read_for_display(in, str.substring(14));
					return true;
				}
			}
			return false;
		} catch (Exception e) {
			MsgBox.msg("ProcessSnpQtl_Simple was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
			e.printStackTrace(System.out);
			return false;
		}
	}
}
