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

package gui.exporter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import data.TraitFile;
import doe.MsgBox;
import gui.Prefs;

public class FileWriterQUA {
	private File file;

	public FileWriterQUA(File file) {
		this.file = file;
		new File(file.getParent()).mkdir();
	}

	/** write the traitFile data to the file.
	 * 
	 */
	public boolean writeData(TraitFile tFile, String tName) {
		if (tName != null) {
			tName = tName.trim();
		}

		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));

			// Number of markers
			if (tName == null) {
				out.write("" + tFile.getSelectedCount());
			} else {
				out.write("1");
			}
			out.newLine();

			// Marker names
			for (int i = 0; i < tFile.getNames().size(); i++) {
				if (tFile.getEnabled().get(i)) {
					if (tName == null || tName.equals(tFile.getNames().get(i))) {
						out.write("  " + tFile.getNames().get(i));
					}
				}
			}
						
			out.newLine();

			// Data
			for (float[] data : tFile.getRows()) {
				out.write("  " + ((int) data[0]));

				for (int i = 0; i < tFile.getNames().size(); i++) {
					if (tFile.getEnabled().get(i)) {
						if (tName == null || tName.equals(tFile.getNames().get(i))) {
							out.write("  " + Prefs.d3.format(data[i + 1]));
						}
					}
				}

				out.newLine();
			}

			out.close();
		} catch (IOException e) {
			MsgBox.msg("Unable to write " + file + " due to the following " + "error:\n" + e, MsgBox.ERR);
			return false;
		}

		return true;
	}
}