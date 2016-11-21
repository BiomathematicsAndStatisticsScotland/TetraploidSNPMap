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
import data.Allele;
import data.AlleleDosage;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;

public class FileWriterSNPloc {
	private File file;
	public boolean checkedOnly = false;
	public boolean fixedOnly = false;
	public boolean safeNameNrs = false;

	public FileWriterSNPloc(File file) {
		this.file = file;
		new File(file.getParent()).mkdir();
	}

	/** write the linkagegroup to the SNPloc file.
	 * 
	 * <p>set the following options: .checkedOnly, .fixedOnly, .safeNameNrs.
	 * 
	 */
	public boolean writeData(LinkageGroup lGroup) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			out.write("  " + (lGroup.getIndividualCount() - 2) + "  ");
			if (fixedOnly) {
				out.write("" + lGroup.getFixedMarkerCount());
			} else if (checkedOnly) {
				out.write("" + lGroup.getSelectedMarkerCount());
			} else {
				out.write("" + lGroup.getMarkerCount());
			}
			out.newLine();
			for (CMarker cm : lGroup.getMarkers()) {
				if (fixedOnly && cm.marker.fix_changed == false) {
					continue;
				}

				if (checkedOnly && cm.checked == false) {
					continue;
				}
				Marker m = cm.marker;

				if (safeNameNrs) {
					out.write(Integer.parseInt(cm.safeName.substring(3)) + " ");
				} else {
					out.write(m.getName() + "  ");
				}

				for (int a = 0; a < m.getAlleleCount(); a++) {
					writeDosages(out, m.getAllele(a));
					out.newLine();
				}
			}

			out.close();
		} catch (IOException e) {
			MsgBox.msg("Unable to write " + file + " due to the following " + "error:\n" + e, MsgBox.ERR);
			return false;
		}

		return true;
	}

	private void writeDosages(BufferedWriter out, Allele allele) throws IOException {
		for (AlleleDosage dosage : allele.getDosages()) {
			out.write(" " + dosage.getDosage());
		}
	}

}
