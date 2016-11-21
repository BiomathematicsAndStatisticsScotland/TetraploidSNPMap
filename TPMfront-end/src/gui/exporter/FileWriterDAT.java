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
import data.AlleleState;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;
import gui.AppFrame;

public class FileWriterDAT {
	// The file that will be written to
	private File file;

	/** FileWriterDAT() - i believe this is NONSNP only..
	 * 
	 */
	public FileWriterDAT(File file) {
		this.file = file;

		new File(file.getParent()).mkdir();
	}

	/** writes the lGroup to the file.
	 * 
	 */
	public boolean writeData(LinkageGroup lGroup, boolean checkedOnly) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));

			out.write("  " + (lGroup.getIndividualCount() - 2) + "  ");
			if (checkedOnly) {
				out.write("" + lGroup.getSelectedMarkerCount());
			} else {
				out.write("" + lGroup.getMarkerCount());
			}
			
			out.newLine();

			for (CMarker cm : lGroup.getMarkers()) {
				if (checkedOnly && cm.checked == false) {
					continue;
				}

				Marker m = cm.marker;

				out.write(cm.safeName + "  " + m.getAlleleCount());
				out.newLine();

				for (int a = 0; a < m.getAlleleCount(); a++) {
					if (AppFrame.tpmmode == AppFrame.TPMMODE_SNP) {
						//writeDosages(out, m.getAllele(a));
					} else {
						writeStates(out, m.getAllele(a));
					}

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

	// TV: Writes dosages from AlleleDosage in SNP data sets to cluster.dat file
	/*private void writeDosages(BufferedWriter out, Allele allele) throws IOException {
		int count = 0;
		for (AlleleDosage dosage : allele.getDosages()) {
			out.write(" " + dosage.getDosage());
			if (++count == 40) {
				out.newLine();
				count = 0;
			}
		}
	}*/

	private void writeStates(BufferedWriter out, Allele allele) throws IOException {
		int count = 0;
		for (AlleleState state : allele.getStates()) {
			out.write(" " + state.getState());
			if (++count == 40) {
				out.newLine();
				count = 0;
			}
		}
	}
}