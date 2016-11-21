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
import java.text.DecimalFormat;
import java.util.Vector;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;
import data.OrderedResult;
import doe.MsgBox;

public class FileWriterPhase {

	private File file;

	/** FileWriterPhase().
	 * 
	 */
	public FileWriterPhase(File file) {
		this.file = file;

		new File(file.getParent()).mkdir();
	}

	/** writes the given OrderedResult (or) to File.
	 * 
	 */
	public boolean writeData(OrderedResult or) {
		String ds;
		DecimalFormat d = new DecimalFormat("0.00");
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			LinkageGroup lGroup = or.getLinkageGroup();

			out.write("" + lGroup.getSelectedMarkerCount());
			out.newLine();
			Vector<Float> distances = or.getDistances();
			int i;
			float dist = 0;
			Vector<CMarker> ms = lGroup.getMarkers();
			for (int j = 0; j < ms.size(); j++) {
				if (or.flip) {
					i = ms.size() - j - 1;
				} else {
					i = j;
				}
				if (!or.flip) {
					dist += distances.get(i);
				}
				Marker m = ms.elementAt(i).marker;
				ds = d.format(dist);

				out.write(m.getName());
				for (int n = m.getName().length() + ds.length(); n < 33; n++) {
					out.write(" ");
				}
				out.write(ds);
				out.write("  ");
				out.write(m.getPhenotypeInfo(1));

				out.write(" ");
				out.write(m.getPhenotypeInfo(2));

				out.write("    ");
				out.write(m.getParentDosage(1));

				out.write("    ");
				out.write(m.getParentDosage(2));

				out.newLine();
				if (or.flip) {
					dist += distances.get(i);
				}
			}

			out.close();
		} catch (IOException e) {
			MsgBox.msg("Unable to write " + file + " due to the following " + "error:\n" + e, MsgBox.ERR);
			return false;
		}

		return true;
	}
}
