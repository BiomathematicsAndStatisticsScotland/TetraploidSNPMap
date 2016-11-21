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
import java.util.Vector;
import data.CMarker;
import data.OrderedResult;
import doe.MsgBox;
import gui.Prefs;

public class FileWriterMAP {
	private File file;

	/** FileWriterMap() - NONSNP only.
	 * 
	 */
	public FileWriterMAP(File file) {
		this.file = file;

		new File(file.getParent()).mkdir();
	}

	/** writes the map, given in OrderedResult and rows.
	 * 
	 */
	public boolean writeData(OrderedResult order, Vector<String[]> rows) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			out.write("" + order.getLinkageGroup().getMarkerCount());
			out.newLine();

			Vector<Float> distances = order.getDistances();
			float cmValue = 0;
			int i = 0;

			for (CMarker cm : order.getLinkageGroup().getMarkers()) {
				// Calculations...
				if (i > 0) {
					cmValue += distances.get(i - 1);
				}
				out.write(getName20(cm.safeName));
				String s1 = Prefs.d1.format(cmValue);
				String s2 = s1;
				for (int j = s1.length(); j < 6; j++) {
					s2 = " " + s2;
				}
				out.write(s2 + "     ");

				// Parent geno 2
				String[] data = rows.get(i++);
				out.write(data[0] + "  ");
				out.write(data[1]);

				out.newLine();
			}

			out.close();
		} catch (IOException e) {
			MsgBox.msg("Unable to write " + file + " due to the following " + "error:\n" + e, MsgBox.ERR);
			return false;
		}

		return true;
	}

	private String getName20(String name) {
		String str = name;
		int ml = Prefs.nonsnpmarkername_maxlen;

		if (str.length() == ml) {
			return str;
		} else if (str.length() > ml) {
			return str.substring(0, ml);
		} else {
			for (int i = str.length(); i < ml; i++) {
				str += " ";
			}
			return str;
		}
	}
}
