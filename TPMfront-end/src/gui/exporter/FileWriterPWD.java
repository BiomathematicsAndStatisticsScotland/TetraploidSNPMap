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
import java.util.StringTokenizer;
import data.CMarker;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;
import gui.Prefs;

// Writes a file in the format expected as input to SIMANNEAL.EXE
public class FileWriterPWD {
	// The file that will be written to
	private File file;

	public FileWriterPWD(File file) {
		this.file = file;
		new File(file.getParent()).mkdir();
	}

	/** NONSNP only..
	 * 
	 */
	public boolean writeData(OrderedResult result, LinkageGroup lGroup) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));

			// Header line
			int mCount = lGroup.getMarkerCount();
			out.write(mCount + " " + (int) (mCount * (mCount - 1) / 2));
			out.newLine();

			// The results from TwoPoint
			StringTokenizer st1 = new StringTokenizer(result.tp1.toString(), "\n");
			st1.nextToken();
			while (st1.hasMoreTokens()) {
				StringTokenizer st2 = new StringTokenizer(st1.nextToken());

				out.write(st2.nextToken());
				out.write("\t");
				out.write(st2.nextToken());
				out.write("\t");

				float rfq = Float.parseFloat(st2.nextToken());
				if (rfq >= 0.5f) {
					out.write("0.4999");
				} else {
					out.write(Prefs.d4.format(rfq));
				}

				out.write("\t");
				out.write(st2.nextToken());

				// out.write((String)st.nextToken());
				out.newLine();
			}

			// The (random) initial marker order
			for (CMarker cm : lGroup.getMarkers()) {
				out.write(" " + cm.safeName);
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