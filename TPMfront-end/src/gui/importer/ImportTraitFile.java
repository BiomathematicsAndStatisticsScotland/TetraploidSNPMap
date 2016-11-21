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

package gui.importer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;
import data.TraitFile;
import doe.MsgBox;

public class ImportTraitFile {
	private File file;
	private TraitFile tFile;

	public ImportTraitFile(File file) {
		this.file = file;
	}

	/** import the File.
	 * 
	 * <p>the result is retrieved by getTraitFile().
	 */
	public boolean doImport() {
		tFile = new TraitFile();

		try {
			BufferedReader in = new BufferedReader(new FileReader(file));

			// First line containing number of traits
			in.readLine();

			// Next line containing trait names
			StringTokenizer st = new StringTokenizer(in.readLine());
			while (st.hasMoreTokens()) {
				tFile.addName(st.nextToken());
			}
			String str = in.readLine();
			while (str != null && str.length() > 0) {
				float[] data = new float[tFile.getNames().size() + 1];
				Vector<String> datas = new Vector<String>();
				String l;

				st = new StringTokenizer(str);
				if (st.countTokens() != 0) {
					for (int i = 0; i < data.length; i++) {
						l = st.nextToken();
						datas.add(l);
						data[i] = Float.parseFloat(l);
					}
					tFile.addRow(data);
				}

				str = in.readLine();
			}

			in.close();
		} catch (Exception e) {
			e.printStackTrace(System.out);
			MsgBox.msg("TetraploidMap could not import any trait data from the "
					+ "selected file. Check to ensure the file format is correct.\n" + e, MsgBox.ERR);

			return false;
		}

		return true;
	}

	public TraitFile getTraitFile() {
		return tFile;
	}
}