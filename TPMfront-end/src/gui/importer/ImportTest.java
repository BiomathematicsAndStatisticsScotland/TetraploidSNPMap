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
import data.Allele;
import data.AlleleState;
import data.CreationException;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;

public class ImportTest {
	private LinkageGroup lGroup;
	private int iCount, mCount;
	private String lastMarker;

	/** ImportTest(File file) reads the given .loc marker file.
	 * 
	 */
	public ImportTest(File file) {
		lGroup = new LinkageGroup(file.getName());

		try {
			BufferedReader in = new BufferedReader(new FileReader(file));

			StringTokenizer st = new StringTokenizer(in.readLine());
			try {
				iCount = Integer.parseInt(st.nextToken()) + 2;
				mCount = Integer.parseInt(st.nextToken());
			} catch (Exception e) {
				in.close();
				throw new CreationException(CreationException.UNKNOWN_FILE);
			}


			for (int markers = 0; markers < mCount; markers++) {
				readMarkerData(in);
			}


			in.close();
			lGroup.verify();
		} catch (Exception e) {
			String msg = "TetraploidMap was unable to open " + file 
					+ " due to " + "the following reason:\n" + e
					+ ". Make sure that the format of the input file is correct.";
			MsgBox.msg(msg, MsgBox.ERR);
			if (lastMarker != null) {
				MsgBox.msg("The error occurred after marker: " + lastMarker, MsgBox.ERR);
			}
			lGroup = null;
		}
	}

	public LinkageGroup getLinkageGroup() {
		return lGroup;
	}

	private void readMarkerData(BufferedReader in) throws CreationException, Exception {
		// Header data for this marker - its name and number of alleles
		StringTokenizer st = new StringTokenizer(in.readLine());
		String name = st.nextToken();
		lastMarker = name;
		int alleleCount = Integer.parseInt(st.nextToken());

		// Add this marker to the dataset
		Marker marker = lGroup.getOrAddMarker(name, alleleCount);

		// Then get all the available input - this will give us an array
		// containing all the allele_1 data, then all the _2 data, etc
		byte[] input = getIndividualData(in, iCount * alleleCount);

		// Then, add data for each Allele based on this
		for (int a = 0; a < alleleCount; a++) {
			// Create the Allele...
			// Allele allele = new Allele(marker);
			Allele allele = new Allele();
			// ...and add it to the Marker
			marker.addAllele(a, allele);

			// Then give it the Individual/State information
			for (int i = 0; i < iCount; i++) {
				allele.addAlleleState(new AlleleState(input[(a * iCount) + i]));
			}
		}
	}

	// Reads input until the given number of individuals have been found
	private byte[] getIndividualData(BufferedReader in, int count) throws Exception {
		byte[] input = new byte[count];
		int found = 0;

		while (found < input.length) {
			StringTokenizer st = new StringTokenizer(in.readLine());
			while (st.hasMoreElements()) {
				input[found++] = Byte.parseByte(st.nextToken());
			}
		}

		return input;
	}
}