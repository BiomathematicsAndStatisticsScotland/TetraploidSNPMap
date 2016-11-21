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
import javax.swing.JOptionPane;
import data.Allele;
import data.AlleleDosage;
import data.CreationException;
import data.LinkageGroup;
import data.Marker;
import doe.MsgBox;

public class ImportSNPFile {

	private LinkageGroup lGroup;
	private int iCount, mCount;
	private String lastMarker;

	/** imports the given SNPloc marker file.
	 * 
	 */
	public ImportSNPFile(File file) {
		lGroup = new LinkageGroup(file.getName());

		try {
			BufferedReader in = new BufferedReader(new FileReader(file));

			StringTokenizer st = new StringTokenizer(in.readLine());
			try {
				/*
				 * it will add +2 to include also the first two instances added
				 * to the population, that correspond to the parents.
				 */
				iCount = Integer.parseInt(st.nextToken()) + 2;
				mCount = Integer.parseInt(st.nextToken());
			} catch (Exception e) {
				in.close();
				throw new CreationException(CreationException.UNKNOWN_FILE);
			}

			// System.out.println("Reading " + iCount + " and " + mCount);

			for (int markers = 0; markers < mCount; markers++) {
				readSNPMarkerData(in);
			}


			in.close();
			lGroup.verify();
		} catch (Exception e) {
			String msg = "TetraploidMap was unable to open " + file 
					+ " due to " + "the following reason:\n" + e;
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

	private void readSNPMarkerData(BufferedReader in) throws CreationException, Exception {
		// Header data for this marker - its name and number of alleles
		String l = in.readLine();
		StringTokenizer st = new StringTokenizer(l);
		long c = st.countTokens();

		if (c < 3) {
			l = l + " " + in.readLine();
			st = new StringTokenizer(l);
			c = st.countTokens();
		}
		boolean containSCgroups;

		if (c == iCount + 1) {
			containSCgroups = false;
		} else if (c == iCount + 2) {
			containSCgroups = true;
		} else {
			JOptionPane.showMessageDialog(null, "Make sure that the format of the input file is correct.");
			throw new Exception(
					" The format of the imported file is not compatible with the running mode of "
					+ "TetraploidMap.\n If you think that the format of the file is correct, then "
					+ "try to open it while running TetraploidMap in a different mode.");
		}

		String name = st.nextToken();
		lastMarker = name;
		String scgroup = "";

		/*
		 * For consistency with the previous file format we have kept the notion
		 * of multiple alleles, while we know that for the new SNP marker data
		 * there will always be only 1 allele. However in order to distinct this
		 * from AFLP markers that also have 1 allele, we will note this to the
		 * Marker constructor with a minus one -1.
		 */
		// int alleleCount = Integer.parseInt(st.nextToken());
		int alleleCount = -1;

		// Add this marker to the dataset
		Marker marker = lGroup.getOrAddMarker(name, alleleCount);
		// System.out.println("importsnpfile.readSNPMarkerData after
		// getoraddmarker()");
		if (containSCgroups) {
			scgroup = st.nextToken();

			// System.out.println("ScGroup: '" + scgroup + "'");
			if (scgroup.contentEquals("Blank")) {
				// System.out.println("changing to ''");
				marker.setPrefix("");
			} else {
				marker.setPrefix(scgroup);
			}
		}

		// Set alleleCount to positive +1
		alleleCount = Math.abs(alleleCount);

		// Then get all the available input - this will give us an array
		// containing all the allele_1 data, then all the _2 data, etc
		// System.out.println("getting indiv. snp data");

		byte[] input = getIndividualSNPData(st, iCount * alleleCount);

		// Then, add data for each Allele based on this
		for (int a = 0; a < alleleCount; a++) {
			// Create the Allele...
			// Allele allele = new Allele(marker);
			Allele allele = new Allele();
			// ...and add it to the Marker
			marker.addAllele(a, allele);

			// Then give it the Individual/State information
			for (int i = 0; i < iCount; i++) {
				allele.addAlleleDosage(new AlleleDosage(input[(a * iCount) + i]));
			}

		}
	}

	// Reads input until the given number of individuals have been found
	private byte[] getIndividualSNPData(StringTokenizer st, int count) throws Exception {
		byte[] input = new byte[count];
		int found = 0;

		while (found < input.length) {
			// StringTokenizer st = new StringTokenizer(in.readLine());
			while (st.hasMoreElements()) {
				input[found++] = Byte.parseByte(st.nextToken());
			}
		}

		// TV: Converts data to |4-value| if (P1+P2)>4 and to |2-value| if P1:P2
		// is 0:3 or 3:0
		input = homogeniseData(input);
		return input;
	}

	// TV: homogenise data by converting them using the formula:
	// > if ((P1+P2)>4) then value = |4-value| unless (value==9)
	// This will invert A and B, but this is OK since it is the ratio that
	// matters rather than the absolute values as measured in the experiment.
	private byte[] homogeniseData(byte[] in) {
		// TV: check if A and B designation needs reversing
		if (in[0] + in[1] > 4) {
			for (int i = 0; i < in.length; i++) {
				if (!(in[i] == 9)) {
					in[i] = (byte) Math.abs(4 - in[i]);
				}
			}
		}

		if ((in[0] == 3 && in[1] == 0) || (in[0] == 0 && in[1] == 3)) {
			// System.out.println(in[0]+":"+in[1]);
			if (in[0] == 3) {
				in[0] = 1;
			} else if (in[1] == 3) {
				in[1] = 1;
			}
			for (int i = 2; i < in.length; i++) {
				if (!(in[i] == 9)) {
					in[i] = (byte) Math.abs(2 - in[i]);
				}
			}
		}

		return in;
	}

}