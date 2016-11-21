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

package analyses.qtl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.QTLResult;
import data.Trait;
import doe.MsgBox;

class ProcessQTLResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;

	private QTLResult qtlResult;
	private File file;

	ProcessQTLResults(QTLResult qtlResult, File file, File bkFile) {
		this.qtlResult = qtlResult;
		this.file = file;
		start();
	}

	public void run() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));

			processTraits(in);
			in.close();

		} catch (Exception e) {
			e.printStackTrace(System.out);

			error = true;
			MsgBox.msg("QTL was unable to run due to the following error:" + "\n" + e, MsgBox.ERR);
		}

		isRunning = false;
	}

	private void processTraits(BufferedReader in) throws Exception {
		String line = in.readLine();
		while (line != null) {
			if (line.startsWith(" Profile for trait")) processTrait(in, line);
			line = in.readLine();
		}
	}

	private void processTrait(BufferedReader in, String line) throws Exception {
		Trait trait = new Trait();
		qtlResult.getTraits().add(trait);

		in.readLine(); // trait number
		trait.setName(in.readLine());

		// Read the position and variance (array) data for this trait
		line = in.readLine();
		while (line.startsWith(" Best position with iteration") == false) {
			StringTokenizer st = new StringTokenizer(line);
			float position = Float.parseFloat(st.nextToken());
			st.nextToken();
			float lod = Float.parseFloat(st.nextToken());

			trait.getPositions().add(position);
			trait.getLODs().add(lod);

			line = in.readLine();
		}

		line = in.readLine();

		// Position, variance, error mean square
		StringTokenizer st = new StringTokenizer(line);
		trait.qtlPosition = Float.parseFloat(st.nextToken());
		trait.varExplained = Float.parseFloat(st.nextToken());
		trait.errMS = Float.parseFloat(st.nextToken());
		trait.maxLOD = Float.parseFloat(st.nextToken());

		// Best position with iteration data (6 lines)
		for (int i = 0; i < 6; i++) {
			trait.qtlEffects[i] = in.readLine();
		}

		line = in.readLine();
		while (line.startsWith("Model") == false) {
			line = in.readLine();
		}

		// Models
		for (int i = 0; i < 10; i++) {
			line = in.readLine();

			st = new StringTokenizer(line);
			st.nextToken();
			for (int j = 0; j < 6; j++) {
				trait.modelScores[i][j] = Float.parseFloat(st.nextToken());
			}
			// Read the extra column that was being ignored before
			trait.modelScoresExtra[i][0] = Float.parseFloat(st.nextToken());
			line = in.readLine();
		}

		/*
		 * // Final model summary scores (last 2 locations of array) for (int i
		 * = 0; i < 10; i++) { st = new StringTokenizer(line); st.nextToken();
		 * 
		 * trait.modelScores[i][6] = Float.parseFloat(st.nextToken());
		 * trait.modelScores[i][7] = Float.parseFloat(st.nextToken());
		 * 
		 * line = in.readLine(); }
		 */
		/*
		 * trait.mean = Float.parseFloat(st.nextToken()); trait.mean_se =
		 * Float.parseFloat(st.nextToken());
		 * 
		 * // QTL effects for (int i = 0; i < 3; i++) { line = in.readLine(); st
		 * = new StringTokenizer(line); trait.ch_e[i] =
		 * Float.parseFloat(st.nextToken()); trait.ch_se[i] =
		 * Float.parseFloat(st.nextToken()); }
		 */
	}
}