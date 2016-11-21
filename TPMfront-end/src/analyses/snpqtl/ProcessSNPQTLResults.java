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

package analyses.snpqtl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.QTLResult;
import data.Trait;
import doe.MsgBox;


class ProcessSNPQTLResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;
	
	private QTLResult qtlResult;
	private File file;
	
	ProcessSNPQTLResults(QTLResult qtlResult, File file) {
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
			MsgBox.msg("QTL was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	private void processTraits(BufferedReader in)
		throws Exception {
		String line = in.readLine();
		while (line != null) {
			if (line.startsWith(" Position, coefficients, lod")) {
				processTopTable(in, line);
			}
			if (line.startsWith(" Profile for trait")) {
				processTrait(in, line);
			}
			line = in.readLine();
		}
	}
	
	private void processTopTable(BufferedReader in, String line)
			throws Exception {
		in.readLine(); // ' ation'
		in.readLine(); // column names
		line = in.readLine();
		while (line != null && line.length() > 2) {
			Trait trait = new Trait();
			qtlResult.getTraits().add(trait);
			StringTokenizer st = new StringTokenizer(line);
			st.nextToken();
			trait.setName(st.nextToken());
			// Position, variance, error mean square
			trait.qtlPosition = Float.parseFloat(st.nextToken());
			for (int i = 0; i < 7; i++) {
				trait.qtlEffects[i] = st.nextToken();
			}
			trait.varExplained = Float.parseFloat(st.nextToken());
			trait.maxLOD = Float.parseFloat(st.nextToken());
			trait.errMS = Float.parseFloat(st.nextToken());
			for (int i = 0; i < 7; i++) {
				trait.seEffects[i] = st.nextToken();
			}
			
			//trait.errMS = Float.parseFloat(st.nextToken());
			line = in.readLine();
		}
	}
	
	private void processTrait(BufferedReader in, String line)
		throws Exception {
		Integer i = Integer.valueOf(new StringTokenizer(in.readLine()).nextToken());
		Trait trait = qtlResult.getTraits().get(i - 1);
		
		in.readLine();	// trait name
		// Read the position and variance (array) data for this trait
		line = in.readLine();
		while (line != null && line.length() > 2) {
			StringTokenizer st = new StringTokenizer(line);			
			float position = Float.parseFloat(st.nextToken());
			float lod = Float.parseFloat(st.nextToken());
			
			trait.getPositions().add(position);
			trait.getLODs().add(lod);
			line = in.readLine();
		}
	}
}