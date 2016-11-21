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

package analyses.anova;

import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import data.LinkageGroup;
import data.AnovaResult;
import data.AnovaTraitResult;
import data.Marker;
import doe.MsgBox;

class ProcessAnovaResults extends Thread {
	// Have any errors occurred?
	boolean error = false;
	
	private LinkageGroup lGroup;
	private AnovaResult results;
	private File file;
	
	ProcessAnovaResults(File f, AnovaResult r, LinkageGroup l) {
		file = f;
		results = r;
		lGroup = l;
		
		process();
	}
	
	public void process() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			processTraits(in);
			in.close();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("Anova was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
			
			e.printStackTrace(System.out);
		}
	}
	
	private void processTraits(BufferedReader in)
		throws Exception {
		String line = in.readLine();
		while (line != null) {
			if (line.startsWith(" trait =")) {
				line = processTrait(in, line);
			} else {				
				line = in.readLine();
			}
		}
	}
	
	private String processTrait(BufferedReader in, String line)
		throws Exception {
		String traitName = line.substring(9);
		AnovaTraitResult r = new AnovaTraitResult(traitName);
		results.addResult(r);
				
		line = in.readLine();
		line = in.readLine();
		while (line != null && line.length() > 0) {
			if (line.trim().length() == 0) {
				break;
			}
			
			String markerName = line.substring(0, 6);
			Marker m = lGroup.getMarkerBySafeName(markerName).marker;
			
			String data = line.substring(22);			
			r.addMarker(m, data);
			
			line = in.readLine();
		}
		return line;
	}
}
