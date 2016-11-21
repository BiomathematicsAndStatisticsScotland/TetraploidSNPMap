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

package analyses.findgeno;

import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.LinkageGroup;
import data.Marker;
import data.CMarker;
import data.MarkerBandPattern;
import data.MarkerProbability;
import doe.MsgBox;

class ProcessResults extends Thread {
	// How many lines have been processed?
	int locusCount = 0;
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;
	
	private LinkageGroup lGroup;
	private File file;
	
	ProcessResults(LinkageGroup lGroup, File file) {
		this.lGroup = lGroup;
		this.file = file;
		
		start();
	}
	
	public void run() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			processParentalPhenotypes(in);
			processLocusData(in);
			in.close();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("FindGeno was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	private void processParentalPhenotypes(BufferedReader in)
		throws Exception {
		// Header information - discard
		in.readLine();
		in.readLine();
		
		String line = in.readLine();
		while (line.length() > 0) {
			StringTokenizer st = new StringTokenizer(line);
			
			// Marker number
			st.nextToken();
			
			// Marker name
			Marker marker = lGroup.getMarkerBySafeName(st.nextToken()).marker;
			
			// Phenotype information
			marker.setPhenotypeInfo(st.nextToken(), st.nextToken());
			
			line = in.readLine();
		}
	}
	
	private void processLocusData(BufferedReader in)
		throws Exception {
		String line = in.readLine();
		while (line != null) {
			if (line.startsWith("Locus")) {
				StringTokenizer st = new StringTokenizer(line);
			
				st.nextToken();
				st.nextToken();
				CMarker cm = lGroup.getMarkerBySafeName(st.nextToken());
				Marker marker = cm.marker;
				
				processLocus(in, cm, marker);
			}
			
			line = in.readLine();
		}
	}
	
	private void processLocus(BufferedReader in, CMarker cm, Marker marker)
		throws Exception {
		in.readLine();
		in.readLine();
		in.readLine();
		
		String line = in.readLine();
		
		// Read band patterns
		while (line.length() > 0) {
			StringTokenizer st = new StringTokenizer(line.substring(18));
			
			String band = line.substring(2, 18);
			int count = Integer.parseInt(st.nextToken());
			float proportion = Float.parseFloat(st.nextToken());
			
			marker.addMarkerBandPattern(
				new MarkerBandPattern(band, count, proportion));

			line = in.readLine();
		}

		line = in.readLine();
		line = in.readLine();
		
		// Double reduction tests
		StringTokenizer st = new StringTokenizer(line.substring(26));
		marker.setDRTestValues(
			Float.parseFloat(st.nextToken()), Float.parseFloat(st.nextToken()));
					
		// Posterior propabilities
		processProbs(in, marker, true);
		processProbs(in, marker, false);
		
		// Can the marker's best ratio be determined?
		cm.checked = marker.determineRatio();
	}
	
	private void processProbs(BufferedReader in, Marker marker, boolean dblR)
		throws Exception {
		String line = in.readLine();
		while (line.startsWith("   ") == false) {
			if (line.equals(" No configuration found!")) {
				return;
			}
			line = in.readLine();
		}
		
		while (line.startsWith("   ") == true) {
			StringTokenizer st = new StringTokenizer(line);
			
			String p1 = st.nextToken();
			String p2 = st.nextToken();
			float  pb = Float.parseFloat(st.nextToken());
			float  df = Float.parseFloat(st.nextToken());
			float chi = Float.parseFloat(st.nextToken());
			
			MarkerProbability mp = new MarkerProbability(p1, p2, pb, df, chi);
			marker.addMarkerProbability(mp, dblR);
			
			line = in.readLine();
		}
	}
}