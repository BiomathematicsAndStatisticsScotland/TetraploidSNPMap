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

package analyses.findsnpgeno;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import java.util.Vector;
import gui.AppFrame;
import data.CMarker;
import data.LinkageGroup;
import data.Marker;
//import data.MarkerBandPattern;
//import data.MarkerProbability;
import data.MarkerSNPPattern;
import data.SigLinkage;
import doe.MsgBox;

public class ProcessSNPResults extends Thread {
	// How many lines have been processed?
	int locusCount = 0;
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;
	//private int num_np = 0, num_dr = 0;
	
	private LinkageGroup lGroup;
	private File file;
	//private AppFrame appFrame;
	private String errors = "";
	public int parsestep = 0;
	public String parsestatus = "";
	
	
	ProcessSNPResults(AppFrame appFrame, LinkageGroup lGroup, File file) {
		this.lGroup = lGroup;
		this.file = file;
		start();
	}
	
	/**
	 * This processes the output from SNPMatch/ReadingTPM/FindSNPGeno.
	 */
	public void run() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			processSNPData(in);
			in.close();
			//System.out.println(errors);
			if (!errors.isEmpty()) MsgBox.msg(errors, MsgBox.ERR);
		} catch (Exception e) {
			error = true;
			e.printStackTrace(System.out);
			System.out.println("Exception cought in: ProcessSNPResults.run() :: " + e);
			MsgBox.msg("error reading result file in ProcessSNPResult.run().", MsgBox.ERR);
		}
		
		isRunning = false;
	}
	
	private void processSNPData(BufferedReader in) throws Exception {
		String line = in.readLine();
		int dataCount = 0;	
		while (line != null) {
			line = in.readLine();
			if (line == null) return;
			if (line.startsWith("Count")) {
				this.parsestep = 1;
				this.parsestatus = "Reading observed frequencies..";
				this.processSNPMarkerData(in);
			} 
			if (line.startsWith(" Q0")) {
				errors += line + "\n";
			}
			//Parse Renamed loci 
			if (line.startsWith(" Renamed loci")) {
				this.parsestep = 2;
				this.parsestatus = "Reading renamed loci..";
				this.processSNPRenamedLoci(in);	
			}
			//Parse Linkages for Parent 1
			if (line.startsWith(" Info for parent 1 tab")) {
				//int dataCount = 0;	
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Simplex-duplex linkages for parent 1")) {
					this.parsestep = 3;
					this.parsestatus = "Reading P1 Simplex-duplex linkages";
					processSNPLinkages(++dataCount, in);			
				}
				// Identify second block
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Simplex-double simplex linkages for parent 1")) {
					this.parsestep = 4;
					this.parsestatus = "Reading P1 Simplex-double linkages";
					processSNPLinkages(++dataCount, in);	
				}
				// Identify third block
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Other linkages to simplex markers for parent 1")) {
					this.parsestep = 4;
					this.parsestatus = "Reading P1 other linkages";
					processSNPLinkages(++dataCount, in);
				}
			} // ENDOF " Info for parent 1 tab"
			//Parse Linkages for Parent 2
			if (line.startsWith(" Info for parent 2 tab")) {
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Simplex-duplex linkages for parent 2")) {
					this.parsestep = 5;
					this.parsestatus = "Reading P2 Simplex-duplex linkages";
					processSNPLinkages(++dataCount, in);			
				}
				// Identify second block
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Simplex-double simplex linkages for parent 2")) {
					this.parsestep = 6;
					this.parsestatus = "Reading P2 Simplex-double linkages";
					processSNPLinkages(++dataCount, in);			
				}
				line = in.readLine();
				if (line == null) return;
				if (line.startsWith(" Other linkages to simplex markers for parent 2")) {
					this.parsestep = 7;
					this.parsestatus = "Reading P2 other linkages";
					//++dataCount;
					processSNPLinkages(++dataCount, in);
				}
			} // ENDOF " Info for parent 2 tab"
		} //END OF WHILE LOOP
	}
	
	
	private void processSNPMarkerData(BufferedReader in) throws Exception {
		String iname;
		MarkerSNPPattern spat;
		String line = in.readLine();
		StringTokenizer st = new StringTokenizer(line);
	
		st.nextToken();
		iname = st.nextToken();

		CMarker cm = lGroup.getMarkerBySafeNameNr(Integer.parseInt(iname));
 		Marker marker = cm.marker;
		
		// P1 P2 dosages
		marker.setParentDosage(st.nextToken(), st.nextToken());
		
		// nmiss
		marker.setNMiss(st.nextToken());
		
		line = in.readLine();
		st = new StringTokenizer(line);
		// status OK/DR/NP
		String status = st.nextToken();
		marker.setStatus(status);
		
		line = in.readLine();
		
		// Observed frequencies
		if (line.startsWith(" Observed frequencies")) {
			line = in.readLine();
			while (!line.startsWith("Chi")) {
				st = new StringTokenizer(line);
				spat = new MarkerSNPPattern(st.nextToken(), st.nextToken(), st.nextToken());
				//System.out.println(spat.getDosage() + spat.getCount() + spat.getProportion());
				marker.addMarkerSNPPatern(spat);											
				line = in.readLine();
			}
		}	
		if (line.startsWith("Chi")) {
			st = new StringTokenizer(line);
			st.nextToken();
			st.nextToken();
			st.nextToken();
			
			String wrd = st.nextToken();
			if (wrd.contains("-")) wrd = "0.0";
			marker.setChiSig(Double.parseDouble(wrd));
			
			st.nextToken();
			st.nextToken();
			marker.setAlpha(Float.parseFloat(st.nextToken()));
		}
		// By default select marker when parsed if Ratio_Sig > 0.001
		cm.checked = marker.canEnable();
	}
	
	private void processSNPRenamedLoci(BufferedReader in) throws Exception {
		String line;
		StringTokenizer st;
		String iname;
		
		// skip first line
		line = in.readLine();
		while (!line.equals(" ")) {
			st = new StringTokenizer(line);
			iname = st.nextToken();
			//CMarker cm = lGroup.getMarkerByName(iname);
			CMarker cm = lGroup.getMarkerBySafeNameNr(Integer.parseInt(iname));
			Marker marker = cm.marker;
			String prefix = st.nextToken();//.substring(0, 5);
			if (prefix.contains("Blank")) {
				prefix = "";
				marker.setRatioCode(7);
				marker.setSNPRatio("higher");
			} else {
				marker.setSNPRatio("1:1");
				marker.setRatioCode(1);
			}
			//System.out.println("marker " + marker + " prefix was " + marker.getPrefix());
			marker.setPrefixIfunset(prefix);
			line = in.readLine();
		}
	}
	
	private void processSNPLinkages(int dataCount, BufferedReader in) throws Exception {
		String line;
		if (in.readLine() == null) return;
		
		line = in.readLine();
				
		while (line != null && !line.equals(" ")) {
			StringTokenizer st = new StringTokenizer(line);
			Marker m1 = lGroup.getMarkerBySafeNameNr(
					Integer.parseInt(st.nextToken())).marker;
			Marker m2 = lGroup.getMarkerBySafeNameNr(
					Integer.parseInt(st.nextToken())).marker;
			
			float chi = Float.parseFloat(st.nextToken());
			float sig = Float.parseFloat(st.nextToken());
			String phase = null;
			if (st.hasMoreTokens()) phase = st.nextToken();
			
			// Add the data for marker 1
			SigLinkage s1 = new SigLinkage(m2, chi , sig, phase);
			m1.getSimMatchData().add(this.getSNPSigLinkage(m1, dataCount), s1);
			
			// Add the data for marker 2
			SigLinkage s2 = new SigLinkage(m1, chi, sig, phase);
			m2.getSimMatchData().add(this.getSNPSigLinkage(m2, dataCount), s2);
			
			line = in.readLine();
		}					
	}
	
	/*
	 * TV:  Modified to map the SimMatchData with the 6 blocks as appeared in ReadingTPM.out
	 */
	private Vector<SigLinkage> getSNPSigLinkage(Marker marker, int dataCount) {
		switch (dataCount) {
		    case 1: return marker.getSimMatchData().p1A;
		    case 2: return marker.getSimMatchData().p1B;
			case 3: return marker.getSimMatchData().p1C;			
			case 4: return marker.getSimMatchData().p2A;
			case 5: return marker.getSimMatchData().p2B;
			case 6: return marker.getSimMatchData().p2C;
			default: MsgBox.msg("found a 7th SNP linkage block in results file.", MsgBox.ERR);
		}
		
		return null;
	}
	
}
