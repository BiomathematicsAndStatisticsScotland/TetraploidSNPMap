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

package data;

import java.io.Serializable;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.util.Vector;
import javax.swing.ImageIcon;
import java.util.StringTokenizer;
import java.io.BufferedWriter;
import java.io.FileWriter;
import gui.Prefs;

public class OrderedResult implements Serializable {
	static final long serialVersionUID = 481001104925097199L;
	public double meannnfit;
	private Summary summary;
	private String name;
	private boolean simOK;
	public boolean is3d = false;
	
	// A linkage group that stores the final ordered list of markers
	private LinkageGroup lGroup = new LinkageGroup(null);
	// and the intermarker distances
	private Vector<Float> distances = new Vector<Float>();
	
	private PhasePair[][] ppData;

	public int[] translateIndexes;
	public int[] translatePPIndexes;
	public int[] translatePP2Indexes;
	public Vector<String[]> rows;
	private Vector<RemovedLocus> removedLoci = new Vector<RemovedLocus>();
	
	private Vector<QTLResult> qtlResults;
	private int qtlCount = 0;
	private Vector<LinkageMapGraph> maps;
	private int mapCount = 0;
	private int phaseCount = 0;
	private Vector<OrderedResult> phaseResults;
	public final static int ORT_TWOPOINT = 1;
	public final static int ORT_MDS = 2;
	public final static int ORT_NONSNP = 3;
	public final static int ORT_READQTL = 4;
	public final static int ORT_PHASE = 5;
	public int orderedresulttype;
	private boolean excludeDuplicates = false;
	private boolean fullOutput = false;
	public boolean flip = false;
	public ImageIcon MDSimage;
	

	public OrderedResult(int mkrCount) {
		ppData = new PhasePair[mkrCount][mkrCount];
		orderedresulttype = ORT_NONSNP;
	}

	public OrderedResult(int mkrCount, int orttype) {
		ppData = new PhasePair[mkrCount][mkrCount];
		orderedresulttype = orttype;
	}

	public OrderedResult(LinkageGroup lg) {
		lGroup = lg;
		int smc = lg.getSelectedMarkerCount();
		ppData = new PhasePair[smc][smc];
		orderedresulttype = ORT_NONSNP;
	}

	public OrderedResult(LinkageGroup lg, int orttype) {
		lGroup = lg;
		int smc = lg.getSelectedMarkerCount();
		ppData = new PhasePair[smc][smc];
		orderedresulttype = orttype;
	}

	public void reorderpheno(int p, String neworder) {
		//int i = 0;
		for(CMarker cm: lGroup.getMarkers()) {
			//i++;
			cm.marker.reorderPhenoType(p, neworder);
		}
	}

	public void freezeSafeNames() {
		lGroup.freezeSafeNames();
	}
	public void removeLocus(String removedLocus, String realremovedLocus, String similarLocus, String realretainedLocus, String similarity)
	{
		CMarker cm;
		try {
			cm = lGroup.getMarkerBySafeNameNr(Integer.parseInt(removedLocus));
		} catch (Exception e) {
			e.printStackTrace(System.out);
			//MsgBox.msg("Failed to find one of the removed loci (" + removedLocus + ")", MsgBox.ERR);
			return;
		}
		lGroup.removeMarker(cm);
		removedLoci.add(new RemovedLocus(realremovedLocus,  realretainedLocus, similarity));
	}
	public boolean is_twopoint()
	{
		return( orderedresulttype == ORT_TWOPOINT);
	}
	public boolean is_mds()
	{
		return( orderedresulttype == ORT_MDS);
	}
	public boolean is_nonsnp()
	{
		return( orderedresulttype == ORT_NONSNP);
	}
	public boolean is_readqtl()
	{
		return (orderedresulttype == ORT_READQTL);
	}
	public boolean is_phase()
	{
		return (orderedresulttype == ORT_PHASE);
	}
	
	
	
	// Resets the ordering - used when simanneal is run *after* a custom order
	// has been created. The current order is passed to simanneal as its initial
	// order, but it must then be blanked, as simanneal will create a new result
	public void reset()
	{
		lGroup = new LinkageGroup(null);
		distances = new Vector<Float>();
	}
	public boolean getExcludeDuplicates()
	{
		return this.excludeDuplicates;
	}
	public boolean getFullOutput()
	{
		return this.fullOutput;
	}
	public String getExcludedDuplicateString()
	{
		String str = "Excluded Marker                  ";
		str +=       "Retained Marker                  ";
		str += "No. mismatches\n";
		if(this.removedLoci.isEmpty())
		{
			return "";
		}
		for(RemovedLocus rl : removedLoci)
		{
			str += rl + "\n";
		}
		return str;
	}
	public void setExcludeDuplicates(boolean b)
	{
		this.excludeDuplicates = b;
	}
	public void setFullOutput(boolean b)
	{
		this.fullOutput = b;
	}
	public int getNumExcluded()
	{
		return this.removedLoci.size();
	}
	public void setSummary(Summary summary)
		{ this.summary = summary; }
	
	public Summary getSummary()
		{ return summary; }
	
	public void setSimOK()
		{ simOK = true; }
	
	public boolean isSimOK()
		{ return simOK; }
	
	public Vector<QTLResult> getQTLResults()
		{ return qtlResults; }
	public Vector<OrderedResult> getPhaseResults()
	{
		return phaseResults;
	}
	public Vector<LinkageMapGraph> getLinkageMaps()
		{ return maps; }
	
	public void addQTLResult(QTLResult newResult)
	{
		if (qtlResults == null)
			qtlResults = new Vector<QTLResult>();
		
		newResult.setName("QTL Analysis " + (++qtlCount));
		qtlResults.add(newResult);
	}
	public void addPhaseResult(OrderedResult newResult)  
	{
		if (phaseResults == null)
			phaseResults = new Vector<OrderedResult>();
		
		newResult.setName("Phase Analysis " + (++phaseCount));
		phaseResults.add(newResult);
	}
	public void addLinkageMapGraph(LinkageMapGraph newMap)
	{
		if (maps == null)
			maps = new Vector<LinkageMapGraph>();
		
		newMap.setName("Linkage Map " + (++mapCount));
		maps.add(newMap);
	}
	
	public LinkageGroup getLinkageGroup()
		{ return lGroup; }
	
	public void removeResult(OrderedResult or) {
		if(or.is_phase()) phaseResults.remove(or);
		if(or.is_mds())  lGroup.getResults().remove(or);
		if(or.is_twopoint()) System.out.println("twopoint should not be a daughter node of OrderFolderNode");//lGroup.getResults().remove(or);
		if(or.is_nonsnp()) lGroup.getResults().remove(or);
		if(or.is_readqtl()) System.out.println("readqtl should not be a daughter node of OrderFolderNode"); 
	}
	
	public PhasePair[][] getPhasePairArray()
		{ return ppData; }
	public void setPhasePairArray(PhasePair[][] ppD)
	{ ppData = ppD; }
	
	/*public void saveas(String filename)
	{
		try{
			if(is_mds()){
				FileWriterMAP mapw = new FileWriterMAP(new File(filename));
				mapw.writeData(this, this.rows);
			}
			FileWriterSNPloc4 snploc = new FileWriterSNPloc4(new File(filename + ".SNPLOC"));
			snploc.writeData(lGroup, false);
			BufferedWriter out = new BufferedWriter(new FileWriter(new File(filename+"pairmatrix")));
			int matrixsize = ppData.length;
			for(int i = 0; i < matrixsize ; i++){
				for(int j = 0; j < matrixsize ; j++) {
					if(ppData[i][j] == null) out.write("NULL\t");
					else{
						if(ppData[i][j].cm1 != null) out.write(ppData[i][j].cm1.safeName + "-x-" + ppData[i][j].cm1.marker.getName() + "-x-");
						else out.write("NULL-x-");
						if(ppData[i][j].cm2 != null) out.write(ppData[i][j].cm2.safeName + "-x-" + ppData[i][j].cm2.marker.getName() + "\t");
						else out.write("NULL\t");
					}
				}
				out.write("\n");
			}
			out.close();
			
		}catch(Exception e)
		{
			e.printStackTrace();
		}
		
	}*/
	// Adds the given marker to the list of ordered markers
	public void addMarker(CMarker cm)
		{ lGroup.addMarker(cm);	}
	
	public void addDistance(float distance)
	{
		// TODO: Can we do this?
		// Ensure no distance is negative
		if (distance < 0){
			System.out.println("Trying to add negative distance (addDistance()) " + Prefs.d3.format(distance));
			distance *= -1;
		}

		
		// Removed (BB) : Multiply by 100 to get the distance into centiMorgans
		distances.add(distance);// * 100);
		//System.out.println(" addDistance:  " + distance);
		//System.out.println(" numdistances: " + distances.size());
	}
	
	// Returns the sum of all the inter-marker distances
	public float getDistanceTotal()
	{
		float total = 0;
		for (float d: distances)
			total += d;
		
		return total;
	}
	
	public boolean doesRowsContainZeros()
	{
		for (String[] s: rows)
			if (s[0].equals("0000") && s[1].equals("0000"))
				return true;
		
		return false;
	}
	public String loclist() {
			double sum = 0;
			String str = "Number	Name                        Position	NNfit\n";
			int i = 0;
			//double prevdist = 0.0;
			lGroup.freezeSafeNames();
			for(String l: tp5.toString().split("\\n")) {
				if(i != 0) {
					String[] ll = l.split(",");
					CMarker cm = lGroup.getMarkerBySafeNameNr(Integer.parseInt(ll[1]));
					Double loc =  Double.valueOf(ll[2]);
					Double nnfit = Double.valueOf(ll[3]);
					sum += nnfit;
					String mnam =cm.marker.getName();
					while(mnam.length() < 28) mnam += " ";
					
					str += ll[1] + "\t" + mnam + "\t" + Prefs.d1.format(loc) + "\t" + Prefs.d3.format(nnfit) + "\n";
					
				}
				i++;
			}
			meannnfit = sum / (i-1);
			
			return str;
	}
	public Vector<Float> getDistances() { return distances; }
	
	// Adds a new PhasePair
	public void addPhasePair(CMarker cm1, CMarker cm2, String p1, String p2)
	{
		PhasePair pp = new PhasePair(cm1, cm2, p1, p2);		
//		phasePairs.add(pp);
		
		//int i = cm1.getIndex();
		int i = lGroup.getMarkerIndexBySafeName(cm1.safeName);
		//System.out.println("addPP index for cm1 (" + cm1.safeName + "): " + i);
		
		//int j = cm2.getIndex();
		int j = lGroup.getMarkerIndexBySafeName(cm2.safeName);
		//System.out.println("addPP index for cm2 (" + cm2.safeName + "): " + j);
				
		ppData[i][j] = ppData[j][i] = pp;
	}
	
	// Sets a PhasePair's recombination frequency and LOD score
	public void setPhasePairValues(CMarker cm1, CMarker cm2, float rfq, float lod)
	{
		//int i = cm1.getIndex();
		int i = lGroup.getMarkerIndexBySafeName(cm1.safeName);

		//int j = cm2.getIndex();
		int j = lGroup.getMarkerIndexBySafeName(cm2.safeName);
		
		ppData[i][j].lod = lod;
		ppData[i][j].rfq = rfq;
		
		
	}
	public String getName()
	{
		return this.name;
	}
	public void setName(String name)
		{ 
			this.name = name; }
	
	public String toString()
		{ return name; }
	
	////////////////////////////////////////////////////////////////////////////
	public StringBuffer tp1 = new StringBuffer();
	public StringBuffer tp2 = new StringBuffer();
	public StringBuffer tp2a = new StringBuffer();
	public StringBuffer tp3 = new StringBuffer();
	public StringBuffer tp4 = new StringBuffer();
	public StringBuffer tp5 = new StringBuffer();

	public StringBuffer sm1 = new StringBuffer();
	public StringBuffer sm2 = new StringBuffer();
	
	public StringBuffer phases = new StringBuffer();
	
	public StringBuffer maploc = new StringBuffer();
	public StringBuffer mds_smacconf = new StringBuffer();
	public StringBuffer mds_locikey = new StringBuffer();
	public StringBuffer mds_pc = new StringBuffer();
	
	public void setMDS3dResults(File f1, File f2, File f3)
			throws Exception
		{
			readFile(mds_smacconf, f1);
			readFile(mds_locikey, f2);
			readFile(mds_pc, f3);
		}
	public void writeMDS3dResults(File f1, File f2, File f3)
				throws Exception
			{
				writeFile(mds_smacconf, f1);
				writeFile(mds_locikey, f2);
				writeFile(mds_pc, f3);
			}
	public void setTwoPointResults(File f1, File f2, File f3)
		throws Exception
	{
		readFile(tp1, f1);
		readFile(tp2, f2);
		readFile(tp2a, f3);
	}
	public void setTwoPointResults(File f1, File f2)
			throws Exception
		{
			readFile(tp1, f1);
			readFile(tp2, f2);
		}
	public void setMaplocResult(File f1)
	throws Exception
	{
		readFile(maploc, f1);
	}
	public void WriteMaploc(File f1)
	throws Exception
	{
		writeFile(maploc, f1);
	}
	public void WriteTwoPointResults(File f1, File f2)
		throws Exception
	{
		writeFile(tp1, f1);
		writeFile(tp2, f2);
	}
	public void WriteTwoPointResults(File f1, File f2, boolean numtonam)
			throws Exception
		{
			if(numtonam) {
				StringBuffer tp_tmp = new StringBuffer();
				translate_markernumbers_pwd(tp1, tp_tmp);
				writeFile(tp_tmp, f1);
				tp_tmp.setLength(0);
				translate_markernumbers_snpout(tp2, tp_tmp);
				writeFile(tp_tmp, f2);
				
			}else{
				writeFile(tp1, f1);
				writeFile(tp2, f2);
			}
		}
	public void WriteTwoPointResults(File f1, File f2, File f3, boolean numtonam)
			throws Exception
		{
			if(numtonam) {
				StringBuffer tp_tmp = new StringBuffer();
				translate_markernumbers_pwd(tp1, tp_tmp);
				writeFile(tp_tmp, f1);
				tp_tmp.setLength(0);
				translate_markernumbers_snpout(tp2, tp_tmp);
				writeFile(tp_tmp, f2);
				tp_tmp.setLength(0);
				translate_markernumbers_snpfullout(tp2a, tp_tmp);
				writeFile(tp_tmp, f3);
			}else{
				writeFile(tp1, f1);
				writeFile(tp2, f2);
				writeFile(tp2a, f3);
			}
		}
	public void setMDSResults(File f1, File f2, File f3)
		throws Exception
	{
		readFile(tp3, f1);
		readFile(tp4, f2);
		readFile(tp5, f3);
	}
	public void writeMDSResults(File f1, File f2, File f3)
			throws Exception
		{
			writeFile(tp3, f1);
			writeFile(tp4, f2);
			writeFile(tp5, f3);
		}
	
	public void setSimAnnealResults(File f1, File f2)
		throws Exception
	{
////	readFile(sm1, f1);
		readFile(sm2, f2);
	}
	
	private void readFile(StringBuffer str, File file)
		throws Exception
	{
		BufferedReader in = new BufferedReader(new FileReader(file));
		String line = in.readLine();
		while (line != null)
		{
			str.append(line + "\n");
			line = in.readLine();
		}
			
		in.close();
	}
	private void writeFile(StringBuffer str, File file)
		throws Exception
	{
		BufferedWriter out = new BufferedWriter(new FileWriter(file));
		String txt = str.toString();
		out.write(txt);
		out.close();
	}
	private void translate_marker(String s, StringBuffer sb){
		try{
			int i = Integer.parseInt(s);
			CMarker m =lGroup.getMarkerBySafeNameNr(i);
			sb.append(s + " (" + m.marker.getName() + ") ");
		}catch( Exception e){
			sb.append(s + " ");
		}
	}
	private void change_any_int(String s, StringBuffer sb) {
		StringTokenizer st = new StringTokenizer(s);
		while(st.hasMoreTokens()){
			translate_marker(st.nextToken(), sb);
		}
		sb.append("\n");
	}
	private void change_first_int(String s, StringBuffer sb) {
		StringTokenizer st = new StringTokenizer(s);
		if(st.hasMoreTokens()) translate_marker(st.nextToken(), sb);
		while(st.hasMoreTokens()){
			sb.append(st.nextToken() + " ");
		}
		sb.append("\n");
	}
	private void change_second_int(String s, StringBuffer sb) {
		StringTokenizer st = new StringTokenizer(s);
		if(st.hasMoreTokens()) sb.append(st.nextToken() + " ");
		if(st.hasMoreTokens()) translate_marker(st.nextToken(), sb);
		while(st.hasMoreTokens()){
			sb.append(st.nextToken() + " ");
		}
		sb.append("\n");
	}
	private void change_third_int(String s, StringBuffer sb) {
		StringTokenizer st = new StringTokenizer(s);
		if(st.hasMoreTokens()) sb.append(st.nextToken() + " ");
		if(st.hasMoreTokens()) sb.append(st.nextToken() + " ");
		if(st.hasMoreTokens()) translate_marker(st.nextToken(), sb);
		while(st.hasMoreTokens()){
			sb.append(st.nextToken() + " ");
		}
		sb.append("\n");
	}
	private void change_pairwise_ints(String s, StringBuffer sb) {
		// starts with INT: change it
		// starts with INT and only 2 words: change second
		// don't change anything else
		StringTokenizer st = new StringTokenizer(s);
		if(st.countTokens() == 0) {
			sb.append("\n");
			return;
		}
		if(s.startsWith(" ")) {
			sb.append(s + "\n");
		}else{
			if(st.countTokens() == 2) {
				change_any_int(s, sb);
			}else{
				change_first_int(s, sb);
			}
			sb.append("\n");
		}
	}
	private void change_first_two_ints(String s, StringBuffer sb) {
		StringTokenizer st = new StringTokenizer(s);
		if(st.hasMoreTokens()) translate_marker(st.nextToken(), sb);
		if(st.hasMoreTokens()) translate_marker(st.nextToken(), sb);
		while(st.hasMoreTokens()){
			sb.append(st.nextToken() + " ");
		}
		sb.append("\n");
	}
	
	private void translate_markernumbers_snpout(StringBuffer sb_in, StringBuffer sb_out){
		int parse_removed_loc = 1;
		int parse_most_likely = 2;
		int where_am_i = parse_removed_loc;
		String txt[] = sb_in.toString().split("\n");
		for(String s: txt){
			if(where_am_i == parse_removed_loc) {
				if(s.startsWith(" The most likely"))
				{
					where_am_i = parse_most_likely;
					sb_out.append(s + "\n");
				}else{
					change_any_int(s, sb_out);
				}
			}else if(where_am_i == parse_most_likely){
				change_first_int(s, sb_out);
			}
		}
	}
	private void translate_markernumbers_pwd(StringBuffer sb_in, StringBuffer sb_out){
		String txt[] = sb_in.toString().split("\n");
		boolean firstline = true;
		for(String s: txt){
			if(firstline) {
				sb_out.append(s + "\n");
				firstline = false;
			}else{
				change_first_two_ints(s, sb_out);
			}
		}
	}
	private void translate_markernumbers_snpfullout(StringBuffer sb_in, StringBuffer sb_out){
		int parse_parental_pheno = 1;
		int parse_locus = 2;
		int parse_pairwise = 3;
		//int parse_locus = 2;
		int where_am_i = parse_parental_pheno;
		String txt[] = sb_in.toString().split("\n");
		for(String s: txt){
			if(where_am_i == parse_parental_pheno) {
				if(s.startsWith("Locus")) {
					change_third_int(s, sb_out);
					where_am_i = parse_locus;
				}else{
					change_second_int(s, sb_out);
				}
			}else if(where_am_i == parse_locus) {
				if(s.startsWith(" Pairwise")) {
					where_am_i = parse_pairwise;
					sb_out.append(s + "\n");
				}else if(s.startsWith("Locus")) {
					change_third_int(s, sb_out);	
				}else{
					sb_out.append(s + "\n");
				}
			}else if(where_am_i == parse_pairwise) {
				change_pairwise_ints(s, sb_out);
			}
		}
	}
}
