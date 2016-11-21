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
import java.text.DecimalFormat;
import java.util.LinkedList;
import java.util.Vector;
import gui.AppFrame;
import pal.statistics.ChiSquareDistribution;

/* Marker. SNP & NONSNP.
 * 
 * <p>This core class got enriched with the addition of some new data, required
 * for the representation of the new type of SNP markers.
 * 
 * Differences for SNP marker data:
 * - There is no percentageUnknown, but there is the "nmiss" attribute
 * - There are some new properties such as: observed frequencies, chi, np, df, chisig
 * - best ratio is not yet implemented in FORTRAN for SNP markers
 * - SNP type is treated like AFLP since both have only one(1) allele
 * 
 * What remains the same also for SNP marker data:
 * - the segregation ratio options
 */

public class Marker implements Serializable {
	static final long serialVersionUID = 4313476580087668492L;

	public static final int UNKNOWN = 0;

	public static final int RFLP = 1;
	public static final int AFLP = 2;
	public static final int SSR = 3;
	public static final int SNP = 4;

	// ratio's:
	public static final int R1_1 = 1;
	public static final int R5_1 = 5;
	public static final int R3_1 = 3;
	public static final int R11_1 = 11;
	public static final int R35_1 = 35;
	public static final int R_h = 7;

	private String snpRatio; // "1:1" or "higher"

	private String name = null;
	private String prefix = null;

	public Allele[] alleles;

	private int percentageUnknown;
	private String nmiss;
	private double chi;
	private int np;
	private double df;
	private double chisig;

	public boolean fix_changed = false; // used in FixDRNP

	// Marker type: AFLP, SSR, etc
	private int type = UNKNOWN;

	public LinkedList<MarkerBandPattern> bands = new LinkedList<MarkerBandPattern>();
	public LinkedList<MarkerSNPPattern> snpbands = new LinkedList<MarkerSNPPattern>();
	public LinkedList<MarkerProbability> probsPre = new LinkedList<MarkerProbability>();
	public LinkedList<MarkerProbability> probsAbs = new LinkedList<MarkerProbability>();

	// Parental phenotypes
	private String[] phenotype = new String[2];
	// Parental dosages - added by thanasis
	private String[] pdosage = new String[2];
	// Double reduction test - alpha and LR values
	private float drAlpha, drLR;
	private double drSig;
	private String errors = "";

	// Data post-FINDGENO that is calculated by this program
	// Marker ratio: 1:1, 3:1, etc
	private int ratio = UNKNOWN;
	// And the posterior probability that gives it
	// private MarkerProbability bestRatio = null;
	public MarkerProbability bestRatio = null;
	// private boolean isBestRatioKnown = true;
	// What parents is this marker present in
	private boolean[] isPresentInParent = new boolean[2];

	// Data obtained from SimMatch analysis
	// private SimMatchData simData;
	public SimMatchData simData;

	// Variable which stores the status of the Marker as this
	// is printed in the ReadingTPM.out :: OK/DR/NP
	private String status;

	/*
	 * **************************************************** Marker class
	 * constructor requires a name and the number of alleles it has
	 * ****************************************************
	 */
	public Marker(String markerName, int alleleCount) throws CreationException {
		name = markerName;

		if (alleleCount == 1) {
			type = AFLP;
		} else if (alleleCount == -1) {
			type = SNP;
		} else {
			type = SSR;
		}

		alleleCount = Math.abs(alleleCount);
		alleles = new Allele[alleleCount];

	}

	public void addError(String error) {
		this.errors += error + "\n";
	}

	public String getErrors() {
		return errors;
	}

	public void fix_npdr(boolean fix_np, boolean fix_dr) {
		if (status.contentEquals("DR") && !fix_dr) {
			this.fix_changed = false;
			return;
		}
		if (status.contentEquals("NP") && !fix_np) {
			this.fix_changed = false;
			return;
		}
		if (!status.contentEquals("NP") && !status.contentEquals("DR")) {
			this.fix_changed = false;
			return;
		}
		this.fix_changed = true;
		Vector<AlleleDosage> dosages = alleles[0].getDosages();

		byte pd1 = dosages.get(0).getDosage();
		byte pd2 = dosages.get(1).getDosage();
		if (pd1 > pd2) {
			byte t = pd1;
			pd1 = pd2;
			pd2 = t;
		}
		int[] count = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
		for (int i = 2; i < dosages.size(); i++) {
			byte d = dosages.get(i).getDosage();
			if (fix_np && pd1 == 0 && pd2 > 0 && pd2 < 4 && d > 2)
				dosages.get(i).setDosage((byte) 9);
			else if (fix_dr && pd1 == 1 && pd2 > 0 && pd2 < 4 && d == 4)
				dosages.get(i).setDosage((byte) 9);
			else if (fix_dr && d == 0 && pd2 == 3 && pd1 < 2)
				dosages.get(i).setDosage((byte) 9);
			else if (fix_dr && d == 3 && pd1 == 1 && pd2 == 1)
				dosages.get(i).setDosage((byte) 9);
			else if (fix_dr && d == 2 && pd1 == 0 && pd2 == 1)
				dosages.get(i).setDosage((byte) 9);
			count[dosages.get(i).getDosage()]++;
		}
		this.nmiss = "" + count[9];
		this.snpbands.clear();
		DecimalFormat df = new DecimalFormat("0.00000");
		float totalknown = (float) dosages.size() - 2 - count[9];
		for (int i = 0; i <= 4; i++) {
			if (count[i] > 0) {
				snpbands.add(new MarkerSNPPattern("" + i, "" + count[i], df.format((float) count[i] / totalknown)));
			}
		}
		if ((pdosage[0].contentEquals("1") && pdosage[1].contentEquals("0"))
				|| (pdosage[0].contentEquals("0") && pdosage[1].contentEquals("1")))
			snpRatio = "1:1";
		if (fix_dr && status.contentEquals("DR"))
			status = "FDR";
		if (fix_np && status.contentEquals("NP"))
			status = "FNP";
		if (!fix_np) {
			for (int i = 2; i < dosages.size(); i++) {
				byte d = dosages.get(i).getDosage();
				if (pd1 == 0 && pd2 > 0 && pd2 < 4 && d > 2 && d != 9)
					status = "NP";
			}
		}
		if (!fix_dr) {
			for (int i = 2; i < dosages.size(); i++) {
				byte d = dosages.get(i).getDosage();
				if (pd1 == 1 && pd2 > 0 && pd2 < 4 && d == 4)
					status = "DR";
				else if (d == 0 && pd2 == 3 && pd1 < 2)
					status = "DR";
				else if (d == 3 && pd1 == 1 && pd2 == 1)
					status = "DR";
				else if (d == 2 && pd1 == 0 && pd2 == 1)
					status = "DR";
			}
		}
		// System.out.println("marker " + toString() + " new status: " +
		// status);
	}

	public SimMatchData getSimMatchData() {
		if (simData == null)
			simData = new SimMatchData();
		return simData;
	}

	public void setType(int type) {
		this.type = type;
	}

	public void addAllele(int alleleIndex, Allele a) {
		alleles[alleleIndex] = a;
	}

	public void setParentDosage(String p1, String p2) {
		// TV Since Reading.out doesn't contain the right information for
		// Parent dosages when P1:P2 is 0:3 or 3:0 This is done here until
		// corrected in the back end
		// TODO: check if the data of individuals are getting updated properly
		// also
		if (p1.equals("0") && p2.equals("3")) {
			System.out.println("p1+p2(0,3): " + (p1 + p2));
			p2 = "1";
		} else if (p1.equals("3") && p2.equals("0")) {
			System.out.println("p1+p2(3,0): " + (p1 + p2));
			p1 = "1";
		}

		this.pdosage[0] = p1;
		this.pdosage[1] = p2;
	}

	public String getParentDosage(int p) {
		return this.pdosage[p - 1];
	}

	public void reorderPhenoType(int p, String neworder) {
		String newpheno = "";
		for (int i = 0; i < 4; i++) {
			String myc = "" + (i + 1);
			int j = neworder.indexOf(myc);

			newpheno += phenotype[p].charAt(j);

		}
		phenotype[p] = newpheno;
	}

	public void setPhenotypeInfo(String p1, String p2) {
		phenotype[0] = p1;
		phenotype[1] = p2;

		// Test for the presence of this marker in the two parents
		if (phenotype[0].contains("1"))
			isPresentInParent[0] = true;
		if (phenotype[1].contains("1"))
			isPresentInParent[1] = true;
	}

	public String getPhenotypeInfo(int p) {
		return phenotype[p - 1];
	}

	public boolean isPresentInParent(int p) {
		return isPresentInParent[p - 1];
	}

	public boolean isBestRatioKnown() {
		if (bestRatio == null)
			return false;
		else
			return true;
		// return isBestRatioKnown;
	}

	public void setDRTestValues(float a, float l) {
		drAlpha = a;
		drLR = l;

		drSig = (1 - (ChiSquareDistribution.cdf(drLR, 1)));
	}

	public void addMarkerBandPattern(MarkerBandPattern band) {
		bands.add(band);
	}

	// TV
	public void addMarkerSNPPatern(MarkerSNPPattern spat) {
		snpbands.add(spat);
	}

	public int getMarkerBandPatternCount() {
		return bands.size();
	}

	public void addMarkerProbability(MarkerProbability mp, boolean dblRPresent) {
		if (dblRPresent)
			MarkerProbability.insert(probsPre, mp);
		else
			MarkerProbability.insert(probsAbs, mp);
	}

	public String getName() {
		return name;
	}

	public String getPrefix() {
		if (prefix == null)
			return "";
		else
			return prefix;
	}

	public String getDendrogramName() {
		if (prefix == null)
			return name;
		else
			// return "(" + prefix + ") " + name;
			return prefix + " " + name;
	}

	public void setPrefix(String newPrefix) {
		// System.out.println("marker.setPrefix('" + newPrefix + "');");
		prefix = newPrefix;
	}

	public void setPrefixIfunset(String newPrefix) {
		if (prefix == null) {
			// System.out.println("marker " + this.toString() + " setting prefix
			// to " + newPrefix);
			prefix = newPrefix;
		} else {
			// System.out.println("marker " + this.toString() + " NOT setting
			// prefix to " + newPrefix);
		}
	}

	// TV methods to set and get bridge attribute
	// public void setBridge(boolean i_bridge)
	// {
	// bridge = i_bridge;
	// }

	// public boolean isBridge()
	// {
	// return bridge;
	// }

	public String toString() {
		return name;
	}

	public Allele getAllele(int index) {
		return alleles[index];
	}

	public int getType() {
		return type;
	}

	public int getAlleleCount() {
		return alleles.length;
	}

	public void setAlpha(float alpha) {
		this.drAlpha = alpha;
	}

	public float getAlpha() {
		return drAlpha;
	}

	public float getLR() {
		return drLR;
	}

	public double getDRSignificance() {
		return drSig;
	}

	public String getTypeDescription(boolean full) {
		switch (type) {
		case AFLP:
			return full ? "AFLP" : "A";
		case SSR:
			return full ? "SSR" : "S";
		case RFLP:
			return full ? "RFLP" : "R";
		case SNP:
			return full ? "SNP" : "D";
		}

		return "--";
	}

	// Attempts to determine the ratio of this marker. The ratio is known if
	// a) it's an AFLP marker with one best
	public boolean determineRatio() {
		// TV: This method should never get called when we use SNP Marker data
		if (AppFrame.tpmmode != AppFrame.TPMMODE_NONSNP)
			System.out.println(
					"| !  TV ALERT  ! | __ |! ! !  Marker.determineRatio() called --------> Only to be called for non-SNP markers |");

		boolean canDetermineRatio = true;

		// In trouble? if none were found
		if (probsAbs.size() == 0)
			return false;

		// Check to see if multiple best matches were found
		if (probsAbs.size() > 1) {
			if (probsAbs.get(0).pb == probsAbs.get(1).pb && probsAbs.get(0).sig == probsAbs.get(1).sig) {
				canDetermineRatio = false;
			}
		}

		bestRatio = probsAbs.get(0);

		// We can only do this for AFLP markers?
		// TODO: RFLPs too?
		if (type != AFLP) {
			if (canDetermineRatio == false)
				bestRatio = null;
			return canDetermineRatio;
		}

		// 1:1
		if (bestRatio.p1.equals("1000") && bestRatio.p2.equals("0000")
				|| bestRatio.p1.equals("0000") && bestRatio.p2.equals("1000")) {
			ratio = R1_1;
		}

		// 5:1
		else if (bestRatio.p1.equals("1100") && bestRatio.p2.equals("0000")
				|| bestRatio.p1.equals("0000") && bestRatio.p2.equals("1100")) {
			ratio = R5_1;
		}

		// 11:1
		else if (bestRatio.p1.equals("1100") && bestRatio.p2.equals("1000")
				|| bestRatio.p1.equals("1000") && bestRatio.p2.equals("1100")) {
			ratio = R11_1;
		}

		// 3:1
		else if (bestRatio.p1.equals("1000") && bestRatio.p2.equals("1000")) {
			ratio = R3_1;
		}

		// 35:1
		else if (bestRatio.p1.equals("1100") && bestRatio.p2.equals("1100")) {
			ratio = R35_1;
		}

		// TV next 2 added to determine ratio for SNP markers
		// Replaced by setRatio() method
		// 1:1 - SNP
		/*
		 * else if (snpRatio.equals("1:1")) { ratio = R1_1; }
		 * 
		 * // higher - SNP else if (snpRatio.equals("higher")) { ratio = R_h; }
		 */
		// Although the bestRatio was allowed above, it was only temporary when
		// the canDetermineRatio value is false, otherwise markers will be
		// selectable when they shouldn't be.
		if (canDetermineRatio == false)
			bestRatio = null;
		return canDetermineRatio;
	}

	public double getRatioSignificance() {
		if (bestRatio != null)
			return bestRatio.sig;

		return 0;
	}

	public String getRatioGenotypes() {
		if (bestRatio != null)
			return bestRatio.p1 + " " + bestRatio.p2;
		else
			return "bestRatioUnknown";
		// else if (isBestRatioKnown == false)
		// return "bestRatioUnknown";
		// else
		// return "N/A";
	}

	/*
	 * TV: This method is getting called only for SNP Markers instead of the
	 * method determineRatio() which is called for the old Markers
	 * 
	 * public void setRatio(String i_ratio) { // determine ratio for SNP markers
	 * // 1:1 - SNP if (i_ratio.equals("1:1")) { this.ratio = R1_1; } // higher
	 * - SNP else if (i_ratio.equals("higher")) { this.ratio = R_h; } }
	 * 
	 */

	public String getRatio() {
		switch (ratio) {
		case R1_1:
			return "1:1";
		case R5_1:
			return "5:1";
		case R11_1:
			return "11:1";
		case R3_1:
			return "3:1";
		case R35_1:
			return "35:1";
		}

		if (bestRatio != null)
			// if (isBestRatioKnown)
			return "N/A";
		else
			return "--";
	}

	// TV added for direct setting of ratio for SNP Markers when no
	// determineRatio()
	// method is required
	// NOTE: possibly later on it could get determined somehow from the data
	public void setRatioCode(int i_ratio) {
		this.ratio = i_ratio;
	}

	public int getRatioCode() {
		return ratio;
	}

	// public int getSNPRatioCode() {return snpRatio; }

	public String getSNPSummaryInfo() {
		String str = new String("<html><pre><font size='3'>");

		str += "<b>Number of individuals:</b> " + (alleles[0].getDosageCount() - 2) + " (" + getNMiss() + " nmiss)<br>";

		str += "<br><b>P1 dosage  P2 dosage</b>" + "<br> " + this.getParentDosage(1) + "           "
				+ this.getParentDosage(2) + "<br>";

		str += "<br><b>SNP Pattern</b>";
		str += "<table border=\"1\"><tr><td>Dosage</td><td>Count</td><td>Proportion</td></tr>";
		for (MarkerSNPPattern spat : snpbands)
			str += "<tr><td>" + spat.dosage + "</td><td>" + spat.count + "</td><td>" + spat.proportion + "</td></tr>";
		str += "</table>";
		str += "<br>";

		str += "<br><b>Test for double reduction</b>" + "<br>MLE of alpha: " + drAlpha + "<br>";
		// BB debugging:-->
		/*
		 * str += "<br>present in p1: " + isPresentInParent(1); str +=
		 * "<br>present in p2: " + isPresentInParent(2);
		 * 
		 * str += "<br>p1 pheno: " + phenotype[0]; str += "<br>p2 pheno: " +
		 * phenotype[1];
		 */
		str += "</html>";
		return str;
	}

	public String getSummaryInfo() {
		String str = new String("<html><pre><font size='3'>");

		str += "<b>Number of alleles:</b> " + alleles.length + "<br>";
		str += "<b>Number of individuals:</b> " + (alleles[0].getStateCount() - 2) + " (" + getMissingPercentage()
				+ "% unknown)<br>";

		str += "<br><b>Phenotype 1  Phenotype 2</b>" + "<br> " + phenotype[0] + "     " + phenotype[1] + "<br>";

		str += "<br><b>Test for double reduction</b>" + "<br>MLE of alpha and LR test: " + drAlpha + " " + drLR
				+ "<br>DR significance: " + drSig + "<br>";

		str += "<br><b>Marker Band Patterns</b>";
		for (MarkerBandPattern mp : bands)
			str += "<br> " + mp.pattern + "\t" + mp.count + "\t" + mp.proportion;
		str += "<br>";

		DecimalFormat d = new DecimalFormat("0.0000000000");
		str += "<br><b>Posterior probabilities in presence of double reduction:"
				+ "<br> Parental genotypes\tProbability\td.f.\tchisquare\t sig</b>";
		for (MarkerProbability mp : probsPre)
			str += "<br> " + mp.p1 + " " + mp.p2 + "\t\t" + d.format(mp.pb) + "\t" + mp.df + "\t" + mp.chi + "\t "
					+ (mp.sig);
		if (probsPre.size() == 0)
			str += "<br> No data available";
		str += "<br><b>And in absence of double reduction:</b>";
		for (MarkerProbability mp : probsAbs)
			str += "<br> " + mp.p1 + " " + mp.p2 + "\t\t" + d.format(mp.pb) + "\t" + mp.df + "\t" + mp.chi + "\t "
					+ (mp.sig);
		if (probsAbs.size() == 0)
			str += "<br> No data available";

		str += "</html>";
		return str;
	}

	public String getName(int width) {
		if (name.length() >= width)
			return name;

		String str = name;
		for (int i = name.length(); i < width; i++)
			str += " ";
		return str;
	}

	public String getSNPRatio() {
		return this.snpRatio;
	}

	public void setSNPRatio(String i_snpRatio) {
		this.snpRatio = i_snpRatio;
	}

	/*
	 * TV: MarkerSNPPattern class stores this variable public double getFreq(int
	 * i) { return this.freq[i]; }
	 * 
	 * public void setFreq(int i, double ifreq) { this.freq[i] = ifreq; }
	 */

	public double getChi() {
		return this.chi;
	}

	public void setChi(double i_chi) {
		this.chi = i_chi;
	}

	// TV: It seems that ChiSig is the equivalent to RatioSignificance for old
	// markers
	public double getChiSig() {
		return this.chisig;
	}

	public void setChiSig(double i_chisig) {
		this.chisig = i_chisig;
	}

	public int getNP() {
		return this.np;
	}

	public void setNP(int i_np) {
		this.np = i_np;
	}

	public double getDF() {
		return this.df;
	}

	public void setDF(double i_df) {
		this.df = i_df;
	}

	private void determineMissingPercentage() {
		int count = 0;
		for (AlleleState state : alleles[0].getStates())
			if (state.getState() == AlleleState.UNKNOWN)
				count++;

		float percent = (count / (float) (alleles[0].getStateCount())) * 100;
		percentageUnknown = Math.round(percent);
	}

	public int getMissingPercentage() {
		return percentageUnknown;
	}

	// nmiss handling added by thanasis
	public String getNMiss() {
		return nmiss;
	}

	public void setNMiss(String _nmiss) {
		this.nmiss = _nmiss;
	}

	public boolean canEnable() {
		return canEnable(false);
	}

	public boolean canEnable(boolean ignore_fnp) {
		// TV: condition for SNP Markers
		if (AppFrame.tpmmode != AppFrame.TPMMODE_NONSNP) {
			// System.out.println(this.toString() + " " + this.getChiSig() + "
			// '" + this.status + "'");
			if (!ignore_fnp && this.status != null && this.status.contentEquals("FNP"))
				return true;
			if (this.getChiSig() < 0.001)
				return false;
			if (this.status == null)
				return true;
			if (this.status.contentEquals("NP"))
				return false;
			if (this.status.contentEquals("DR"))
				return false;
			if (this.status.contentEquals("00"))
				return false;
			if (this.status.contentEquals("MO"))
				return false;
			return true;
		}

		// A marker's best genotype must be known
		boolean isOK = isBestRatioKnown();

		// And for SSRs, information on absense of dbl-reduction must be known
		if (type == SSR && probsAbs.size() == 0)
			isOK = false;

		return isOK;
	}

	// UPDATE: method merged with canEnable()
	// TV: filters SNP Markers that can not be selected
	// The SNP markers with Ratio_Sig very close to 0 represent false data
	/*
	 * public boolean canSNPEnable() { boolean isOK = false;
	 * 
	 * if (this.getChiSig() > 0.001) isOK = true;
	 * 
	 * return isOK; }
	 */

	// Performs a series of checks on the allele data within this marker to
	// ensure it is valid for further analysis by the fortran routines
	void verifyAlleles() throws CreationException {
		if (getAlleleCount() == 0)
			throw new CreationException(CreationException.NO_ALLELES);
		if (getAlleleCount() > 8)
			throw new CreationException(CreationException.TOO_MANY_ALLELES);

		if (type == SSR) {
			int iCount = alleles[0].getStateCount();
			byte UNKNOWN = AlleleState.UNKNOWN;

			for (int i = 0; i < iCount; i++) {
				// change the first allele for this I to 9 if any of the
				// subsequent alleles are 9
				for (int a = 1; a < alleles.length; a++) {
					if (alleles[a].getStates().get(i).getState() == UNKNOWN) {
						alleles[0].getStates().get(i).setState(UNKNOWN);
						break;
					}
				}

			}
		}

		determineMissingPercentage();
	}

	// Methods for seting the status of the Marker depending on
	// what was printed in the ReadingTPM.out file
	public void setStatus(String status) {
		this.status = status;
	}

	public String getStatus() {
		return this.status;
	}

}