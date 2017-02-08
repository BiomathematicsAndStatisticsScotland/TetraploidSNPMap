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

package gui;

import java.awt.Font;
import java.io.File;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.LinkedList;
import java.util.Locale;
import javax.swing.UIManager;
import data.Marker;
import doe.Preferences;

public class Prefs extends Preferences {
	public static DecimalFormat d1 = new DecimalFormat("0.0");
	public static DecimalFormat d2 = new DecimalFormat("0.00");
	public static DecimalFormat d3 = new DecimalFormat("0.000");
	public static DecimalFormat d4 = new DecimalFormat("0.0000");
	public static DecimalFormat d5 = new DecimalFormat("0.00000");
	public static DecimalFormat d8 = new DecimalFormat("0.00000000");
	public static DecimalFormat i3 = new DecimalFormat("000");
	public static DecimalFormat i6 = new DecimalFormat("000000");

	static {
		// Force all output to be UK because the Fortran can't deal with 0,5
		// instead of 0.5
		d1.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.UK));
		d3.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.UK));
		d4.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.UK));
		d5.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.UK));
		d8.setDecimalFormatSymbols(new DecimalFormatSymbols(Locale.UK));
	}

	public static Font labelFont = (Font) UIManager.get("Label.font");

	public static String tools_scratch = System.getProperty("java.io.tmpdir") + File.separator + "tpmap";

	public static String tools_rscript = System.getProperty("user.dir") + File.separator
			+ "lib\\R\\bin\\x64\\Rscript.exe";

			
	public static String[] rlibs_env = {"R_LIBS="+System.getProperty("user.dir") + File.separator 
			+ "lib\\R\\library", "R_LIBS_USER="+System.getProperty("user.dir") + File.separator 
			+ "lib\\R\\library"};
	
	// User Manual
	
	public static String usermanual = System.getProperty("user.dir") + File.separator
			+ "docdata\\TetraploidSNPmap users manual.pdf";
			
	// SNP BINARIES:

	// recalc chisig
	public static String tools_recalc_chisig_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\recalc_chisig.exe";
	// snpMATCH
	public static String tools_snpmatch_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\SNPmatch_noimsl.exe";
	// SNPcluster
	public static String tools_SNPcluster_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\SNPcluster_noimsl.exe";
	// SNPcluster2 - only to create the distmatrix:
	public static String tools_cluster_chimatrixonly_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\cluster_chimatrixonly.exe";

	// SNPcexp
	public static String tools_SNPtwopoint_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\SNPcexp_noimsl_dupcheck.exe";
	// BB: phase program
	public static String tools_phase_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\phasev6_noimsl.exe";
	// SNPQTL
	public static String tools_snpqtl_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\SNP_QTL_newinput.exe";
	// readqtl
	public static String tools_readqtl_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\Read_QTLdata.exe";
	// snpqtlperm
	public static String tools_snpqtlperm_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\SNP_QTLperm_noimsl.exe";
	// simple_model
	public static String tools_simplemodel_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\simple_model.exe";
	// simple_model_additive
	public static String tools_simplemodeladditive_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\simple_model_additive.exe";

	// NON SNP binaries:

	// findgeno
	public static String tools_findgeno_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\findgeno.exe";
	// cluster
	public static String tools_cluster_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\cluster.exe";
	// simmatch
	public static String tools_simmatch_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\simmatch.exe";
	// anova
	public static String tools_anova_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\anova.exe";
	// perm
	public static String tools_perm_path = System.getProperty("user.dir") + File.separator 
			+ "lib\\binaries\\perm.exe";

	// permsimp
	public static String tools_permsimp_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\permsimp.exe";
	// twopoint
	public static String tools_twopoint_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\twopoint.exe";
	// altQTLmodel
	public static String tools_altqtl_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\altQTLmodel.exe";
	// ripple
	public static String tools_ripple_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\ripple.exe";
	// simanneal
	public static String tools_simanneal_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\simanneal.exe";
	// realoneparrecon
	public static String tools_qtl_path = System.getProperty("user.dir") + File.separator
			+ "lib\\binaries\\realoneparrecon.exe";

	// TV: path to R scripts
	public static String tools_Rscripts_path = System.getProperty("user.dir") + File.separator 
			+ "lib\\r-scripts";

	// public static String os_name = System.getProperty("os.name");

	// All other variables...
	public static String gui_dir = "tutorials"; // System.getProperty("user.dir");
	public static LinkedList<String> gui_recent = new LinkedList<String>();

	public static int maxperms = 500;

	public static int gui_iSize = 7;
	public static int gui_mSize = 12;

	public static int markername_maxlen = 20;
	public static int nonsnpmarkername_maxlen = 20;

	public static boolean gui_select_on_type; // Test for marker type
	public static int gui_select_type = Marker.AFLP; // with this value
	public static boolean gui_select_on_parent; // Test for present in parent
	public static int gui_select_parent = 1; // 1=p1, 2=p2, 3=both
	public static boolean gui_select_on_ratio; // Test for marker ratio
	public static int gui_select_ratio = Marker.R1_1; // with this value
	public static float gui_select_ratio_sig = 0.05f; // and this signifiance
	public static boolean gui_select_on_dr; // Test for double-reduction
	public static boolean gui_select_dr = true; // with this value
	public static float gui_select_dr_sig = 0.05f; // and this signifiance
	public static boolean gui_select_match_selected;

	public static float gui_pick_similarity = 0.9f;
	public static int gui_graph_scale_type = 0;

	public static boolean sim_run_ripple = false;
	public static boolean sim_run_sim = false;
	public static float sim_rt_value = 0.85f;
	public static float sim_t_value = 10f;
	public static float sim_nt_value = 100f;
	public static float sim_eps_value = 0.1f;

	private String os;
	private String tmpdir;
	private String arch;
	

	/** Prefs().
	 * 
	 */
	public Prefs() {
		os = System.getProperty("os.name");
		tmpdir = System.getProperty("java.io.tmpdir");
		arch = System.getProperty("os.arch");

		System.out.println("You are running: " + os);
		System.out.println("Your temp dir is: " + tmpdir);
		System.out.println("Your OS architecture is: " + arch);

		if (System.getProperty("os.name").equals("Linux")) {
			this.setLinuxPreferences();
		}
	}


	private void setLinuxPreferences() {
		/**
		 * UNUSED.
		 */
	}

	protected void getPreferences() {
		for (int i = 0; i < 4; i++) {
			String str = getStr(p.getProperty("gui_recent_" + i), "");
			if (str.length() > 0) {
				gui_recent.add(str);
			}
		}

		gui_dir = getStr(p.getProperty("gui_dir"), gui_dir);

		gui_select_on_type = getBool(p.getProperty("gui_select_on_type"), gui_select_on_type);
		gui_select_type = getInt(p.getProperty("gui_select_type"), gui_select_type);
		gui_select_on_parent = getBool(p.getProperty("gui_select_on_parent"), gui_select_on_parent);
		gui_select_parent = getInt(p.getProperty("gui_select_parent"), gui_select_parent);
		gui_select_on_ratio = getBool(p.getProperty("gui_select_on_ratio"), gui_select_on_ratio);
		gui_select_ratio = getInt(p.getProperty("gui_select_ratio"), gui_select_ratio);
		gui_select_ratio_sig = getFloat(p.getProperty("gui_select_ratio_sig"), gui_select_ratio_sig);
		gui_select_on_dr = getBool(p.getProperty("gui_select_on_dr"), gui_select_on_dr);
		gui_select_dr = getBool(p.getProperty("gui_select_dr"), gui_select_dr);
		gui_select_dr_sig = getFloat(p.getProperty("gui_select_dr_sig"), gui_select_dr_sig);
		gui_select_match_selected = getBool(p.getProperty("gui_select_match_selected"), 
				gui_select_match_selected);

		gui_pick_similarity = getFloat(p.getProperty("gui_pick_similarity"), gui_pick_similarity);
		gui_graph_scale_type = getInt(p.getProperty("gui_graph_scale_type"), gui_graph_scale_type);

		sim_run_ripple = getBool(p.getProperty("sim_run_ripple"), sim_run_ripple);
		sim_run_sim = getBool(p.getProperty("sim_run_sim"), sim_run_sim);
		sim_rt_value = getFloat(p.getProperty("sim_rt_value"), sim_rt_value);
		sim_t_value = getFloat(p.getProperty("sim_t_value"), sim_t_value);
		sim_nt_value = getFloat(p.getProperty("sim_nt_value"), sim_nt_value);
		sim_eps_value = getFloat(p.getProperty("sim_eps_value"), sim_eps_value);
	}

	protected void setPreferences() throws Exception {
		for (int i = 0; i < gui_recent.size(); i++) {
			write("gui_recent_" + i, setPath(gui_recent.get(i)));
		}

		write("gui_dir", setPath(gui_dir));

		write("gui_select_on_type", "" + gui_select_on_type);
		write("gui_select_type", "" + gui_select_type);
		write("gui_select_on_parent", "" + gui_select_on_parent);
		write("gui_select_parent", "" + gui_select_parent);
		write("gui_select_on_ratio", "" + gui_select_on_ratio);
		write("gui_select_ratio", "" + gui_select_ratio);
		write("gui_select_ratio_sig", "" + gui_select_ratio_sig);
		write("gui_select_on_dr", "" + gui_select_on_dr);
		write("gui_select_dr", "" + gui_select_dr);
		write("gui_select_dr_sig", "" + gui_select_dr_sig);
		write("gui_select_match_selected", "" + gui_select_match_selected);

		write("gui_pick_similarity", "" + gui_pick_similarity);
		write("gui_graph_scale_type", "" + gui_graph_scale_type);

		write("sim_run_ripple", "" + sim_run_ripple);
		write("sim_run_sim", "" + sim_run_sim);
		write("sim_rt_value", "" + sim_rt_value);
		write("sim_t_value", "" + sim_t_value);
		write("sim_nt_value", "" + sim_nt_value);
		write("sim_eps_value", "" + sim_eps_value);
	}
}