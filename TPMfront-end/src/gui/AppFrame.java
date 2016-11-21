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

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSplitPane;
import analyses.anova.AnovaDialog;
import analyses.cluster.ClusterAnalysis;
import analyses.fastcluster.SNPClusterAnalysis;
import analyses.findgeno.FindGenoDialog;
import analyses.findsnpgeno.FindSNPGenoDialog;
import analyses.findsnpgeno.FixDRNPDialog;
import analyses.order.LinkageAnalysis;
import analyses.order.OrderSettingsDialog;
import analyses.order.custom.CustomDialog;
import analyses.order.mds.MDSDialog;
import analyses.order.ripple.RippleDialog;
import analyses.order.sim.SimAnnealDialog;
import analyses.order.twopoint.TwoPointDialog;
import analyses.order.twopointsnp.TwoPointSNPDialog;
import analyses.phase.PhaseDialog;
import analyses.qtl.QTLDialog;
import analyses.readqtl.ReadQTLDialog;
import analyses.simmatch.SimMatchDialog;
import analyses.snpqtl.SNPQTLDialog;
import data.AnovaResult;
import data.CMarker;
import data.Cluster;
import data.LinkageGroup;
import data.LinkageMapGraph;
import data.OrderedResult;
import data.QTLResult;
import data.Summary;
import data.TraitFile;
import doe.MsgBox;
import gui.exporter.FileWriterPhase;
import gui.exporter.FileWriterSNPloc;
import gui.importer.ImportSNPFile;
import gui.importer.ImportTest;
import gui.importer.ImportTraitFile;
import gui.nav.MarkersNode;
import gui.nav.NavPanel;
import gui.nav.OrderedNode;
import gui.nav.PhaseNode;

public class AppFrame extends JFrame {
	private Prefs prefs;
	private Project project;
	public AppFrameMenuBar menubar;
	private AppFrameToolBar toolbar;

	static final long serialVersionUID = 620974691371146L;

	public static final int TPMMODE_NONSNP = 1;
	public static final int TPMMODE_SNP = 2;
	public static final int TPMMODE_QTL = 3;

	public static int tpmmode;

	static NavPanel navPanel;
	public static JSplitPane splits;
	public MarkerDetails markerDetails;
	public MarkerSNPDetails markerSNPDetails;

	private TwoPointDialog twopointdialog;

	/** AppFrame(prefs, tpmmode).
	 * 
	 * @param preferences = gui.Prefs object.
	 * @param tpmmode = int; NONSNP, SNP or QTL (1,2,3)
	 */
	public AppFrame(Prefs preferences, int tpmmode) {
		AppFrame.tpmmode = tpmmode;
		this.prefs = preferences;
		this.menubar = new AppFrameMenuBar(this);
		this.toolbar = new AppFrameToolBar(this);

		splits = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		navPanel = new NavPanel(this, splits);
		if (tpmmode != AppFrame.TPMMODE_NONSNP) {
			markerSNPDetails = new MarkerSNPDetails(navPanel);
		} else {
			markerDetails = new MarkerDetails(navPanel);
		}

		this.add(toolbar, BorderLayout.NORTH);
		this.add(splits);
		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				exit();
			}
		});

		this.setJMenuBar(menubar);
		this.setTitle();
		this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		this.setSize(1000, 800);
		this.setLocationRelativeTo(null);
		this.setIconImage(Icons.BB2.getImage());
		this.setVisible(true);
	}

	public Project getProject() {
		return project;
	}

	void setTitle() {
		if (project == null) {
			if (tpmModeSNP()) setTitle("TetraploidMap - SNP");
			if (tpmModeNONSNP()) setTitle("TetraploidMap - non-SNP");
			if (tpmModeQTL()) setTitle("TetraploidMap - SNP-QTL");
		} else {
			if (tpmModeSNP()) setTitle("TetraploidMap - SNP - " + Project.filename.getName());
			if (tpmModeNONSNP()) setTitle("TetraploidMap - non-SNP - " + Project.filename.getName());
			if (tpmModeQTL()) setTitle("TetraploidMap - SNP-QTL - " + Project.filename.getName());
		}
	}

	void exit() {
		if (!okToContinue()) return;

		// Prefs.gui_win_width = getWidth();
		// Prefs.gui_win_height = getHeight();

		prefs.savePreferences(System.getProperty("user.home") + Prefs.sepF + ".TetraploidMap.txt");
		System.exit(0);
	}

	// Called whenever an option has been selected that would close the
	// current project. The user is therefore queried to find out if they
	// would like to save it first (or cancel the operation)
	boolean okToContinue() {
		if (project != null) {
			if (AppFrameMenuBar.aFileSave.isEnabled()) {
				int res = MsgBox.yesnocan("The current project has unsaved " + "changes. Save now?", 0);

				if (res == JOptionPane.YES_OPTION) {
					if (Project.save(project, false)) {
						return true;
					} else {
						return false;
					}
				} else if (res == JOptionPane.NO_OPTION) {
					return true;
				} else if (res == JOptionPane.CANCEL_OPTION || res == JOptionPane.CLOSED_OPTION) {
					return false;
				}
			}
		}

		return true;
	}

	// Creates a new project
	void newProject() {
		if (!okToContinue()) return;

		NewProjectDialog dialog = new NewProjectDialog(this);
		if (dialog.getProject() != null) {
			project = dialog.getProject();

			menubar.setProjectOpenedState(project);
			navPanel.clear();

			MsgBox.msg("Project \"" + project.getName() + "\" successfully "
					+ "created. import datasets into it to begin analysis.", MsgBox.INF);
		}
	}

	// Opens an existing project
	void openProject(String filename) {
		if (!okToContinue()) return;

		setCursor(new Cursor(Cursor.WAIT_CURSOR));
		Project temp = Project.open(filename);
		if (temp != null) {
			project = temp;
			Project.logger.add("Session resumed.");

			menubar.setProjectOpenedState(project);
			if (tpmModeQTL() && !project.getLinkageGroups().isEmpty()) {
				AppFrameMenuBar.aFileImportMap.setEnabled(true);
			}
			navPanel.displayProject(project.getLinkageGroups());
		}

		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	// Saves the current project (either directly, or under a new name)
	void saveProject(boolean saveAs) {
		setCursor(new Cursor(Cursor.WAIT_CURSOR));

		try {
			Project.save(project, saveAs);
			AppFrameMenuBar.aFileSave.setEnabled(false);
		} catch (java.lang.StackOverflowError e) {
			e.printStackTrace();
			String msg = "An error has occurred while saving the project";
			MsgBox.msg(msg, MsgBox.ERR);
		}

		menubar.updateRecentFileList(project);

		setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	File choose_savefile(String dialogtitle) {
		JFileChooser fc = new JFileChooser();
		// fc.addChoosableFileFilter(Filters.getFileFilter(3));
		fc.setDialogTitle(dialogtitle);
		// fc.setSelectedFile(suggestedfilename);

		while (fc.showSaveDialog(MsgBox.frm) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			// Confirm overwrite
			if (file.exists()) {
				int response = MsgBox.yesnocan(file + " already exists.\nDo " + "you want to replace it?", 1);

				if (response == JOptionPane.NO_OPTION) {
					continue;
				} else if (response == JOptionPane.CANCEL_OPTION || response == JOptionPane.CLOSED_OPTION) {
					return (File) null;
				}
			}

			// Otherwise it's ok to save...
			Prefs.gui_dir = "" + fc.getCurrentDirectory();
			// filename = file;

			return file;
		}

		return (File) null;
	}

	void save_lgroup(LinkageGroup lGrp) {
		String dialogtitle = "Save ";
		int smc = lGrp.getSelectedMarkerCount();
		dialogtitle += smc;
		if (smc == 0) {
			MsgBox.msg("Nothing to save; no (selected) markers in this marker group", MsgBox.ERR);
			return;
		}
		if (smc == 1) {
			dialogtitle += " selected marker.";
		} else {
			dialogtitle += " selected markers.";
		}
		File f = choose_savefile(dialogtitle);
		if (f != null) {
			FileWriterSNPloc fw = new FileWriterSNPloc(f);
			fw.checkedOnly = true;
			fw.writeData(lGrp);
		}
	}

	void save_phase(OrderedResult or) {
		File f = choose_savefile("Save '" + or.getName() + "'");
		if (f != null) {
			FileWriterPhase fw = new FileWriterPhase(f);
			fw.writeData(or);
		}
	}

	void save_mds(OrderedResult or) {
		File phasemap = choose_savefile("Save the 'phasemap.txt' for '" + or.getName() + "'");
		File locikey = new File(phasemap.getPath() + ".locikey");
		File estimatedmap = new File(phasemap.getPath() + ".estimatedmap");
		try {
			or.writeMDSResults(phasemap, locikey, estimatedmap);
		} catch (Exception e) {
			MsgBox.msg("Saving MDS results failed.", MsgBox.WAR);
		}
	}

	void save_twopoint(OrderedResult or) {
		File pwd = choose_savefile("Save '" + or.getName() + "'");
		if (pwd != null) {
			File out = new File(pwd.getPath() + ".out");
			try {
				if (or.getFullOutput()) {
					File fullout = new File(pwd.getPath() + ".fullout");
					or.WriteTwoPointResults(pwd, out, fullout, true);
				} else {
					or.WriteTwoPointResults(pwd, out, true);
				}
			} catch (Exception e) {
				System.out.println("save_twopoint; error.");
				e.printStackTrace(System.out);
			}
		}
	}

	void save_orderedresult(OrderedResult or) {
		if (or.is_phase()) {
			save_phase(or);
		} else if (or.is_mds()) {
			save_mds(or);
		} else if (or.is_twopoint()) {
			save_twopoint(or);
		}

	}

	void saveSelected() {
		Object o = navPanel.getSelectedNode();
		if (o instanceof MarkersNode) {
			if (tpmmode == TPMMODE_SNP) {
				save_lgroup(((MarkersNode) o).lGroup);
			}
		} else if (o instanceof OrderedNode) {
			if (tpmmode == TPMMODE_SNP) {
				save_orderedresult(((OrderedNode) o).order);
			}
		} else if (o instanceof PhaseNode) {
			if (tpmmode == TPMMODE_SNP) {
				save_orderedresult(((PhaseNode) o).phaseResult);
			}
		}
	}

	void importDataSet() {
		// BB: this is only used for NON SNP datasets.
		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			LinkageGroup lGroup = null;

			lGroup = new ImportTest(file).getLinkageGroup();

			if (lGroup == null) return;

			FindGenoDialog fDialog = new FindGenoDialog(this, lGroup);
			if (fDialog.isOK() == false) return;

			SimMatchDialog sDialog = new SimMatchDialog(this, lGroup);
			if (sDialog.isOK() == false) return;

			navPanel.addRootLinkageGroup(lGroup);
			project.addLinkageGroup(lGroup);
			Project.logger.add("Dataset " + file + " imported.");

		}
	}

	/** copy a file.
	 * 
	 * @param sourceFile = File the file to copy.
	 * @param destFile = File, the file to create.
	 * @throws IOException = when IO fails.
	 */
	public static void copyInputFile(File sourceFile, File destFile) throws IOException {
		if (!destFile.exists()) {
			destFile.createNewFile();
		}

		FileChannel source = null;
		FileInputStream fis = null;
		FileChannel destination = null;
		FileOutputStream fos = null;
		try {
			fis = new FileInputStream(sourceFile);
			source = fis.getChannel();
			fos = new FileOutputStream(destFile);
			destination = fos.getChannel();
			// long i =
			destination.transferFrom(source, 0, source.size());
		} finally {
			if (source != null) {
				source.close();
			}
			if (fis != null) {
				fis.close();
			}
			if (fos != null) {
				fos.close();
			}
			if (destination != null) {
				destination.close();
			}
		}
	}

	void fixDRNP() {
		Runnable r = new Runnable() {
			public void run() {
				redisplayLG();
			}
		};

		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
		int[] npdrcount = lGroup.count_npdr();
		if (npdrcount[0] > 0 || npdrcount[1] > 0) {
			new FixDRNPDialog(this, npdrcount[1], npdrcount[0], lGroup, r);
		}
	}

	public void redisplayLG() {
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
		markerSNPDetails.displayLinkageGroup(lGroup);
	}

	void importSNPMap() {
		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		TraitFile tFile = root.getTraitFile();
		if (tFile == null) {
			MsgBox.msg("This dataset currently has no trait data associated "
					+ "with it. You must associate trait data before importing " + "a Map File.", MsgBox.ERR);
			return;
		}

		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			long start = System.currentTimeMillis();
			ReadQTLDialog dialog = new ReadQTLDialog(this, tFile, fc.getSelectedFile(), root);
			if (dialog.isOK()) {
				OrderedResult order = dialog.getResult();
				LinkageGroup lGroup = order.getLinkageGroup();
				lGroup.setTraitFile(tFile);
				order.setSummary(new Summary(root, (System.currentTimeMillis() - start)));
				root.addOrderedResult(order);
				navPanel.addOrderedResult(order, root, null);
				AppFrameMenuBar.aFileSave.setEnabled(true);
				Project.logger.add("imported map file");
			}

		}

	}

	void importSNPDataSet() {
		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			LinkageGroup lGroup = null;

			lGroup = new ImportSNPFile(file).getLinkageGroup();
			if (lGroup == null) {
				return;
			}

			FindSNPGenoDialog snpDialog = new FindSNPGenoDialog(this, lGroup);
			if (snpDialog.isOK() == false) return;

			navPanel.addRootLinkageGroup(lGroup);
			AppFrameMenuBar.aFileSave.setEnabled(true);
			project.addLinkageGroup(lGroup);
			Project.logger.add("Dataset " + file + " imported.");
		}
	}

	void runCluster() {
		// Get the currently selected LinkageGroup from the navigation tree.
		// Remember that more than one datasets can be loaded in a project
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();

		ClusterAnalysis cAnalysis = new ClusterAnalysis(this, lGroup);
		Cluster cluster = cAnalysis.getCluster();

		if (cluster != null) {
			lGroup.addCluster(cluster);
			navPanel.addCluster(cluster, lGroup, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);
			Project.logger.add("Cluster analysis run on " + navPanel.getSelectedLinkageGroupPathStr() + " - "
					+ cluster.getGroups().size() + " linkage group(s) created.");
		}
	}

	void runSNPCluster() {
		// Get the currently selected LinkageGroup from the navigation tree.
		// Remember that more than one datasets can be loaded in a project
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();

		SNPClusterAnalysis cAnalysis = new SNPClusterAnalysis(this, lGroup);
		Cluster cluster = cAnalysis.getCluster();

		if (cluster != null) {
			lGroup.addCluster(cluster);
			navPanel.addCluster(cluster, lGroup, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);

			Project.logger.add("Cluster analysis run on " + navPanel.getSelectedLinkageGroupPathStr() + " - "
					+ cluster.getGroups().size() + " linkage group(s) created.");
		}
	}
	
	/**  called from AppFrameMenuBar. SNP only. Runs the twopoint analysis on selected lGroup.
	 * 
	 */
	public void runTwoPointSNP() {
		// Get the currently selected LinkageGroup from the navigation tree
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
		if (lGroup.getSelectedMarkerCount() < 2) {
			MsgBox.msg("You must have at least 2 selected markers available to " 
					+ "run the ordering algorithms.", MsgBox.ERR);
			return;
		}

		long start = System.currentTimeMillis();

		// And clone it...
		LinkageGroup cGroup = lGroup.getClonedLinkageGroup(true, true);
		// second 'true': maintain names BB - trying to keep safeNames..

		TwoPointSNPDialog tpDialog = new TwoPointSNPDialog(this, cGroup);
		if (tpDialog.isOK() == false) return;

		OrderedResult order = tpDialog.getOrderedResult();
		boolean excludedup = order.getExcludeDuplicates();
		int numexcluded = order.getNumExcluded();
		order.setSummary(new Summary(lGroup, (System.currentTimeMillis() - start), excludedup, numexcluded));

		lGroup.addTwopointResult(order);
		navPanel.addOrderedResult(order, lGroup, null);
		AppFrameMenuBar.aFileSave.setEnabled(true);
		Project.logger.add("TwoPoint analysis run on " + navPanel.getSelectedLinkageGroupPathStr());
	}

	public void inspect() {
		navPanel.inspect();
		
		navPanel.inspectProject();
	}
	
	/** called from AppFrameMenuBar. SNP only. Runs the MDS ordering on selected twopoint result.
	 * 
	 */
	public void runMDS() {
		final long start = System.currentTimeMillis();

		// Get the currently selected LinkageGroup from the navigation tree
		LinkageGroup lGroup = navPanel.getParentLinkageGroup();
		if (lGroup == null) {
			System.out.println("failed to find markers node parent");
			return;
		}
		if (lGroup.getSelectedMarkerCount() < 2) {
			MsgBox.msg("You must have at least 2 selected markers available to " 
					+ "run the ordering algorithms.", MsgBox.ERR);
			return;
		}

		OrderedResult twopointresult = navPanel.getSelectedOrderedResult();
		MDSDialog mdsDialog = new MDSDialog(this, twopointresult, lGroup);
		if (mdsDialog.isOK() == false) {
			System.out.println("runMDS() isOK false");
			return;
		}

		OrderedResult mdsResult = mdsDialog.getResult();
		int nummarkers = mdsResult.getLinkageGroup().getMarkerCount();
		mdsResult.setSummary(new Summary(lGroup, (System.currentTimeMillis() - start),
				twopointresult.getExcludeDuplicates(), twopointresult.getNumExcluded(), nummarkers));
		twopointresult.getLinkageGroup().addMDSResult(mdsResult);
		navPanel.addOrderedResult(mdsResult, lGroup, null);
		AppFrameMenuBar.aFileSave.setEnabled(true);
		Project.logger.add("MDS run on " + navPanel.getSelectedLinkageGroupPathStr());
	}

	void runOrdering() {
		// Get the currently selected LinkageGroup from the navigation tree
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
		if (lGroup.getSelectedMarkerCount() < 2) {
			MsgBox.msg("You must have at least 2 selected markers available to " 
					+ "run the ordering algorithms.", MsgBox.ERR);
			return;
		}

		OrderSettingsDialog settings = new OrderSettingsDialog(this);
		if (settings.isOK() == false) return;

		final long start = System.currentTimeMillis();

		// And clone it...
		LinkageGroup cGroup = lGroup.getClonedLinkageGroup(true, false);

		TwoPointDialog tpDialog = new TwoPointDialog(this, cGroup);
		if (tpDialog.isOK() == false) return;
		OrderedResult order = tpDialog.getOrderedResult();

		CustomDialog cuDialog = new CustomDialog(this, order, cGroup);
		if (cuDialog.isOK() == false) return;

		if (Prefs.sim_run_ripple) {
			RippleDialog rpDialog = new RippleDialog(this, order, cGroup);
			if (rpDialog.isOK() == false) return;
		}

		if (Prefs.sim_run_sim) {
			SimAnnealDialog smDialog = new SimAnnealDialog(this, order, cGroup);
			if (smDialog.isOK() == false) return;
		}

		order.setSummary(new Summary(lGroup, (System.currentTimeMillis() - start)));
		lGroup.addOrderedResult(order);
		navPanel.addOrderedResult(order, lGroup, null);
		AppFrameMenuBar.aFileSave.setEnabled(true);
		Project.logger.add("Marker ordering run on " + navPanel.getSelectedLinkageGroupPathStr());
	}

	void runQTL() {
		// NON SNP

		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		TraitFile tFile = root.getTraitFile();
		if (tFile == null) {
			MsgBox.msg("This dataset currently has no trait data associated "
					+ "with it. You must associate trait data before running a QTL " 
					+ "analyis.", MsgBox.ERR);
			return;
		}

		OrderedResult order = navPanel.getCurrentOrderedResult();

		if (order.doesRowsContainZeros()) {
			String msg = "One or more marker phases are currently undefined "
					+ "(set to 0000). Are you sure you want to continue with the " 
					+ "QTL analysis at this stage?";
			if (MsgBox.yesno(msg, 1) != JOptionPane.YES_OPTION) return;
		}

		Object[] values = { "Parent 1", "Parent 2" };
		Object obj = JOptionPane.showInputDialog(MsgBox.frm, "Select which parent to use:", "Select Parent",
				JOptionPane.QUESTION_MESSAGE, null, values, values[0]);
		if (obj == null) return;
		int parent = (obj == values[0]) ? 1 : 2;

		QTLDialog dialog = new QTLDialog(this, order, tFile, parent);
		if (dialog.isOK()) {
			QTLResult qtlResult = dialog.getQTLResult();
			qtlResult.setTraitFile(tFile);

			order.addQTLResult(qtlResult);
			navPanel.addQTLResult(qtlResult, order, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}
	}

	void runSNPQTL() {
		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		TraitFile tFile = root.getTraitFile();
		if (tFile == null) {
			MsgBox.msg("This dataset currently has no trait data associated "
					+ "with it. You must associate trait data before running a QTL " 
					+ "analyis.", MsgBox.ERR);
			return;
		}

		OrderedResult order = navPanel.getCurrentOrderedHeadNode().order;

		SNPQTLDialog dialog = new SNPQTLDialog(this, order, tFile);
		if (dialog.isOK()) {
			QTLResult qtlResult = dialog.getQTLResult();
			qtlResult.setTraitFile(tFile);

			order.addQTLResult(qtlResult);
			navPanel.addQTLResult(qtlResult, order, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}
	}

	void runPhase() {
		long start = System.currentTimeMillis();

		// Get the currently selected LinkageGroup from the navigation tree
		LinkageGroup lGroup = navPanel.getParentLinkageGroup();
		if (lGroup == null) {
			System.out.println("failed to find markers node parent");
			return;
		}

		OrderedResult mdsresult = navPanel.getSelectedOrderedResult();
		OrderedResult twopointresult = navPanel.getTwopointForSelectedMDS();

		PhaseDialog dialog = new PhaseDialog(this, mdsresult, twopointresult);
		if (dialog.isOK()) {
			OrderedResult phaseResult = dialog.getResult();
			phaseResult.setSummary(new Summary(lGroup, (System.currentTimeMillis() - start)));
			mdsresult.addPhaseResult(phaseResult);
			navPanel.addPhaseResult(phaseResult, lGroup, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}
	}

	void selectMarkers(int selectionType) {
		// Get the currently selected LinkageGroup from the navigation tree
		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();

		if (isSNP()) {
			switch (selectionType) {
				case 1:
					SelectSNPMarkersDialog dialog = new SelectSNPMarkersDialog(this, lGroup);
					if (dialog.isOK() == false) return;
					break;
				case 2:
					SelectSNPMarkers.selectAll(lGroup);
					break;
				case 3:
					SelectSNPMarkers.selectNone(lGroup);
					break;
				case 4:
					SelectSNPMarkers.selectInvt(lGroup);
					break;
				default:
					System.out.println("selectMarkers(); isSNP() selectionType = " + selectionType);
					break;
			}
		} else { /* NON SNP */
			switch (selectionType) {
				case 1:
					SelectMarkersDialog dialog = new SelectMarkersDialog(this, lGroup);
					if (dialog.isOK() == false) return;
					break;
				case 2:
					SelectMarkers.selectAll(lGroup);
					break;
				case 3:
					SelectMarkers.selectNone(lGroup);
					break;
				case 4:
					SelectMarkers.selectInvt(lGroup);
					break;
				default:
					System.out.println("selectMarkers(); NONSNP selectionType = " + selectionType);
					break;
			}
		}
		navPanel.markersUpdated();
		AppFrameMenuBar.aFileSave.setEnabled(true);
	}

	void print() {
		IPrintable toPrint = navPanel.getSelectedPrintableNode();
		toPrint.print();
	}

	void removeAnalysis() {
		String msg = "Removing this analysis object will permanently remove "
				+ "all its results from the project. Continue?";
		if (MsgBox.yesno(msg, 1) == JOptionPane.YES_OPTION) {
			navPanel.removeAnalysis(project);
		}
	}

	void associateTraitData() {
		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		if (root.getTraitFile() != null) {
			if (MsgBox.yesno(
					"This dataset already has trait data associated "
							+ "with it. Are you sure you want to overwrite this data with " 
							+ "new information?", 1) != JOptionPane.YES_OPTION) {
				return;
			}
		}

		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			ImportTraitFile importer = new ImportTraitFile(file);
			if (importer.doImport() == false) return;

			TraitFile tFile = importer.getTraitFile();

			Project.logger.add("Trait file " + file + " imported.");

			root.setTraitFile(tFile);

			AppFrameMenuBar.aFileSave.setEnabled(true);
			viewTraitData();
		}
		if (tpmModeQTL()) AppFrameMenuBar.aFileImportMap.setEnabled(true);
	}

	void viewTraitData() {
		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		if (root.getTraitFile() == null) {
			MsgBox.msg("No trait information has been associated with this " + "dataset yet.", MsgBox.INF);
			return;
		}

		new TraitViewerDialog(this, root.getTraitFile());
	}

	void makeLinkageMap() {
		OrderedResult order = navPanel.getCurrentOrderedResult();

		LinkageAnalysis la;
		LinkageMapGraph map = null;
		// TV edit
		if (AppFrame.tpmmode == AppFrame.TPMMODE_SNP) {
			LinkageGroup[] lGroups = new LinkageGroup[] { order.getLinkageGroup() };
			map = new LinkageMapGraph(lGroups);

		} else if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
			la = new LinkageAnalysis(order);
			map = new LinkageMapGraph(la.getGroups());

		} else {
			// NONSNP
			Object[] values = { "Parent 1", "Parent 2" };
			Object obj = JOptionPane.showInputDialog(MsgBox.frm, "Select which parent to use:", 
					"Select Parent", JOptionPane.QUESTION_MESSAGE, null, values, values[0]);
			if (obj == null) return;
			int parent = (obj == values[0]) ? 1 : 2;

			la = new LinkageAnalysis(order, parent);
			map = new LinkageMapGraph(la.getGroups());

		}
		order.addLinkageMapGraph(map);
		navPanel.addLinkageMap(map, order, null);
		AppFrameMenuBar.aFileSave.setEnabled(true);
	}

	void runAnova() {
		LinkageGroup root = navPanel.getRootLinkageGroupForDataSet();
		if (root.getTraitFile() == null) {
			MsgBox.msg("No trait information has been associated with this " + "dataset yet.", MsgBox.INF);
			return;
		}

		LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
		LinkageGroup cGroup = lGroup.getClonedLinkageGroup(true, false);

		AnovaDialog dialog = new AnovaDialog(this, cGroup, root.getTraitFile());
		if (dialog.isOK()) {
			AnovaResult results = dialog.getAnovaResults();

			lGroup.addAnovaResults(results);
			navPanel.addAnovaResult(results, lGroup, null);
			AppFrameMenuBar.aFileSave.setEnabled(true);

			Project.logger.add("ANOVA run on " + navPanel.getSelectedLinkageGroupPathStr() + ".");
		}
	}

	void moveMarkers() {
		if (AppFrame.tpmmode == AppFrame.TPMMODE_SNP) {
			// Get the current marker
			CMarker cm = markerSNPDetails.getSelectedMarker();

			// And move it to another cluster's linkagegroup
			Cluster cluster = navPanel.getCurrentClusterHeadNode();
			if (cluster != null) {
				LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
				new MoveMarker(cm, lGroup, cluster);

				navPanel.refreshTree();
				markerSNPDetails.displayLinkageGroup(lGroup);
			}
		} else {
			// Get the current marker
			CMarker cm = markerDetails.getSelectedMarker();

			// And move it to another cluster's linkagegroup
			Cluster cluster = navPanel.getCurrentClusterHeadNode();
			if (cluster != null) {
				LinkageGroup lGroup = navPanel.getSelectedLinkageGroup();
				new MoveMarker(cm, lGroup, cluster);

				navPanel.refreshTree();
				markerDetails.displayLinkageGroup(lGroup);
			}
		}

	}

	public boolean isSNP() {
		if (AppFrame.tpmmode != AppFrame.TPMMODE_NONSNP) return true;
		return false;
	}

	public boolean tpmModeSNP() {
		return (AppFrame.tpmmode == AppFrame.TPMMODE_SNP);
	}

	public boolean tpmModeNONSNP() {
		return (AppFrame.tpmmode == AppFrame.TPMMODE_NONSNP);
	}

	public boolean tpmModeQTL() {
		return (AppFrame.tpmmode == AppFrame.TPMMODE_QTL);
	}

	public void setTwoPointDialog(TwoPointDialog runTwoPoint) {
		this.twopointdialog = runTwoPoint;
	}

	public TwoPointDialog getTwoPointDialog() {
		return this.twopointdialog;
	}

}