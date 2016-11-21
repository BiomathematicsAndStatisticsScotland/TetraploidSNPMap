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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import data.OrderedResult;

public class AppFrameMenuBar extends JMenuBar {
	static final long serialVersionUID = 756204434942437L;
	private AppFrame appFrame;

	JMenu mFile, mFileRecent, mFileImportSelect;
	JMenuItem mFileNewProject, mFileOpenProject, mFileExit, mFileImport, mFileImportMap;
	JMenuItem mFileSave, mFileSaveAs, mFilePrint, mFilePrintSetup, mFixDRNP;

	JMenu mMarkers, mMarkersSelect;
	JMenuItem mTrait, mTraitView, mSelect, mSelectInvt, mSelectNone, mSelectAll;
	JMenuItem mMove, mPhase, mFileSaveSel, mInspect;

	JMenu mAnalysis;
	JMenuItem mAnalysisCluster, mAnalysisTwoPoint, mAnalysisMDS, mAnalysisOrder, mAnalysisLog;
	JMenuItem mAnalysisQTL, mAnalysisRemove, mAnalysisMap, mAnalysisAnova;

	public static AbstractAction aFileNewProject, aFileOpenProject, aFileExit, aFilePrint;
	public static AbstractAction aFileImport, aFileImportMap, aFileSave, aFileSaveAs, aFilePrintSetup;
	public static AbstractAction aTrait, aTraitView, aSelect, aSelectInvt, aSelectNone, aSelectAll;
	public static AbstractAction aMove, aPhase, aFileSaveSel, aFixDRNP, aAnalysisMDS, aInspect;
	public static AbstractAction aAnalysisCluster, aAnalysisOrder, aAnalysisLog, aAnalysisQTL;
	public static AbstractAction aAnalysisRemove, aAnalysisMap, aAnalysisAnova, aAnalysisTWOPOINT;

	/** AppFrameMenuBar. this creates all the menu items and actions for AppFrame.
	 * 
	 * @param appFrame = the AppFrame.
	 */
	public AppFrameMenuBar(AppFrame appFrame) {
		this.appFrame = appFrame;
		createCommonActions();
		if (appFrame.tpmModeSNP()) {
			createSNPActions();
		} else if (appFrame.tpmModeQTL()) {
			createQTLActions();
		} else {
			createNONSNPActions();
		}
		createFileMenu();
		createMarkersMenu();
		createAnalysisMenu();
		createHelpMenu();
		setBorderPainted(false);
		setInitialState();
		aFileSave.setEnabled(false);
	}

	@SuppressWarnings("serial")
	private void createCommonActions() {
		aFileNewProject = new AbstractAction("New Project...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.newProject();
			}
		};
		
		/*aInspect = new AbstractAction("Inspect") {
			public void actionPerformed(ActionEvent e) {
				appFrame.inspect();
			}
		};*/
		
		aFileOpenProject = new AbstractAction("Open Project...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.openProject(null);
			}
		};

		aFileSave = new AbstractAction("Save Project") {
			public void actionPerformed(ActionEvent e) {
				appFrame.saveProject(false);
			}
		};

		aFileSaveAs = new AbstractAction("Save Project As...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.saveProject(true);
			}
		};

		aFileSaveSel = new AbstractAction("Save Selected...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.saveSelected();
			}
		};

		aFilePrint = new AbstractAction("Print") {
			public void actionPerformed(ActionEvent e) {
				appFrame.print();
			}
		};

		aFilePrintSetup = new AbstractAction("Page Setup...") {
			public void actionPerformed(ActionEvent e) {
				PrinterDialog.showPageSetupDialog(appFrame);
			}
		};

		aFileExit = new AbstractAction("Exit") {
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		};

		aAnalysisLog = new AbstractAction("View Project Log...") {
			public void actionPerformed(ActionEvent e) {
				new LogViewerDialog(appFrame, appFrame.getProject());
			}
		};

	}

	@SuppressWarnings("serial")
	private void createQTLActions() {
		aFileImport = new AbstractAction("SNP dataset") {
			public void actionPerformed(ActionEvent e) {
				appFrame.importSNPDataSet();
			}
		};

		aFileImportMap = new AbstractAction("Map file") {
			public void actionPerformed(ActionEvent e) {
				appFrame.importSNPMap();
			}
		};

		aTrait = new AbstractAction("Associate Trait Data...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.associateTraitData();
			}
		};

		aTraitView = new AbstractAction("View Trait Data...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.viewTraitData();
			}
		};

		aAnalysisQTL = new AbstractAction("Run QTL Analysis...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runSNPQTL();
			}
		};

		aAnalysisMap = new AbstractAction("Generate Linkage Map") {
			public void actionPerformed(ActionEvent e) {
				appFrame.makeLinkageMap();
			}
		};

		aAnalysisRemove = new AbstractAction("Remove Results") {
			public void actionPerformed(ActionEvent e) {
				appFrame.removeAnalysis();
			}
		};
	}

	@SuppressWarnings("serial")
	private void createSNPActions() {
		aFileImport = new AbstractAction("SNP dataset") {
			public void actionPerformed(ActionEvent e) {
				appFrame.importSNPDataSet();
			}
		};

		aSelect = new AbstractAction("Select Markers...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(1);
			}
		};

		aSelectAll = new AbstractAction("Select All") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(2);
			}
		};

		aSelectNone = new AbstractAction("Select None") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(3);
			}
		};

		aSelectInvt = new AbstractAction("Invert Selection") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(4);
			}
		};

		aMove = new AbstractAction("Move Marker to Group...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.moveMarkers();
			}
		};

		aAnalysisCluster = new AbstractAction("Run Clustering...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runSNPCluster();
			}
		};

		aAnalysisTWOPOINT = new AbstractAction("Run TWOPOINT Analysis...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runTwoPointSNP();
			}
		};

		aAnalysisMDS = new AbstractAction("Run Multidimensional Scaling...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runMDS();
			}
		};

		aAnalysisMap = new AbstractAction("Generate Linkage Map") {
			public void actionPerformed(ActionEvent e) {
				appFrame.makeLinkageMap();
			}
		};

		aPhase = new AbstractAction("Call Phase Program...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runPhase();
			}
		};

		aAnalysisRemove = new AbstractAction("Remove Results") {
			public void actionPerformed(ActionEvent e) {
				appFrame.removeAnalysis();
			}
		};

		aFixDRNP = new AbstractAction("Fix DR/NP") {
			public void actionPerformed(ActionEvent e) {
				appFrame.fixDRNP();
			}
		};
	}

	@SuppressWarnings("serial")
	private void createNONSNPActions() {
		aFileImport = new AbstractAction("RFLP/AFLP/SSR dataset") {
			public void actionPerformed(ActionEvent e) {
				appFrame.importDataSet();
			}
		};

		aTrait = new AbstractAction("Associate Trait Data...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.associateTraitData();
			}
		};

		aTraitView = new AbstractAction("View Trait Data...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.viewTraitData();
			}
		};

		aSelect = new AbstractAction("Select Markers...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(1);
			}
		};

		aSelectAll = new AbstractAction("Select All") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(2);
			}
		};

		aSelectNone = new AbstractAction("Select None") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(3);
			}
		};

		aSelectInvt = new AbstractAction("Invert Selection") {
			public void actionPerformed(ActionEvent e) {
				appFrame.selectMarkers(4);
			}
		};

		aMove = new AbstractAction("Move Marker to Group...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.moveMarkers();
			}
		};

		aAnalysisCluster = new AbstractAction("Run Clustering...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runCluster();
			}
		};

		aAnalysisOrder = new AbstractAction("Run Marker Ordering...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runOrdering();
			}
		};

		aAnalysisAnova = new AbstractAction("Run Analysis of Variance...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runAnova();
			}
		};

		aAnalysisQTL = new AbstractAction("Run QTL Analysis...") {
			public void actionPerformed(ActionEvent e) {
				appFrame.runQTL();
			}
		};

		aAnalysisMap = new AbstractAction("Generate Linkage Map") {
			public void actionPerformed(ActionEvent e) {
				appFrame.makeLinkageMap();
			}
		};

		aAnalysisRemove = new AbstractAction("Remove Results") {
			public void actionPerformed(ActionEvent e) {
				appFrame.removeAnalysis();
			}
		};
	}

	private void createFileMenu() {
		mFile = new JMenu("File");
		mFile.setMnemonic(KeyEvent.VK_F);
		
		//mInspect = getItem(aInspect, 0,0,0,false);

		mFileRecent = new JMenu("Recent Projects");
		mFileRecent.setMnemonic(KeyEvent.VK_R);
		setRecentMenu("");

		mFileImportSelect = new JMenu("Import");
		mFileImportSelect.setMnemonic(KeyEvent.VK_I);

		mFileNewProject = getItem(aFileNewProject, KeyEvent.VK_N, KeyEvent.VK_N, KeyEvent.CTRL_MASK, false);
		mFileOpenProject = getItem(aFileOpenProject, KeyEvent.VK_O, KeyEvent.VK_O, KeyEvent.CTRL_MASK, false);
		mFileImport = getItem(aFileImport, KeyEvent.VK_I, 0, 0, false);
		mFileSave = getItem(aFileSave, KeyEvent.VK_S, KeyEvent.VK_S, KeyEvent.CTRL_MASK, false);
		mFileSaveAs = getItem(aFileSaveAs, 0, 0, 0, false);
		mFileSaveSel = getItem(aFileSaveSel, 0, 0, 0, false);
		mFileSaveAs.setDisplayedMnemonicIndex(5);
		mFilePrint = getItem(aFilePrint, KeyEvent.VK_P, KeyEvent.VK_P, KeyEvent.CTRL_MASK, false);
		mFilePrintSetup = getItem(aFilePrintSetup, KeyEvent.VK_U, 0, 0, false);
		mFileExit = getItem(aFileExit, KeyEvent.VK_X, 0, 0, false);

		//mFile.add(mInspect);
		mFile.add(mFileNewProject);
		mFile.add(mFileOpenProject);
		mFile.addSeparator();
		mFile.add(mFileSave);
		mFile.add(mFileSaveAs);
		if (appFrame.tpmModeSNP()) {
			mFile.add(mFileSaveSel);
		}
		mFile.addSeparator();
		mFileImportSelect.add(mFileImport);
		if (appFrame.tpmModeQTL()) {
			mFileImportMap = getItem(aFileImportMap, KeyEvent.VK_M, 0, 0, false);
			mFileImportSelect.add(mFileImportMap);
		}
		mFile.add(mFileImportSelect);
		mFile.addSeparator();
		mFile.add(mFilePrint);
		mFile.add(mFilePrintSetup);
		mFile.addSeparator();
		mFile.add(mFileRecent);
		mFile.addSeparator();
		mFile.add(mFileExit);

		add(mFile);
	}

	private void createMarkersMenu() {

		if (appFrame.tpmModeSNP()) {
			mMarkers = new JMenu("Markers");
			mMarkers.setMnemonic(KeyEvent.VK_M);
		} else {
			// QTL and NONSNP
			mMarkers = new JMenu("Trait data");
			mMarkers.setMnemonic(KeyEvent.VK_T);
			mTrait = getItem(aTrait, KeyEvent.VK_A, 0, 0, false);
			mTraitView = getItem(aTraitView, KeyEvent.VK_V, 0, 0, false);

			mMarkers.add(mTrait);
			mMarkers.add(mTraitView);
		}

		if (!appFrame.tpmModeQTL()) {
			mMarkersSelect = new JMenu("Select");
			mMarkersSelect.setMnemonic(KeyEvent.VK_S);

			mSelect = getItem(aSelect, KeyEvent.VK_S, KeyEvent.VK_F6, 0, false);
			mSelectAll = getItem(aSelectAll, KeyEvent.VK_A, 0, 0, false);
			mSelectNone = getItem(aSelectNone, KeyEvent.VK_N, 0, 0, false);
			mSelectInvt = getItem(aSelectInvt, KeyEvent.VK_I, 0, 0, false);

			mMove = getItem(aMove, KeyEvent.VK_M, KeyEvent.VK_M, KeyEvent.CTRL_MASK, false);

			mMarkers.add(mSelect);
			mMarkers.add(mSelectAll);
			mMarkers.add(mSelectNone);
			mMarkers.add(mSelectInvt);
			mMarkers.addSeparator();
			mMarkers.add(mMove);
		}
		if (appFrame.tpmModeSNP()) {
			mFixDRNP = getItem(aFixDRNP, KeyEvent.VK_F, 0, 0, false);
			mMarkers.add(mFixDRNP);
		}
		add(mMarkers);
	}

	private void createAnalysisMenu() {
		mAnalysis = new JMenu("Analysis");
		mAnalysis.setMnemonic(KeyEvent.VK_A);
		if (appFrame.tpmModeNONSNP()) {
			mAnalysisCluster = getItem(aAnalysisCluster, KeyEvent.VK_C, 
					KeyEvent.VK_1, KeyEvent.ALT_MASK, false);
			mAnalysisOrder = getItem(aAnalysisOrder, KeyEvent.VK_O, KeyEvent.VK_2, KeyEvent.ALT_MASK, false);
			mAnalysisAnova = getItem(aAnalysisAnova, KeyEvent.VK_A, KeyEvent.VK_4, KeyEvent.ALT_MASK, false);
			mAnalysisQTL = getItem(aAnalysisQTL, KeyEvent.VK_Q, KeyEvent.VK_5, KeyEvent.ALT_MASK, false);
			mAnalysisMap = getItem(aAnalysisMap, KeyEvent.VK_G, KeyEvent.VK_3, KeyEvent.ALT_MASK, false);
			mAnalysisLog = getItem(aAnalysisLog, KeyEvent.VK_V, 0, 0, false);
			mAnalysisRemove = getItem(aAnalysisRemove, KeyEvent.VK_R, 0, 0, false);

			mAnalysis.add(mAnalysisCluster);
			mAnalysis.add(mAnalysisOrder);
			mAnalysis.add(mAnalysisMap);
			mAnalysis.add(mAnalysisAnova);
			mAnalysis.add(mAnalysisQTL);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisLog);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisRemove);
		}

		if (appFrame.tpmModeQTL()) {
			mAnalysisQTL = getItem(aAnalysisQTL, KeyEvent.VK_Q, KeyEvent.VK_5, KeyEvent.ALT_MASK, false);
			mAnalysisMap = getItem(aAnalysisMap, KeyEvent.VK_G, KeyEvent.VK_3, KeyEvent.ALT_MASK, false);
			mAnalysisLog = getItem(aAnalysisLog, KeyEvent.VK_V, 0, 0, false);
			mAnalysisRemove = getItem(aAnalysisRemove, KeyEvent.VK_R, 0, 0, false);

			mAnalysis.add(mAnalysisQTL);
			mAnalysis.add(mAnalysisMap);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisLog);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisRemove);
		}

		if (appFrame.tpmModeSNP()) {
			mAnalysisCluster = getItem(aAnalysisCluster, KeyEvent.VK_C, 
					KeyEvent.VK_1, KeyEvent.ALT_MASK, false);
			mAnalysisTwoPoint = getItem(aAnalysisTWOPOINT, KeyEvent.VK_T, 
					KeyEvent.VK_6, KeyEvent.ALT_MASK, false);
			mAnalysisMDS = getItem(aAnalysisMDS, KeyEvent.VK_M, KeyEvent.VK_7, KeyEvent.ALT_MASK, false);
			mAnalysisMap = getItem(aAnalysisMap, KeyEvent.VK_G, KeyEvent.VK_3, KeyEvent.ALT_MASK, false);
			mPhase = getItem(aPhase, KeyEvent.VK_P, KeyEvent.VK_4, KeyEvent.ALT_MASK, false);

			mAnalysisLog = getItem(aAnalysisLog, KeyEvent.VK_V, 0, 0, false);
			mAnalysisRemove = getItem(aAnalysisRemove, KeyEvent.VK_R, 0, 0, false);

			mAnalysis.add(mAnalysisCluster);
			mAnalysis.add(mAnalysisTwoPoint);
			mAnalysis.add(mAnalysisMDS);
			mAnalysis.add(mAnalysisMap);
			mAnalysis.add(mPhase);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisLog);
			mAnalysis.addSeparator();
			mAnalysis.add(mAnalysisRemove);
		}

		add(mAnalysis);
	}

	private void createHelpMenu() {
		JMenu mHelp = new JMenu("Help");
		mHelp.setMnemonic(KeyEvent.VK_H);

		JMenuItem mHelpAbout = new JMenuItem("About TetraploidMap...");
		mHelpAbout.setMnemonic(KeyEvent.VK_A);
		mHelpAbout.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String msg = "<html><b>TetraploidMap_SNP for Windows</b><br><br>" 
						+ "Copyright &copy 2005-2016<br><br>"
						+ "Biomathematics & Statistics Scotland<br>"
						+ "C. A. Hackett, B. Boskamp, A. Vogogias, K. Preedy, I. Milne<br>"
						+ "<br/>This program is free software: you can redistribute it and/or modify<br/>"
						+ "it under the terms of the GNU General Public License as published by<br/>"
						+ "the Free Software Foundation, either version 3 of the License, or<br/>"
						+ "(at your option) any later version.<br/>"
						+ "<br/>This program is distributed in the hope that it will be useful,<br/>"
						+ "but WITHOUT ANY WARRANTY; without even the implied warranty of<br/>"
						+ "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the<br/>"
						+ "GNU General Public License for more details.<br/>"
						+ "<br/>You should have received a copy of the GNU General Public License<br/>"
						+ "along with this program.  If not, see <http://www.gnu.org/licenses/" + "</html>";

				doe.MsgBox.msg(msg, doe.MsgBox.INF);
			}
		});

		mHelp.add(mHelpAbout);

		add(mHelp);
	}

	private JMenuItem getItem(Action act, int m, int k, int mask, boolean icon) {
		JMenuItem item = new JMenuItem(act);
		item.setMnemonic(m);
		if (k != 0) {
			item.setAccelerator(KeyStroke.getKeyStroke(k, mask));
		}
		return item;
	}

	/** this sets a default state for all actions.
	 * 
	 */
	public void setInitialState() {
		if (appFrame.tpmModeNONSNP()) {
			// mFileImportSelect.setEnabled(false);
			aFileImport.setEnabled(false);
			// aFileSave.setEnabled(false);
			aFileSaveAs.setEnabled(false);
			aFileSaveSel.setEnabled(false);
			aFilePrint.setEnabled(false);
			aTrait.setEnabled(false);
			aTraitView.setEnabled(false);
			aSelect.setEnabled(false);
			aSelectAll.setEnabled(false);
			aSelectNone.setEnabled(false);
			aSelectInvt.setEnabled(false);
			aMove.setEnabled(false);
			aAnalysisCluster.setEnabled(false);
			aAnalysisOrder.setEnabled(false);
			aAnalysisLog.setEnabled(false);
			aAnalysisAnova.setEnabled(false);
			aAnalysisQTL.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aAnalysisRemove.setEnabled(false);
		}
		if (appFrame.tpmModeSNP()) {
			// mFileImportSelect.setEnabled(false);
			aFileImport.setEnabled(false);
			// aFileSave.setEnabled(false);
			aFileSaveAs.setEnabled(false);
			aFileSaveSel.setEnabled(false);
			aFilePrint.setEnabled(false);
			aSelect.setEnabled(false);
			aSelectAll.setEnabled(false);
			aSelectNone.setEnabled(false);
			aSelectInvt.setEnabled(false);
			aMove.setEnabled(false);
			aFixDRNP.setEnabled(false);
			aAnalysisCluster.setEnabled(false);
			aAnalysisTWOPOINT.setEnabled(false);
			aAnalysisMDS.setEnabled(false);
			aPhase.setEnabled(false);
			aAnalysisLog.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aAnalysisRemove.setEnabled(false);
		}
		if (appFrame.tpmModeQTL()) {
			// mFileImportSelect.setEnabled(false);
			aFileImport.setEnabled(false);
			aFileImportMap.setEnabled(false);
			// aFileSave.setEnabled(false);
			aFileSaveAs.setEnabled(false);
			aFileSaveSel.setEnabled(false);
			aFilePrint.setEnabled(false);
			aTrait.setEnabled(false);
			aTraitView.setEnabled(false);
			aAnalysisLog.setEnabled(false);
			aAnalysisQTL.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aAnalysisRemove.setEnabled(false);
		}
	}

	void setProjectOpenedState(Project project) {
		setProjectOpenedState();
		aFileSave.setEnabled(false);
		updateRecentFileList(project);
	}

	/** upon opening a project several menu options get enabled.
	 * 
	 */
	public void setProjectOpenedState() {
		//mFileImportSelect.setEnabled(true);
		aFileImport.setEnabled(true);
		aFileSaveAs.setEnabled(true);
		aAnalysisLog.setEnabled(true);
	}

	/** enable the correct menu actions depending on the ordered node type. 
	 * 
	 * @param orType = the ordered result type.
	 */
	public void setMenusForOrderedNode(int orType) {
		aFileSaveSel.setEnabled(true);
		switch (orType) {
			case OrderedResult.ORT_TWOPOINT: {
				aAnalysisMDS.setEnabled(true);
				aPhase.setEnabled(false);
				aAnalysisCluster.setEnabled(false);
				aAnalysisTWOPOINT.setEnabled(false);
				aAnalysisMap.setEnabled(false);
				break;
			}
			case OrderedResult.ORT_MDS: {
				aAnalysisMDS.setEnabled(false);
				aPhase.setEnabled(true);
				aAnalysisCluster.setEnabled(false);
				aAnalysisTWOPOINT.setEnabled(false);
				aAnalysisMap.setEnabled(true);
				break;
			}
			case OrderedResult.ORT_NONSNP: {
				aAnalysisCluster.setEnabled(false);
				aAnalysisOrder.setEnabled(false);
				aAnalysisAnova.setEnabled(false);
				aAnalysisQTL.setEnabled(true);
				aAnalysisMap.setEnabled(true);
				break;
			}
			case OrderedResult.ORT_READQTL: {
				aAnalysisMap.setEnabled(true);
				aAnalysisQTL.setEnabled(true);
				break;
			}
			case OrderedResult.ORT_PHASE: {
				aAnalysisMDS.setEnabled(false);
				aPhase.setEnabled(false);
				aAnalysisCluster.setEnabled(false);
				aAnalysisTWOPOINT.setEnabled(false);
				aAnalysisMap.setEnabled(false);
				break;
			}
			default: {
				System.err.println("setMenusForOrderedNode(); unimplemented OR type");
			}
		}
	}

	/** enable the correct menu actions for a markers node (a linkage group).
	 * 
	 */
	public void setMenusForMarkersNode() {
		if (appFrame.tpmModeSNP()) {
			aSelect.setEnabled(true);
			aSelectAll.setEnabled(true);
			aSelectNone.setEnabled(true);
			aSelectInvt.setEnabled(true);

			aFileSaveSel.setEnabled(true);

			aAnalysisCluster.setEnabled(true);
			aAnalysisTWOPOINT.setEnabled(true);
			aPhase.setEnabled(false);
			aAnalysisMDS.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aFixDRNP.setEnabled(false);

		}
		if (appFrame.tpmModeNONSNP()) {
			aSelect.setEnabled(true);
			aSelectAll.setEnabled(true);
			aSelectNone.setEnabled(true);
			aSelectInvt.setEnabled(true);

			aAnalysisCluster.setEnabled(true);
			aAnalysisOrder.setEnabled(true);
			aAnalysisAnova.setEnabled(true);
			aAnalysisQTL.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aTrait.setEnabled(true);
			aTraitView.setEnabled(true);

		}
		if (appFrame.tpmModeQTL()) {
			aTrait.setEnabled(true);
			aTraitView.setEnabled(true);
			aAnalysisQTL.setEnabled(false);
			aAnalysisMap.setEnabled(false);
			aFileImportMap.setEnabled(true);
		}

	}

	void updateRecentFileList(Project project) {
		setRecentMenu(Project.filename.getPath());
		appFrame.setTitle();
	}

	void setRecentMenu(String newStr) {
		mFileRecent.removeAll();
		int loc = -1;

		// First see if it already exists, and reorder the list if it does
		for (int i = 0; i < Prefs.gui_recent.size(); i++) {
			String value = (String) Prefs.gui_recent.get(i);

			if (value.equals(newStr)) loc = i;
		}

		if (loc != -1) {
			Prefs.gui_recent.remove(loc);
			Prefs.gui_recent.addFirst(newStr);
		} else if (newStr.length() > 0) {
			Prefs.gui_recent.addFirst(newStr);
		}

		// Then ensure the list only contains 5 elements
		while (Prefs.gui_recent.size() > 5) {
			Prefs.gui_recent.removeLast();
		}

		// Finally, convert the list into menu items...
		for (int i = 0; i < Prefs.gui_recent.size(); i++) {
			String value = (String) Prefs.gui_recent.get(i);
			createRecentMenuItem(value, (i + 1));
		}

		// ... and enable/disable the menu depending on its contents
		if (Prefs.gui_recent.size() == 0) {
			mFileRecent.setEnabled(false);
		} else {
			mFileRecent.setEnabled(true);
		}
	}

	@SuppressWarnings("serial")
	private void createRecentMenuItem(final String filename, int shortcut) {
		JMenuItem item = new JMenuItem(shortcut + " " + filename);

		switch (shortcut) {
			case 1:
				item.setMnemonic(KeyEvent.VK_1);
				break;
			case 2:
				item.setMnemonic(KeyEvent.VK_2);
				break;
			case 3:
				item.setMnemonic(KeyEvent.VK_3);
				break;
			case 4:
				item.setMnemonic(KeyEvent.VK_4);
				break;
			case 5:
				item.setMnemonic(KeyEvent.VK_5);
				break;
			default:
				// ignore
		}

		item.addActionListener(new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				appFrame.openProject(filename);
			}
		});

		mFileRecent.add(item);
	}
}