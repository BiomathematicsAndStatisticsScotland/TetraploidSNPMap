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

package analyses.order.mds;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;
import gui.AppFrame;
import gui.Prefs;

public class MDSDialog extends JDialog {
	/** MDSDialog. SNP only. asks 2d or 3d. Run MDS ordering (R code).
	 * 
	 */
	private static final long serialVersionUID = -6417958060558102301L;
	private LinkageGroup lGroup;
	private OrderedResult twopointresult;

	private JProgressBar pBar;
	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;
	private ProcessMDSResults pmr;
	private RunMDS runMDS;
	private int numDimensions;

	/** MDSDialog(). SNP only. ask 2d or 3d. Runs MDS ordering. (R code)
	 * 
	 * @param appFrame = the appFrame
	 * @param twopointresult = the twopoint ordered result. the SNPpwd get saved as twopoint.txt, 
	 the SNPout gets saved but is not used. 
	 * @param lGroup = the LinkageGroup with UI 'select' boxes. unselected -> omitvec.
	 */
	public MDSDialog(AppFrame appFrame, OrderedResult twopointresult, LinkageGroup lGroup) {
		super(appFrame, "Processing Dataset", true);

		this.twopointresult = twopointresult;
		this.lGroup = lGroup;
		if(twopointresult.getLinkageGroup().tooFewBridging(2, this.lGroup)) {
			MsgBox.msg("Too few bridging markers to run. You could try splitting the linkage group into one for each parent.", MsgBox.ERR);
			exit();
			return;
		}
		if(twopointresult.getLinkageGroup().tooFewBridging(10, this.lGroup)) {
			MsgBox.msg("Very few bridging markers. You could try splitting the linkage group into one for each parent.", MsgBox.WAR);
		}
		this.numDimensions = get2or3d();
		if (this.numDimensions == 0) {
			exit();
			return;
		}
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		pBar.setMaximum(4);
		statusLabel = new JLabel("Processing (stage 0/4)");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		descriptionLabel = new JLabel("Running marker ordering module (MDS)...");
		p1.add(descriptionLabel, BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(statusLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runMDS();
			}

			public void windowClosing(WindowEvent e) {
				exit();
			}
		});

		pack();
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}

	private void exit() {
		isOK = false;

		if (runMDS != null) runMDS.exit();

		setVisible(false);
	}

	private void runMDS() {
		// TV MDS R script files have already been copied to /tpmap directory
		writeMDSdata();
		copyMDSFiles();
		// Create the thread
		runMDS = new RunMDS(numDimensions);
		// Start the thread that will monitor its progress
		new RunMDSMonitor().start();
		// Start the thread that will run it
		runMDS.start();
	}

	private void writeMDSdata() {
		// System.out.println("runMDS - markers in parent lGroup: " +
		// lGroup.getSelectedMarkerCount());
		// System.out.println("runMDS - markers in stored twopointresult: " +
		// twopointresult.getLinkageGroup().getMarkerCount());
		Set<String> excludemarkers = twopointresult.getLinkageGroup().markersNotIn(lGroup);
		String omitvec;
		String comma = "\"";
		if (excludemarkers.isEmpty()) {
			omitvec = "omitvec<-NULL";
		} else {
			omitvec = "omitvec<-c(";
			for (String cm : excludemarkers) {
				omitvec += comma + cm;
				comma = "\", \"";
			}
			omitvec += "\")\n";
		}
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(
					new File(Prefs.tools_scratch, "omitvec.R")));
			out.write(omitvec);
			out.close();
		} catch (Exception e) {
			System.out.println("can't write omitvec.R" + e);
		}
		try {
			File file1 = new File(Prefs.tools_scratch, "twopoint.txt");
			File file2 = new File(Prefs.tools_scratch, "twopoint.SNPout");
			twopointresult.WriteTwoPointResults(file1, file2);
		} catch (Exception e) {
			System.out.println("can't write twopoint.txt, twopoint.SNPout" + e);
		}

	}

	// TV method that copies all R files required for the MDS ordering in the
	// /tpmap directory
	void copyMDSFiles() {
		// TV first define the files you wish to copy over
		File file1 = new File(Prefs.tools_Rscripts_path, "chromsimfunctions.R");
		File file2 = new File(Prefs.tools_Rscripts_path, "General_estimation.R");
		//File file3 = new File(Prefs.tools_Rscripts_path, "installp.R");
		//File file4 = new File(Prefs.tools_Rscripts_path, "threeD.R");

		// TV copy the files with copyInputFile() method
		File fileclone1 = new File(Prefs.tools_scratch, "chromsimfunctions.R");
		File fileclone2 = new File(Prefs.tools_scratch, "General_estimation.R");
		//File fileclone3 = new File(Prefs.tools_scratch, "installp.R");
		//File fileclone4 = new File(Prefs.tools_scratch, "threeD.R");
		try {
			AppFrame.copyInputFile(file1, fileclone1);
			AppFrame.copyInputFile(file2, fileclone2);
			//AppFrame.copyInputFile(file3, fileclone3);
			//AppFrame.copyInputFile(file4, fileclone4);
		} catch (IOException e) {
			System.out.println("ERR: The R Scripts have FAILED to get copied in /tpmap");
		}
	}

	private class RunMDSMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runMDS.step);
					statusLabel.setText("Processing MDS (step " + (runMDS.step) + " of 4)");
				}
			};
			// System.out.println("runMDSmonitor.run() started");
			while (runMDS.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RunMDSMonitor.run() interrupted..");
				}
			}

			SwingUtilities.invokeLater(r);

			if (runMDS.error == false) {
				processResults();
			} else {
				System.out.println("runMDSmonitor.run() runMDS.error - isOK=false");
				isOK = false;
			}

			setVisible(false);

			SwingUtilities.invokeLater(r);

			setVisible(false);
		}
	}

	public OrderedResult getResult() {
		return pmr.getResult();
	}

	private void processResults() {
		File phasemap = new File(Prefs.tools_scratch, "twopoint_phasemap.txt");
		File locikey = new File(Prefs.tools_scratch, "twopoint_conf.txt");
		File estimatedmap = new File(Prefs.tools_scratch, "twopoint_estimatedmap.txt");
		File imageFile = new File(Prefs.tools_scratch, "twopoint_diagplot.png");

		pmr = new ProcessMDSResults(phasemap, twopointresult, imageFile);
		pmr.start();
		while (pmr.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("MDSDialog.processResults(); interrupted..");
			}
		}

		try {
			pmr.getResult().setMDSResults(phasemap, locikey, estimatedmap);
			if (numDimensions == 3) {
				pmr.getResult().is3d = true;
				File mds3dSmacConf = new File(Prefs.tools_scratch, "smacofsym.conf");
				File mds3dLocikey = new File(Prefs.tools_scratch, "locikey");
				File mds3dPc = new File(Prefs.tools_scratch, "pc");
				pmr.getResult().setMDS3dResults(mds3dSmacConf, mds3dLocikey, mds3dPc);
			}
		} catch (Exception e) {
			MsgBox.msg("Failed to load the MDS 3d results.", MsgBox.WAR);
		}

		isOK = !pmr.error;
	}

	public boolean isOK() {
		return isOK;
	}

	
	
	private Integer get2or3d() {
		String[] selOpt = new String[2];
		selOpt[0] = "2d";
		selOpt[1] = "3d";
		String a = (String) JOptionPane.showInputDialog(this, "Do you want two or three dimensions?",
				"MDS can run in 2 or 3 dimensions. Please enter number of dimensions.",
				JOptionPane.PLAIN_MESSAGE, null,
				selOpt, selOpt[0]);
		if (a == null) return 0;
		if (a.contentEquals(selOpt[0])) return 2;
		return 3;
	}
}
