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

package analyses.order.twopointsnp;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
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
import gui.Project;
import gui.Utils;
import gui.exporter.FileWriterSNPloc;

public class TwoPointSNPDialog extends JDialog {
	/** TwoPointSNPDialog. SNP only.
	 *  Runs the twopoint analysis.
	 */
	private static final long serialVersionUID = -6301950620812366965L;
	private LinkageGroup lGroup;
	private OrderedResult order;
	private JProgressBar pBar;
	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;

	private RunTwoPointSNP twoPoint;

	private String charExclDup, charFullOut;

	/** TwoPointSNPDialog. SNP only.
	 * 
	 * @param appFrame = the appframe
	 * @param grp = the linkage group
	 */
	public TwoPointSNPDialog(AppFrame appFrame, LinkageGroup grp) {
		super(appFrame, "Processing Dataset", true);

		lGroup = grp;
		if(this.lGroup.tooFewBridging(2)) {
			MsgBox.msg("Too few bridging markers to allow MDS to run. You could try splitting the linkage group into one for each parent.", MsgBox.ERR);
		}else if(this.lGroup.tooFewBridging(10)) {
			MsgBox.msg("Very few bridging markers. You could try splitting the linkage group into one for each parent.", MsgBox.WAR);
		}
		order = new OrderedResult(lGroup, OrderedResult.ORT_TWOPOINT);
		if (ProcessTwoPointSNPResults.verifyForTwoPoint(lGroup) == false) {
			isOK = false;
			return;
		}

		charExclDup = this.getExludeDuplicate();
		if (charExclDup == null) {
			exit();
			return;
		}
		charFullOut = this.getFullOutput();
		if (charFullOut == null) {
			exit();
			return;
		}
		order.setExcludeDuplicates((charExclDup.startsWith("y")));
		order.setFullOutput((charFullOut.startsWith("y")));

		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		statusLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		descriptionLabel = new JLabel("Running two-point linkage analysis (TWOPOINT)...");
		p1.add(descriptionLabel, BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(statusLabel, BorderLayout.SOUTH);
		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runTwoPoint();
			}

			public void windowClosing(WindowEvent e) {
				exit();
			}
		});
		pack();
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);

		// System.out.println("__TwoPointDialog is DONE!");
	}

	private void exit() {
		isOK = false;
		if (twoPoint != null) twoPoint.exit();
		setVisible(false);
	}

	private void runTwoPoint() {
		// Write out the file that TwoPoint needs
		Utils.emptyScratch();
		File file;
		file = new File(Prefs.tools_scratch, "twopoint.SNPloc");
		FileWriterSNPloc writer = new FileWriterSNPloc(file);
		writer.checkedOnly = true;
		writer.safeNameNrs = true;
		if (writer.writeData(lGroup) == false) {
			exit();
			return;
		}

		// Create TwoPoint
		twoPoint = new RunTwoPointSNP(charExclDup, charFullOut);
		// twoPoint = new RunTwoPointSNP(charToSend);
		// Start the thread that will monitor its progress
		new TwoPointMonitor().start();
		// Start the thread that will run it
		twoPoint.start();
	}

	private class TwoPointMonitor extends Thread {
		int maxcount = 0;

		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					if (maxcount != twoPoint.maxCount) {
						pBar.setMaximum(twoPoint.maxCount);
						maxcount = twoPoint.maxCount;
					}

					pBar.setValue(twoPoint.count);
				}
			};

			while (twoPoint.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("TwopointMonitor.run(); interrupted.");
				}
			}

			SwingUtilities.invokeLater(r);

			r = new Runnable() {
				public void run() {
					statusLabel.setText("reading results..");
				}
			};
			SwingUtilities.invokeLater(r);

			if (twoPoint.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			setVisible(false);
		}
	}

	private void processResults() {
		File twoPwd; // = new File(Prefs.tools_scratch, "twopoint.pwd");
		File twoOut; // = new File(Prefs.tools_scratch, "twopoint.out");
		File fullOut;

		twoPwd = new File(Prefs.tools_scratch, "twopoint.SNPpwd");
		twoOut = new File(Prefs.tools_scratch, "twopoint.SNPout");
		if (charFullOut.startsWith("y")) {
			fullOut = new File(Prefs.tools_scratch, "twopoint.SNPfullout");
			try {
				order.setTwoPointResults(twoPwd, twoOut, fullOut);
			} catch (Exception e) {
				System.out.println("TwopointSNPDialog.processResults(); interrupted");
			}
		} else {
			try {
				order.setTwoPointResults(twoPwd, twoOut);
			} catch (Exception e) {
				System.out.println("TwopointSNPDialog.processResults(); interrupted");
			}
		}

		ProcessTwoPointSNPResults p = new ProcessTwoPointSNPResults(lGroup, order, twoOut, twoPwd);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("TwopointSNPDialog.processResults(); interrupted");
			}
		}

		isOK = !p.error;

		// If the files were read ok, perform a final check on *what* was read
		// from them
		if (isOK) {
			File file = new File(Project.filename + "_tpFailed.txt");
			// System.out.println("verifyPhase");
			isOK = p.verifyPhaseData(order.getPhasePairArray(), file);
			// System.out.println("verifyPhase done");

		}
	}

	public boolean isOK() {
		return isOK;
	}

	public OrderedResult getOrderedResult() {
		return order;
	}

	private String getExludeDuplicate() {
		String yn = (String) JOptionPane.showInputDialog(this, "Please enter \" yes(y) \" or \" no(n) \" ",
				"Exclude duplicate and near-duplicate markers (y/n)?", 
				JOptionPane.PLAIN_MESSAGE, null, null, "n");

		if (yn != null) {
			try {
				if (!(yn.startsWith("y") || (yn.startsWith("n")))) {
					throw new Exception();
				} else {
					return yn;
				}
			} catch (Exception e) {
				MsgBox.msg("Please ensure that either \"y\" or \"n\" character is entered.", MsgBox.ERR);
			}
		}

		return null;
	}

	private String getFullOutput() {
		String yn = (String) JOptionPane.showInputDialog(this, "Please enter \" yes(y) \" or \" no(n) \" ",
				"Write FULL twopoint output to twopoint.SNPfullout (y/n)?", 
				JOptionPane.PLAIN_MESSAGE, null, null, "n");

		if (yn != null) {
			try {
				if (!(yn.startsWith("y") || (yn.startsWith("n")))) {
					throw new Exception();
				} else {
					return yn;
				}
			} catch (Exception e) {
				MsgBox.msg("Please ensure that either \"y\" or \"n\" character is entered.", MsgBox.ERR);
			}
		}

		return null;
	}

}