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

package analyses.order.twopoint;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.OrderedResult;
import gui.AppFrame;
import gui.Prefs;
import gui.Project;
import gui.Utils;
import gui.exporter.FileWriterDAT;

public class TwoPointDialog extends JDialog {
	/** TwoPointDialog. NON SNP only.
	 * 
	 *  <p>this runs the twopoint ordering, controlled by OrderSettingsDialog.
	 */
	private static final long serialVersionUID = -7559234338595276366L;
	private LinkageGroup lGroup;
	private OrderedResult order;
	private int num = 0;

	private JProgressBar pBar;
	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;

	private RunTwoPoint twoPoint;

	/** TwopointDialog. NON SNP only.
	 * this runs the twopoint ordering, controlled by OrderSettingsDialog
	 * 
	 * @param appFrame  = the appFrame
	 * @param grp  = the linkage group to order.
	 */
	public TwoPointDialog(AppFrame appFrame, LinkageGroup grp) {
		super(appFrame, "Processing Dataset", true);

		lGroup = grp;
		num = lGroup.getMarkerCount();
		order = new OrderedResult(num);

		if (ProcessTwoPointResults.verifyForTwoPoint(lGroup) == false) {
			isOK = false;
			return;
		}

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
				// System.out.println("twopointdialog.runtwopoint()");
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
		file = new File(Prefs.tools_scratch, "twopoint.loc");
		FileWriterDAT writer = new FileWriterDAT(file);

		if (writer.writeData(lGroup, true) == false) {
			// System.out.println("runtwopoint exit fail to write .loc");
			exit();
			return;
		}

		// For 5 markers, TP runs the following iterations:
		// 4 + 3 + 2 + 1 (n-1) * (n/2)
		int markerCount = lGroup.getSelectedMarkerCount();
		int runs = (int) ((markerCount - 1) * ((float) markerCount / 2.0));
		// We do *2 because TP prints out L1 and L2 markers at each iteration
		pBar.setMaximum(2 * runs);

		// Create TwoPoint
		twoPoint = new RunTwoPoint();
		// Start the thread that will monitor its progress
		new TwoPointMonitor().start();
		// Start the thread that will run it
		twoPoint.start();
	}

	private class TwoPointMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(twoPoint.count);
					statusLabel.setText("Processing " + lGroup.getMarkerName(twoPoint.marker1) + " and "
							+ lGroup.getMarkerName(twoPoint.marker2));
				}
			};

			while (twoPoint.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("TwopointDialog.run(); interrupted");
				}
			}

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

		twoPwd = new File(Prefs.tools_scratch, "twopoint.pwd");
		twoOut = new File(Prefs.tools_scratch, "twopoint.out");

		// GUI results only - don't matter
		try {
			order.setTwoPointResults(twoPwd, twoOut);
		} catch (Exception e) {
			System.out.println("TwopointDialog.processResults(); interrupted");
		}

		ProcessTwoPointResults p = new ProcessTwoPointResults(lGroup, order, twoOut, twoPwd);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("TwopointDialog.processResults(); interrupted");
			}
		}

		isOK = !p.error;

		// If the files were read ok, perform a final check on *what* was read
		// from them
		if (isOK) {
			File file = new File(Project.filename + "_tpFailed.txt");
			isOK = p.verifyPhaseData(order.getPhasePairArray(), file);

		}
	}

	public boolean isOK() {
		return isOK;
	}

	public OrderedResult getOrderedResult() {
		return order;
	}

}