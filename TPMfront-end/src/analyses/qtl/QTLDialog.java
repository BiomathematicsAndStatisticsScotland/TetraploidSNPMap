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

package analyses.qtl;

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
import data.OrderedResult;
import data.QTLResult;
import data.TraitFile;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterDAT;
import gui.exporter.FileWriterMAP;
import gui.exporter.FileWriterQUA;

public class QTLDialog extends JDialog {
	/** QTLDialog. NONSNP only.
	 * 
	 * 
	 */
	private static final long serialVersionUID = -7627363848428411986L;
	private OrderedResult order;
	private TraitFile tFile;

	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;

	private RunQTL runQTL;
	private int currentStage = 1;

	private QTLResult qtlResult = new QTLResult();
	private int maximum;

	/** QTLDialog() NONSNP only.
	 * 
	 * @param appFrame = the appFrame
	 * @param order = the ordered result from runOrdering() (controlled by orderedSettingDialog)
	 * @param tFile = the trait file
	 * @param parent = the chosen parent, 1 or 2
	 */
	public QTLDialog(AppFrame appFrame, OrderedResult order, TraitFile tFile, final int parent) {
		super(appFrame, "Running QTL Analysis", true);
		this.order = order;
		this.tFile = tFile;

		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running interval mapping..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runQTL(parent);
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

	public QTLResult getQTLResult() {
		return qtlResult;
	}

	private void exit() {
		if (runQTL != null) runQTL.exit();
		isOK = false;
		setVisible(false);
	}

	private void runQTL(int parent) {
		// Write out the files that QTL needs
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "qtl.loc");
		FileWriterDAT writerDAT = new FileWriterDAT(file);
		if (writerDAT.writeData(order.getLinkageGroup(), true) == false) {
			exit();
			return;
		}

		file = new File(Prefs.tools_scratch, "qtl.map");
		FileWriterMAP writerMAP = new FileWriterMAP(file);
		if (writerMAP.writeData(order, order.rows) == false) {
			exit();
			return;
		}

		file = new File(Prefs.tools_scratch, "qtl.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, null) == false) {
			exit();
			return;
		}

		maximum = order.getLinkageGroup().getIndividualCount();
		pBar.setMaximum(maximum);

		// Create runQTL
		runQTL = new RunQTL(parent);
		// Start the thread that will monitor its progress
		new QTLMonitor().start();
		// Start the thread that will run it
		runQTL.start();
	}

	private class QTLMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					// Assume changeover has happened
					if (runQTL.stage != currentStage) {
						pBar.setMaximum((int) order.getDistanceTotal());
					}

					pBar.setValue(runQTL.indCount);
					currentStage = runQTL.stage;

					if (currentStage == 1) {
						markerLabel.setText("(Phase 1) Individuals processed: " + (runQTL.indCount));
					} else if (currentStage == 2) {
						markerLabel.setText("(Phase 2) Chromosome position: " + (runQTL.indCount));
					}
				}
			};

			while (runQTL.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("QTLMonitor.run() interrupted.");
				}
			}

			SwingUtilities.invokeLater(r);

			if (runQTL.error == false) {
				processResults();
			} else {
				isOK = false;
			}

			setVisible(false);
		}
	}

	private void processResults() {
		File file = new File(Prefs.tools_scratch, "qtlQTL.out");
		File bkDir = new File(Prefs.tools_scratch, "tempqtl");
		File bkFile = new File(bkDir, "test.txt");
		ProcessQTLResults p = new ProcessQTLResults(qtlResult, file, bkFile);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("QTLDialog.processResults(); interrupted");
			}
		}

		isOK = !p.error;

	}

	public boolean isOK() {
		return isOK;
	}
}