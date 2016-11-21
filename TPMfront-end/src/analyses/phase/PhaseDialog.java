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

package analyses.phase;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.OrderedResult;
import gui.Prefs;
import gui.Utils;

public class PhaseDialog extends JDialog {
	/** PhaseDialog. SNP only.
	 * 
	 */
	private static final long serialVersionUID = -1859091809677801521L;
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;

	private RunPhase runPhase;
	private OrderedResult twopointresult, mdsresult, myresult;

	/** PhaseDialog. SNP only.
	 * 
	 * 
	 * @param parent = the appFrame
	 * @param mdsresult = result from MDS (for _nophase.map; other files aren't used)
	 * @param twopointresult = result from twopoint (for SNPpwd; SNPout is written but not used)
	 */
	public PhaseDialog(JFrame parent, OrderedResult mdsresult, OrderedResult twopointresult) {
		super(parent, "Running Phase program", true);
		this.twopointresult = twopointresult;
		this.mdsresult = mdsresult;
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		pBar.setMaximum(8);
		markerLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running Phase program"), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runPhase();
			}

			public void windowClosing(WindowEvent e) {
				exit();
			}
		});

		pack();
		setLocationRelativeTo(parent);
		setResizable(false);
		setVisible(true);
	}

	private void exit() {
		if (runPhase != null) runPhase.exit();

		isOK = false;
		setVisible(false);
	}

	private void runPhase() {
		// Write out the file that FindGeno needs
		Utils.emptyScratch();

		File f1 = new File(Prefs.tools_scratch, "twopoint.SNPpwd");
		File f2 = new File(Prefs.tools_scratch, "twopoint.SNPout");
		File phasemap = new File(Prefs.tools_scratch, "twopoint_nophase.map");
		File locikey = new File(Prefs.tools_scratch, "twopoint_pcconfigurationlocikey.txt");
		File estimatedmap = new File(Prefs.tools_scratch, "twopoint_estimatedmap.txt");
		try {
			twopointresult.WriteTwoPointResults(f1, f2);
			mdsresult.writeMDSResults(phasemap, locikey, estimatedmap);
		} catch (Exception e) {
			System.out.println("can't write twopoint or MDS result files needed for Phase" + e);
		}

		runPhase = new RunPhase();

		// Start the thread that will monitor its progress
		new RunPhaseMonitor().start();
		// Start the thread that will run it
		runPhase.start();
	}

	private class RunPhaseMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runPhase.position);
					// markerLabel.setText(
					// "Chromosome position: " + (runPhase.position));
				}
			};

			while (runPhase.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RunPhaseMonitor.run() interrupted..");
				}
			}

			SwingUtilities.invokeLater(r);

			if (runPhase.error == false) {
				processResults();
			} else {
				isOK = false;
			}

			setVisible(false);
		}
	}

	private void processResults() {
		File file = new File(Prefs.tools_scratch, "twopoint_qhase.map");
		ProcessPhaseResults p = new ProcessPhaseResults(mdsresult, file);
		p.start();
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("PhaseDialog.processResults(); interrupted.");
			}
		}

		isOK = !p.error;
		if (isOK) this.myresult = p.getResult();
	}

	public boolean isOK() {
		return isOK;
	}

	public OrderedResult getResult() {
		return myresult;
	}
}