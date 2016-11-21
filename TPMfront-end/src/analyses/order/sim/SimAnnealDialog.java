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

package analyses.order.sim;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.OrderedResult;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterPWD;

public class SimAnnealDialog extends JDialog {
	/** SimAnnealDialog. NONSNP only.
	 * 
	 * <p>Controlled by OrderSettingsDialog.
	 * 
	 */
	private static final long serialVersionUID = -3005838177059710097L;
	private LinkageGroup lGroup;
	private OrderedResult order;

	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;

	private RunSimAnneal simAnneal;

	/** SimAnnealDialog(). NONSNP only.
	 * 
	 * @param appFrame = the appFrame
	 * @param ord  = the ordered result (which was created in twopoint, and then passed to 
	Custom, then to Riple and finally here to SimAnneal.
	 * @param grp = the linkageGroup (cloned from selected) that is also passed from one 
	orderAnalysis to the next and finally to us. 
	 */
	public SimAnnealDialog(AppFrame appFrame, OrderedResult ord, LinkageGroup grp) {
		super(appFrame, "Processing Dataset", true);

		lGroup = grp;
		order = ord;

		statusLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		descriptionLabel = new JLabel("Running simulated annealing marker ordering (SIMANNEAL)...");
		p1.add(descriptionLabel, BorderLayout.NORTH);
		p1.add(statusLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runSimAnneal();
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
		if (simAnneal != null) simAnneal.exit();
		setVisible(false);
	}

	private void runSimAnneal() {
		// Write out the file that SimAnneal needs
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "sim.pwd");
		FileWriterPWD writer = new FileWriterPWD(file);

		if (writer.writeData(order, order.getLinkageGroup()) == false) {
			exit();
			return;
		}

		// Clear the results from the custom run
		order.reset();

		// Create SimAnneal
		simAnneal = new RunSimAnneal();
		// Start the thread that will monitor its progress
		new SimAnnealMonitor().start();
		// Start the thread that will run it
		simAnneal.start();
	}

	private class SimAnnealMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					if (simAnneal.temp != -1) {
						statusLabel.setText("Current temperature: " + Prefs.d5.format(simAnneal.temp));
					}
				}
			};

			while (simAnneal.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("SimAnnealMonitor.run(); interrupted");
				}
			}

			SwingUtilities.invokeLater(r);

			if (simAnneal.error == false) {
				processResults();
			} else {
				isOK = false;
			}

			setVisible(false);
		}
	}

	private void processResults() {
		File simTxt = new File(Prefs.tools_scratch, "sim.txt");
		File simOut = new File(Prefs.tools_scratch, "sim.out");

		// GUI results only - don't matter
		try {
			order.setSimAnnealResults(simTxt, simOut);
		} catch (Exception e) {
			System.out.println("SimAnnealDialog.processResults(); setSimAnnealResults() failed");
		}

		ProcessSimResults p = new ProcessSimResults(lGroup, order, simOut);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("SimAnnealDialog.processResults(); interrupted.");
			}
		}

		isOK = !p.error;
	}

	public boolean isOK() {
		return isOK;
	}
}