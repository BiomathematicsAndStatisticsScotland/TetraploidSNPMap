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

package analyses.order.ripple;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import data.LinkageGroup;
import data.OrderedResult;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterPWD;

public class RippleDialog extends JDialog {
	/** RippleDialog. NON SNP only. controlled from OrderSettingsDialog.
	 * 
	 */
	private static final long serialVersionUID = -1348312730894825098L;
	private LinkageGroup lGroup;
	private OrderedResult order;

	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;

	private RunRipple ripple;

	/** RippleDialog(). NON SNP only. controlled from OrderSettingsDialog.
	 * 
	 * @param appFrame = the appFrame
	 * @param ord  = the ordered result which was created in twopoint and gets passed 
	to each ordered analysis in turn.
	 * @param grp = the clone of selected linkage group, gets passed to each ordered analyses.
	 */
	public RippleDialog(AppFrame appFrame, OrderedResult ord, LinkageGroup grp) {
		super(appFrame, "Processing Dataset", true);

		lGroup = grp;
		order = ord;

		statusLabel = new JLabel("");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		descriptionLabel = new JLabel("Running ripple marker ordering (RIPPLE)...");
		p1.add(descriptionLabel, BorderLayout.NORTH);
		p1.add(statusLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runRipple();
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
		if (ripple != null) ripple.exit();
		setVisible(false);
	}

	private void runRipple() {
		// Write out the file that Ripple needs
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "ripple.pwd");

		FileWriterPWD writer = new FileWriterPWD(file);
		if (writer.writeData(order, order.getLinkageGroup()) == false) {
			exit();
			return;
		}

		// Clear the results from the custom run
		order.reset();

		// Create Ripple
		ripple = new RunRipple();
		// Start the thread that will monitor its progress
		new RippleMonitor().start();
		// Start the thread that will run it
		ripple.start();
	}

	private class RippleMonitor extends Thread {
		public void run() {
			/*
			 * Runnable r = new Runnable() { public void run() { if (ripple.temp
			 * != -1) statusLabel.setText("Current temperature: " +
			 * Prefs.d5.format(ripple.temp)); } };
			 */
			while (ripple.isRunning) {
				// SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RippleMonitor.run(); interrupted..");
				}
			}

			if (ripple.error == false) {
				processResults();
			} else {
				isOK = false;
			}

			setVisible(false);
		}
	}

	private void processResults() {
		File rippler = new File(Prefs.tools_scratch, "rippler.txt");
		ProcessRippleResults p = new ProcessRippleResults(lGroup, order, rippler);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("RippleDialog.processResults(); interrupted");
			}
		}

		isOK = !p.error;
	}

	public boolean isOK() {
		return isOK;
	}
}