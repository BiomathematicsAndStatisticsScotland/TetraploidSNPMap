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

package analyses.order.custom;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.OrderedResult;
import gui.AppFrame;

public class CustomDialog extends JDialog {
	/** CustomDialog. NONSNP only. controlled by AppFrame.runOrdering() and OrderSettingsDialog.
	 * 
	 */
	private static final long serialVersionUID = 6345902885189030461L;
	private LinkageGroup lGroup;
	private OrderedResult order;
	private int num = 0;

	private JProgressBar pBar;
	private JLabel descriptionLabel, statusLabel;
	private boolean isOK = true;

	private CustomAnalysis getOrder;

	/** CustomDialog(). NONSNP only.
	 * 
	 * @param appFrame = the appFrame
	 * @param ord = the OrderedResult created by twopointDialog that gets passed to all orderings.
	 * @param grp = the linkageGroup that gets passed to all orderings.
	 */
	public CustomDialog(AppFrame appFrame, OrderedResult ord, LinkageGroup grp) {
		super(appFrame, "Processing Dataset", true);

		order = ord;
		lGroup = grp;
		num = lGroup.getMarkerCount();

		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		pBar.setMaximum(num);
		statusLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		descriptionLabel = new JLabel("Running marker ordering module (CUSTOM)...");
		p1.add(descriptionLabel, BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(statusLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runCustom();
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
		if (getOrder != null) getOrder.exit();
		setVisible(false);
	}

	private void runCustom() {
		// Create the thread
		getOrder = new CustomAnalysis(order, lGroup);
		// Start the thread that will monitor its progress
		new GetOrderMonitor().start();
		// Start the thread that will run it
		getOrder.start();
	}

	private class GetOrderMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(getOrder.mkrCount);
					statusLabel.setText("Processing order " + (getOrder.mkrCount + 1) + " of " + num);
				}
			};

			while (getOrder.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("GetOrderMonitor.run() interrupted.");
				}
			}

			SwingUtilities.invokeLater(r);

			setVisible(false);
		}
	}

	public boolean isOK() {
		return isOK;
	}
}