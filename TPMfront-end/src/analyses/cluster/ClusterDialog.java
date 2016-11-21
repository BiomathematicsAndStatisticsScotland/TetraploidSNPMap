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

package analyses.cluster;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JProgressBar;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.Cluster;
import data.Summary;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterDAT;
import doe.MsgBox;

public class ClusterDialog extends JDialog {
	/**
	 * This is the Dialog frame appearing when the user selects to run 
	 * "Clustering Analysis" in NON SNP mode.
	 */
	private static final long serialVersionUID = -3884781248518238175L;
	// LinkageGroup that is being processed
	private LinkageGroup lGroup;
	// Cluster collection that will result from it
	private Cluster cluster;
	// Clustering method to be run
	private int method;
	
	// Number sent to CLUSTER (either number of groups or threshold recom freq)
	private float numToSend;	
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	
	private RunCluster runCluster;

	/** argument 'method' will be passed to the fortran, but it should just be '2'.
	 * 
	 */
	public ClusterDialog(AppFrame appFrame, LinkageGroup lGroup, Cluster cluster, int method) {
		super(appFrame, "Processing Dataset", true);
		this.lGroup = lGroup;
		this.cluster = cluster;
		this.method = method;
		
		// Before doing anything, get the number of groups from the user
		if (method == 2) {
			this.numToSend = getNumGroups();
		}
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running marker grouping module (CLUSTER)..."), 
				BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runCluster();
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
		if (runCluster != null) {
			runCluster.exit();
		}
		
		isOK = false;
		setVisible(false);
	}
	
	private void runCluster() {
		// Write out the file that 'cluster' needs
		Utils.emptyScratch();
		File file;
		file = new File(Prefs.tools_scratch, "cluster.dat");		
		FileWriterDAT writer = new FileWriterDAT(file);
		if (writer.writeData(lGroup, true) == false) {
			exit();
			return;
		}
		
		pBar.setMaximum(2 * lGroup.getSelectedMarkerCount());
	
		// Create Cluster
		runCluster = new RunCluster(numToSend, method);
		// Start the thread that will monitor its progress
		new ClusterMonitor().start();
		
		// Start the thread that will run it
		runCluster.start();		
		
	}
	
	private class ClusterMonitor extends Thread {
		//private int max;
		public void run() {
			final long start = System.currentTimeMillis();
			
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runCluster.position);
				}
			};
			
			while (runCluster.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("You interrupted the "
							+ "sleep of the ClusterMonitor!");
				}
			}
						
			// Final stage...
			r = new Runnable() {
				public void run() {
					markerLabel.setText("Analysing dendrograms");
				}
			};
			
			try {
				SwingUtilities.invokeAndWait(r);
			} catch (Exception e) {
				System.out.println("Analysing dendograms..");
			}
			
			if (runCluster.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			
			cluster.setSummary(
				new Summary(lGroup, (System.currentTimeMillis() - start)));
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file;
		file = new File(Prefs.tools_scratch, "cluster.chi");
		ProcessClusterResults p =
			new ProcessClusterResults(lGroup, cluster, file, method);
		
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("You interrupted the "
						+ "sleep of the Cluster ProcessResults!");
			}
		}
		
		isOK = !p.error;
	}
	
	public boolean isOK() {
		return isOK;
	}
	
	private int getNumGroups() {
		String num = (String) JOptionPane.showInputDialog(this, "Enter the "
			+ "expected number of linkage groups:", "Enter Group Count",
			JOptionPane.PLAIN_MESSAGE, null, null, "" + 12);
		
		if (num != null) {
			try {
				int value = Integer.parseInt(num);
				return value;
			} catch (NumberFormatException e) {
				MsgBox.msg("Please ensure a positive integer value is entered.",
					MsgBox.ERR);
			}
		}
		
		return 0;
	}
}