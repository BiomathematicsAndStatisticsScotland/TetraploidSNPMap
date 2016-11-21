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

package analyses.fastcluster;

import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import javax.swing.JDialog;
import javax.swing.JProgressBar;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.Cluster;
import data.Summary;
import gui.AppFrame;
import gui.Utils;
import gui.Prefs;
import gui.exporter.FileWriterSNPloc;
import doe.MsgBox;

/**
 * This is the Dialog frame appearing when the user selects to run "SNP Clustering Analysis".
 */
public class ClusterDialog extends JDialog {
	private static final long serialVersionUID = -3884781248518238175L;
	// LinkageGroup that is being processed
	private LinkageGroup lGroup;
	// Cluster collection that will result from it
	private Cluster cluster;
	private int numToSend;
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	long start;
	
	private RunChimatrix runChimatrix;
	private RunFastcluster runFastcluster;
	
	/** SNP only.
	 * cluster analysis runs Chimatrix (fortran) and then fastcluster (R).
	 * 
	 */
	public ClusterDialog(AppFrame appFrame, LinkageGroup lGroup, Cluster cluster) {
		super(appFrame, "Processing Dataset", true);
		this.lGroup = lGroup;
		this.cluster = cluster;
		this.numToSend = getNumGroups();
		if (this.numToSend == 0) {
			exit();
			return;
		}
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		if (numToSend < 0) {
			markerLabel = new JLabel("Processing..");
			p1.add(new JLabel(
				"Running slow marker grouping module (MILLIGANCLUSTER)..."), 
				BorderLayout.NORTH);
			pBar.setMaximum(2 * lGroup.getSelectedMarkerCount());
		} else {
			markerLabel = new JLabel("Creating chimatrix..");
			p1.add(new JLabel(
				"Running fast marker grouping module (FASTCLUSTER)..."), 
				BorderLayout.NORTH);
			pBar.setMaximum((lGroup.getSelectedMarkerCount() + 1) 
					* lGroup.getSelectedMarkerCount() / 2);
		}
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
		
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runChimatrix();
			}
			
			public  void windowClosing(WindowEvent e) {
				exit();
			}
		});
		
		pack();
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}
	
	private void exit() {
		if (runChimatrix != null) {
			runChimatrix.exit();
		}
		if (runFastcluster != null) {
			runFastcluster.exit();
		}
		
		isOK = false;
		setVisible(false);
	}
	
	private void runChimatrix() {
		// Write out the files that FindGeno/SNPCluster needs
		Utils.emptyScratch();
		File file;
		file = new File(Prefs.tools_scratch, "cluster.SNPloc");
		FileWriterSNPloc writer = new FileWriterSNPloc(file);
		writer.checkedOnly = true;
		if (writer.writeData(lGroup) == false) {
			exit();
			return;
		}
		
		runChimatrix = new RunChimatrix(
				numToSend, 
				lGroup.getSelectedMarkerCount(), 
				lGroup.getIndividualCount());
		new ChimatrixMonitor().start();
		runChimatrix.start();		
		
	}
	
	private class ChimatrixMonitor extends Thread {		
		public void run() {
			start = System.currentTimeMillis();
			
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runChimatrix.step);
				}
			};
			
			while (runChimatrix.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println(
						"ClusterDialog.ChimatrixMonitor interrupted");
				}
			}
						
			// Final stage...
			
			
			if (runChimatrix.error == false) {
				if (numToSend > 0) {
					runFastcluster();
				} else {
					r = new Runnable() {
						public void run() {
							markerLabel.setText("Analysing dendrograms");
						}
					};
					
					try {
						SwingUtilities.invokeAndWait(r);
					} catch (Exception e) {
						System.out.println("ChimatrixMonitor interrupted 1");
					}
					File f =  new File(Prefs.tools_scratch, "cluster.SNPchi");
					processResults(f);
					cluster.setSummary(
						new Summary(lGroup, (System.currentTimeMillis() - start)));
					
					setVisible(false);
				}
			} else {
				isOK = false;
			}
		}
	}
	
	private void runFastcluster() {
		// Write out the files that FindGeno/SNPCluster needs
		try {
			File file = new File(Prefs.tools_Rscripts_path, "cluster.R");
			File filec = new File(Prefs.tools_scratch, "cluster.R");
			AppFrame.copyInputFile(file, filec);
		} catch (IOException e) {
			System.out.println("ERR: R scripts failed to copy..");
		}
		pBar.setMaximum(100);
		pBar.setValue(0);
		Runnable r = new Runnable() { public void run() {
					markerLabel.setText("Running fastcluster..");
				}
			};
		
		try {
			SwingUtilities.invokeAndWait(r);
		} catch (Exception e) {
			System.out.println("ClusterDialog.runFastcluster interrupted");
		}
		runFastcluster = new RunFastcluster(
				numToSend, 
				lGroup.getSelectedMarkerCount(), 
				lGroup.getIndividualCount());
		new FastclusterMonitor().start();
		runFastcluster.start();
	}
	
	private class FastclusterMonitor extends Thread {		
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runFastcluster.step);
				}
			};
			
			while (runFastcluster.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try { 
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("ClusterDialog.FastclusterMonitor interrupted");
				}
			}
						
			// Final stage...
			r = new Runnable() { public void run() {
						markerLabel.setText("Analysing dendrograms");
					}
				};
			
			try {
				SwingUtilities.invokeAndWait(r);
			} catch (Exception e) {
				System.out.println("ClusterDialog.FastclusterMonitor interrupted 2");
			}
			
			if (runFastcluster.error == false) {
				File f = new File(Prefs.tools_scratch, "cluster.parphen");
				processResults(f);
			} else {
				isOK = false;
			}
			cluster.setSummary(
				new Summary(lGroup, (System.currentTimeMillis() - start)));
			
			setVisible(false);
		}
	}
	
	private void processResults(File file) {
		ProcessClusterResults p =
				new ProcessClusterResults(lGroup, cluster, file);
		
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("ClusterDialog.processResults interrupted");
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