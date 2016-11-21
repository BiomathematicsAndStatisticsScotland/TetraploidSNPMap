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

package analyses.snpperm;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

import data.OrderedResult;
import data.PermResult;
import data.TraitFile;
import doe.MsgBox;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterQUA;

public class SNPPermDialog extends JDialog {
	/** SNPPermDialog. SNP Only. does the permutation test for the selected trait.
	 * runPerm will run 1 process per available core. 
	 * 
	 */
	private static final long serialVersionUID = -3527910035150281635L;
	private TraitFile tFile;
	private String tName;
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	private RunSNPPerm runPerm;
	private PermResult permResult;
	private int numperms;
	private int seed;
	private OrderedResult order;
	
	/** called from QTLResultsPanel.
	 * 
	 * @param parent = MsgBox.frm
	 * @param tFile = traitfile
	 * @param tName  = trait Name (for FileWriterQUA to select the trait to write)
	 */
	public SNPPermDialog(JFrame parent, TraitFile tFile, String tName, OrderedResult order) {
		super(parent, "Running Permutation Test", true);
		this.tFile = tFile;
		this.order = order;
		this.tName = tName;
		numperms = getNumPerms();
		if (numperms == 0) return;
		seed = getSeed();
		if (seed == -1) return;
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		pBar.setMaximum(numperms);
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running permutation test module (PERM)..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runPerm();
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
		if (runPerm != null) runPerm.exit();
		isOK = false;
		setVisible(false);
	}

	private void runPerm() {
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "perm.maploc");
		try {
			order.WriteMaploc(file);
		} catch (Exception e) {
			System.out.println("can't write the maplocfile");
			return;
		}
		file = new File(Prefs.tools_scratch, "perm.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, tName) == false) {
			exit();
			return;
		}
		markerLabel.setText("Stage: 1/2");
		runPerm = new RunSNPPerm(numperms, seed);
		pBar.setMaximum(runPerm.numperms);
		// Start the thread that will monitor its progress
		new RunPermMonitor().start();
		// Start the thread that will run it
		runPerm.start();		
	}

	private class RunPermMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runPerm.totalposition());
					markerLabel.setText("processes: " + runPerm.activeprocs);
				}
			};
			
			while (runPerm.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RunPermMonitor.run(); interrupted..");
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (runPerm.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			
			setVisible(false);
		}
	}
	
	private void processResults() {
		permResult = new PermResult(runPerm.numperms);
		for (int c = 0; c < runPerm.cores; c++) {
			for (int i = 0; i < runPerm.numpermseach; i++) {
				permResult.lodScores[c * runPerm.numpermseach + i] = runPerm.results.get(c).get(i);
			}
		}
		permResult.calcSigScores();
	}
	
	public boolean isOK() {
		return isOK;
	}
	
	public PermResult getResult() {
		return permResult;
	}
	
	private int getNumPerms() {
		int value = 0;
		int maxperms;
		if (Runtime.getRuntime().availableProcessors() == 1) {
			maxperms = 500;
		} else {
			maxperms = 1000;
		}
		while (true) {
			String num = (String) JOptionPane.showInputDialog(this, "Enter the "
					+ "number of permutations (100-" + maxperms + "):", "Number of permutations",
					JOptionPane.PLAIN_MESSAGE, null, null, "" + maxperms);
			if (num == null) return 0;
			try {
				value = Integer.parseInt(num);
				if (value >= 100 && value <= maxperms) {
					return value;
				} else {
					MsgBox.msg("Please enter a number between 100 and " + maxperms + ".",
							MsgBox.ERR);
				}
			} catch (NumberFormatException e) {
				MsgBox.msg("Please ensure a number between 100 and " + maxperms + ".",
							MsgBox.ERR);
			}
		}
	}
	
	private int getSeed() {
		int value = 0;
		while (true) {
			String num = (String) JOptionPane.showInputDialog(this, "Enter the "
					+ "seed:", "Seed",
					JOptionPane.PLAIN_MESSAGE, null, null, "" + (int) (Math.random() * 2147483646));
			if (num == null) return -1;
			try {
				value = Integer.parseInt(num);
				if (value > 0 && value <= 2147483646) {
					return value;
				} else {
					MsgBox.msg("Please enter a positive integer.",
							MsgBox.ERR);
				}
			} catch (NumberFormatException e) {
				MsgBox.msg("Please ensure a positive integer.",
						MsgBox.ERR);
			}
		}
	}
}