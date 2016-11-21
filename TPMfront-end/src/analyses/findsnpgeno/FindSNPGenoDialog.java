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

package analyses.findsnpgeno;

import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterSNPloc;
import analyses.findsnpgeno.RunFindSNPGeno;
import analyses.findsnpgeno.ProcessSNPResults;
import java.lang.Thread;
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
import doe.MsgBox;


/**
 * FindSNPGenoDialog is a modified clone of FindGenoDialog class
 * used to calculate summary statistics for the dosage SNP markers.
 * To achive this it uses a different FORTRAN binary from findgeno.exe
 * ALERT: This new binary is now called ReadingTPM.exe but a better naming
 * convention could be used such as snp_findgeno.exe
 * 
 * <P>The previous FindGenoDialog class will continue to be used for the
 * old standard presence/absence allele markers (AFLP, SNP, ..)
 *  
 */
public class FindSNPGenoDialog extends JDialog {
	static final long serialVersionUID = 5761373833280949L;
	private LinkageGroup lGroup;
	
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	private AppFrame appFrame;
	
	private RunFindSNPGeno findSNPGeno;
	
	/** FindSNPGenoDialog(appframe, linkagegroup) - linkagegroup comes from ImportSNPFile().
	 * Initially a FindSNOGenoDialog instance is created using the original 
	 * constructor of the JDialog class with super(Frame, String, boolean)
	 * 
	 */
	public FindSNPGenoDialog(AppFrame appFrame, LinkageGroup lGroup) {
		super(appFrame, "Importing and Processing Dataset", true);
		this.lGroup = lGroup;
		this.appFrame = appFrame;
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running genotype inference module (READINGTPM)..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
				
		/*
		 * This is an automation that does the following:
		 * - adds a WindowListener to this instance of FindSNPGenoDialog
		 * - this WindowListener/WindowAdapter automatically runs this.runFindSNPGeno() method
		 * - all this creates the visual effect of displaying the progress bar and a sequence of messages
		 */
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runFindSNPGeno();
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
		if (findSNPGeno != null) {
			findSNPGeno.exit();
		}
		
		isOK = false;
		setVisible(false);
	}

	private void runFindSNPGeno() {
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "ReadingTPM.loc");
		FileWriterSNPloc writer = new FileWriterSNPloc(file);
		writer.safeNameNrs = true;
		if (writer.writeData(lGroup) == false) {
			MsgBox.msg("can't write ReadingTPM.loc", MsgBox.ERR);
			exit();
			return;
		}
		
		pBar.setMaximum(lGroup.getMarkerCount());
		
		findSNPGeno = new RunFindSNPGeno();
		new FindSNPGenoMonitor().start();
		findSNPGeno.start();	
	}

	private class FindSNPGenoMonitor extends Thread {
		private int max = 0;
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					if (findSNPGeno.total > max) {
						pBar.setMaximum(findSNPGeno.total);
						max = findSNPGeno.total;
					}
					pBar.setValue(findSNPGeno.position);
				}
			};
			
			while (findSNPGeno.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("FindSNPGenoDialog.FindSNPGenoMonitor interrupted..");
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (findSNPGeno.error == false) {
				processSNPResults();
			} else {
				String err = "FindSNPGeno - fortran error. readingtpm = SNPmatch_noimsl.exe";
				System.out.println(err);
				MsgBox.msg("Critical error:" + err, MsgBox.ERR);						
				isOK = false;
			}
			setVisible(false);
		}
	}
	
	private void processSNPResults() {
		markerLabel.setText("Reading in the results..");
		File file = new File(Prefs.tools_scratch, "ReadingTPM.out");
		ProcessSNPResults p = new ProcessSNPResults(appFrame, lGroup, file);
		pBar.setValue(0);
		pBar.setMaximum(7);
		
		Runnable r = new Runnable() {
			public void run() {
				pBar.setValue(p.parsestep);
				markerLabel.setText(p.parsestatus);
			}
		};
		while (p.isRunning) {
			SwingUtilities.invokeLater(r);
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("FindSNPGenoDialog.processSNPResults interrupted..");
			}
		}
		
		isOK = !p.error;
	}
	
	public boolean isOK() {
		return isOK;
	}
}
