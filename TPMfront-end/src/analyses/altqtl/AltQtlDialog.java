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

package analyses.altqtl;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import data.TraitFile;
import data.Trait;
import gui.Prefs;
import gui.exporter.FileWriterQUA;

public class AltQtlDialog extends JDialog {
	/**
	 *  Class for the NON-SNP alternative QTL analysis.
	 *  This is started from the QTL analysis results panel using the green
	 *  'rescan with simple model' icon
	 */
	private static final long serialVersionUID = -6420703089987520401L;
	private TraitFile tFile;
	private int model;
	private int cmLength;
	private String tName;
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	private RunAltQtl runAltQtl;
	private Trait trait;
	
	/**
	 * .
	 */
	public AltQtlDialog(JFrame parent, TraitFile tFile, Trait trait, int model, int cmLength) {
		super(parent, "Running Alternative QTL Model", true);
		this.tFile = tFile;
		this.trait = trait;
		this.model = model;
		this.tName = trait.getName();
		this.cmLength = cmLength;
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running Alternative QTL Model (ALTQTLMODEL)..."), 
				BorderLayout.NORTH);
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
		if (runAltQtl != null) runAltQtl.exit();
		isOK = false;
		setVisible(false);
	}

	private void runPerm() {
		File file = new File(Prefs.tools_scratch, "altqtl.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, tName) == false) {
			exit();
			return;
		}
		
		pBar.setMaximum(cmLength);
	
		// Create FindGeno
		runAltQtl = new RunAltQtl();
		runAltQtl.model = model;
		// Start the thread that will monitor its progress
		new RunAtlQtlMonitor().start();
		// Start the thread that will run it
		runAltQtl.start();		
	}

	private class RunAtlQtlMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runAltQtl.position);
					markerLabel.setText(
						"Chromosome position: " + (runAltQtl.position));
				}
			};
			
			while (runAltQtl.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try { 
					Thread.sleep(50);
				} catch (InterruptedException e) {
					// we don't care if we've been interrupted.
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (runAltQtl.error == false) processResults();
			else isOK = false;
			
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file = new File(Prefs.tools_scratch, "altqtlaltQTL.out");
		ProcessAltQtlResults p = new ProcessAltQtlResults(trait, file);
		
		isOK = p.process();
	}
	
	public boolean isOK() {
		return isOK;
	}
}