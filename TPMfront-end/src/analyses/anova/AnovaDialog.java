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

package analyses.anova;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import java.io.File;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JDialog;
import data.LinkageGroup;
import data.TraitFile;
import data.AnovaResult;
import gui.AppFrame;
import gui.Utils;
import gui.Prefs;
import gui.exporter.FileWriterDAT;
import gui.exporter.FileWriterQUA;

public class AnovaDialog extends JDialog {
	/**
	 *  NON SNP only.
	 */
	private static final long serialVersionUID = -1229241675319760835L;
	private LinkageGroup lGroup;
	private TraitFile tFile;
	
	private boolean isOK = true;
	
	private RunAnova runAnova;
	private AnovaResult results = new AnovaResult();
	
	/** NON SNP only.
	 * 
	 */
	public AnovaDialog(AppFrame appFrame, LinkageGroup lGroup, TraitFile tFile) {
		super(appFrame, "Running ANOVA", true);
		this.lGroup = lGroup;
		this.tFile = tFile;
				
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running ANOVA module...please be patient."));		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runAnova();
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
	
	public AnovaResult getAnovaResults() {
		return results;
	}
		
	private void exit() {
		if (runAnova != null) {
			runAnova.exit();
		}
		
		isOK = false;
		setVisible(false);
	}

	private void runAnova() {
		// Write out the files that QTL needs
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "anova.loc");
		FileWriterDAT writerDAT = new FileWriterDAT(file);
		if (writerDAT.writeData(lGroup, true) == false) {
			exit();
			return;
		}
		
		file = new File(Prefs.tools_scratch, "anova.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, null) == false) {
			exit();
			return;
		}
		
	
		// Create runAnova
		runAnova = new RunAnova();
		// Start the thread that will monitor its progress
		new AnovaMonitor().start();
		// Start the thread that will run it
		runAnova.start();		
	}

	private class AnovaMonitor extends Thread {
		public void run() {
			while (runAnova.isRunning) {
				try { 
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println(
						"You interupted the sleep of the Anovamonitor!");
				}
			}
									
			if (runAnova.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file = new File(Prefs.tools_scratch, "anova.aov");
		ProcessAnovaResults p = new ProcessAnovaResults(file, results, lGroup);
		
		isOK = !p.error;
	}
	
	public boolean isOK() {
		return isOK;
	}
}