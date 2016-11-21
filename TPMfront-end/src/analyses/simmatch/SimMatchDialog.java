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

package analyses.simmatch;

import java.awt.BorderLayout;
import java.awt.event.WindowEvent;
import java.awt.event.WindowAdapter;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import data.LinkageGroup;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterDAT;

public class SimMatchDialog extends JDialog {
	/** NONSNP only.
	 * called from appFrame() in importDataSet()
	 * 
	 * 
	 */
	private static final long serialVersionUID = 5568045594380162968L;

	private LinkageGroup lGroup;
	
	private JLabel markerLabel;
	private boolean isOK = true;
	
	private RunSimMatch simMatch;
	
	/** SimMatchDialog(). NONSNP only. simplex analysis used upon import of NON SNP dataset.
	 * 
	 * @param appFrame = the appFrame
	 * @param lGroup = the linkagegroup just loaded
	 */
	public SimMatchDialog(AppFrame appFrame, LinkageGroup lGroup) {
		super(appFrame, "Performing Simplex Analysis", true);
		this.lGroup = lGroup;
		
		markerLabel = new JLabel("Performing simplex analysis. Please be patient...");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(markerLabel);
		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runSimMatch();
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
		if (simMatch != null) simMatch.exit();
		
		isOK = false;
		setVisible(false);
	}

	private void runSimMatch() {
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "simmatch.loc");		
		FileWriterDAT writer = new FileWriterDAT(file);
		if (writer.writeData(lGroup, false) == false) {
			exit();
			return;
		}
		
		// Create FindGeno
		simMatch = new RunSimMatch();
		// Start the thread that will monitor its progress
		new SimMatchMonitor().start();
		// Start the thread that will run it
		simMatch.start();		
	}

	private class SimMatchMonitor extends Thread {

		public void run() {
			while (simMatch.isRunning) {
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("SimmatchMonitor.run(); interrupted..");
				}
			}
			
			if (simMatch.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			
			setVisible(false);
		}
	}
	
	private void processResults() {
		File nFile = new File(Prefs.tools_scratch, "simmatch.nam");
		File pFile = new File(Prefs.tools_scratch, "simmatch.p1r");
		ProcessResults p = new ProcessResults(lGroup, nFile, pFile);
		
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("simmatchMonitor.processResults(); interrupted.");
			}
		}
		
		isOK = !p.error;
	}
	
	public boolean isOK() {
		return isOK;
	}
}