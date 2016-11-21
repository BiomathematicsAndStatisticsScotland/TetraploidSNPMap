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

package analyses.findgeno;

import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterDAT;

public class FindGenoDialog extends JDialog {
	/** NON SNP only. this performs the initial analysis when importing the dataset.
	 * 
	 */
	private static final long serialVersionUID = 7327204993214913263L;

	private LinkageGroup lGroup;
	
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	
	private RunFindGeno findGeno;
	
	/** NON SNP only. 
	 * 
	 */
	public FindGenoDialog(AppFrame appFrame, LinkageGroup lGroup) {
		super(appFrame, "Importing and Processing Dataset", true);
		this.lGroup = lGroup;
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running genotype inference module (FINDGENO)..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runFindGeno();
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
		if (findGeno != null) {
			findGeno.exit();
		}
		
		isOK = false;
		setVisible(false);
	}

	private void runFindGeno() {
		Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "findgeno.dat");		
		FileWriterDAT writer = new FileWriterDAT(file);
		if (writer.writeData(lGroup, false) == false) {
			exit();
			return;
		}
		
		pBar.setMaximum(lGroup.getMarkerCount());
	
		// Create FindGeno
		findGeno = new RunFindGeno();
		// Start the thread that will monitor its progress
		new FindGenoMonitor().start();
		// Start the thread that will run it
		findGeno.start();		
	}

	private class FindGenoMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(findGeno.locusCount);
					markerLabel.setText(
						"Processing " + lGroup.getMarkerName(findGeno.marker));
				}
			};
			
			while (findGeno.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try { 
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("FindGenoDialog.Findgenomonitor interrupted");
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (findGeno.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file = new File(Prefs.tools_scratch, "findgeno.seg");
		ProcessResults p = new ProcessResults(lGroup, file);
		
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("FindGenoDialog.processResults interrupted");
			}
		}
		
		isOK = !p.error;
	}
	
	public boolean isOK() {
		return isOK;
	}
}