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

package analyses.perm;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.PermResult;
import data.TraitFile;
import gui.Prefs;
import gui.exporter.FileWriterQUA;

public class PermDialog extends JDialog {
	/** PermDialog. NONSNP only. Permutation test for QTL.
	 * 
	 */
	private static final long serialVersionUID = 6126400246277218625L;
	private TraitFile tFile;
	private int cmLength;
	private String tName;
	private boolean fullModel;

	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;

	private RunPerm runPerm;
	private PermResult permResult;

	/** PermDialog(parent, tFile, fullModel, tName, cmLength) NONSNP only.
	 * 
	 * @param parent = MsgBox.frm
	 * @param tFile = the trait file
	 * @param fullModel  = boolean - if true: run tools_perm_path; if false: tools_permsimp_path 
	 * @param tName = the selected trait name
	 * @param cmLength = the total chromosome length, for the progressbar maximum.
	 */
	public PermDialog(JFrame parent, TraitFile tFile, boolean fullModel, String tName, int cmLength) {
		super(parent, "Running Permutation Test", true);
		this.tFile = tFile;
		this.fullModel = fullModel;
		this.tName = tName;
		this.cmLength = cmLength;

		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
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
		// Write out the file that FindGeno needs
		// Utils.emptyScratch();
		File file = new File(Prefs.tools_scratch, "perm.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, tName) == false) {
			exit();
			return;
		}

		pBar.setMaximum(cmLength);

		// Create FindGeno
		runPerm = new RunPerm();
		runPerm.fullModel = fullModel;
		// Start the thread that will monitor its progress
		new RunPermMonitor().start();
		// Start the thread that will run it
		runPerm.start();
	}

	private class RunPermMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runPerm.position);
					markerLabel.setText("Chromosome position: " + (runPerm.position));
				}
			};

			while (runPerm.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RunPermMonitor.run(); interrupted.");
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
		permResult = new PermResult();

		File file = new File(Prefs.tools_scratch, "permQTL.out");
		ProcessPermResults p = new ProcessPermResults(permResult, file);

		isOK = p.process();
	}

	public boolean isOK() {
		return isOK;
	}

	public PermResult getResult() {
		return permResult;
	}
}