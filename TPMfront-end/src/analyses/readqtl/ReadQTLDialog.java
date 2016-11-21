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

package analyses.readqtl;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.LinkageGroup;
import data.OrderedResult;
import data.TraitFile;
import gui.AppFrame;
import gui.Prefs;
import gui.Utils;
import gui.exporter.FileWriterQUA;
import gui.exporter.FileWriterSNPloc;

public class ReadQTLDialog extends JDialog {
	/** readQTLDialog() SNP Only.
	 *  run to combine markers, map and traits into an orderedResult of type READQTL.
	 * 
	 */
	private static final long serialVersionUID = 879573495757083184L;
	private OrderedResult order;
	private TraitFile tFile;
	private File mapFile;
	private LinkageGroup lGroup;
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	private RunReadQTL runReadQTL;

	/** ReadQTLDialog() SNP Only.
	 * 
	 * @param appFrame = the appFrame
	 * @param tFile  = the trait file
	 * @param lGroup  = the lGroup from reading the SNPloc file.
	 */
	public ReadQTLDialog(AppFrame appFrame, TraitFile tFile, File mapFile, LinkageGroup lGroup) {
		super(appFrame, "reading markers, map, and traits.", true);

		this.tFile = tFile;
		this.mapFile = mapFile;
		this.lGroup = lGroup;
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");

		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("reading markers, map, and traits..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);

		add(p1);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runReadQTL();
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
		if (runReadQTL != null) runReadQTL.exit();

		isOK = false;
		setVisible(false);
	}

	private void runReadQTL() {
		// Write out the files that QTL needs
		// we need SNPloc, MAP and qua.
		// qua = tFile
		// MAP = mFile - has already been copied to TMPDIR/read_qtl.map
		// SNPloc must be written from lGroup
		Utils.emptyScratch();
		File fileclone = new File(Prefs.tools_scratch, "read_qtl.map");
		try {
			AppFrame.copyInputFile(this.mapFile, fileclone);
		} catch (IOException e) {
			System.out.println("ERR: The Input File has FAILED to get copied in /tpmap");
			return;
		}
		File file = new File(Prefs.tools_scratch, "read_qtl.SNPloc");
		FileWriterSNPloc writer = new FileWriterSNPloc(file);
		writer.checkedOnly = true;
		if (writer.writeData(lGroup) == false) {
			exit();
			return;
		}
		file = new File(Prefs.tools_scratch, "read_qtl.qua");
		FileWriterQUA writerQUA = new FileWriterQUA(file);
		if (writerQUA.writeData(tFile, null) == false) {
			exit();
			return;
		}

		// Create runReadQTL
		runReadQTL = new RunReadQTL();
		// Start the thread that will monitor its progress
		new ReadQTLMonitor().start();
		// Start the thread that will run it
		runReadQTL.start();
	}

	private class ReadQTLMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
				}
			};

			while (runReadQTL.isRunning) {
				SwingUtilities.invokeLater(r);

				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("ReadQTLMonitor.run(); interrupted..");
				}
			}

			SwingUtilities.invokeLater(r);

			if (runReadQTL.error == false) {
				processResults();
			} else {
				isOK = false;
			}

			setVisible(false);
		}
	}

	public OrderedResult getResult() {
		return order;
	}

	private void processResults() {
		File maplocfile = new File(Prefs.tools_scratch, "read_qtl.maploc");
		File otherfile = new File(Prefs.tools_scratch, "read_qtl_read_qtlreadQTL.out");
		ProcessReadQTLResults p = new ProcessReadQTLResults(maplocfile, otherfile, lGroup);

		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("ReadQTLDialog.processResults() interrupted.");
			}
		}

		isOK = !p.error;
		order = p.getResult();

	}

	public boolean isOK() {
		return isOK;
	}
}