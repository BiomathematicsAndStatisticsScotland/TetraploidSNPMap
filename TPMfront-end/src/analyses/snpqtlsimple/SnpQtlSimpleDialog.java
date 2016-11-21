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

package analyses.snpqtlsimple;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Vector;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;
import data.QTLResult;
import data.Trait;
import gui.Prefs;
import gui.Utils;

public class SnpQtlSimpleDialog extends JDialog {
	/** SNP only. QTL simple model. 
	 *  first time runs 'simplemodel' (without choosing 2 alleles), second time it runs
	 *  simplemodeladditive (choosing allele 1 and 2).
	 */
	private static final long serialVersionUID = -6420703089987520401L;
	private int cmLength;
	
	private JProgressBar pBar;
	private JLabel markerLabel;
	private boolean isOK = true;
	private int allele1 = -1, allele2;
	
	
	private RunSnpQtlSimple runSimple;
	private int traitI;
	private Trait currentTrait;
	private QTLResult qtlResult;
	
	/** SnpQtlSimpleDialog. 
	 * 
	 * @param parent = MsgBox.frm
	 * @param traitI = unfortunately we need to pass the trait object as well as the
	 * 				   trait index in the traitFile
	 * @param currentTrait = the trait object
	 * @param cmLength = the chromosome length
	 * @param qtlResult = the QTL result, which contains the qmm data to write to simple.qmm
	 */
	public SnpQtlSimpleDialog(JFrame parent, int traitI, 
			Trait currentTrait, int cmLength, QTLResult qtlResult) {
		super(parent, "Running QTL Simple Model", true);
		this.traitI = traitI;
		this.cmLength = cmLength;
		this.currentTrait = currentTrait;
		this.qtlResult = qtlResult;
		
		if (currentTrait.getBestSimpleModels().size() != 0) {
			get_alleles();
		}
		
		
		pBar = new JProgressBar();
		pBar.setPreferredSize(new Dimension(300, 20));
		markerLabel = new JLabel("Processing");
		
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p1.add(new JLabel("Running Alternative QTL Model (ALTQTLMODEL)..."), BorderLayout.NORTH);
		p1.add(pBar, BorderLayout.CENTER);
		p1.add(markerLabel, BorderLayout.SOUTH);
		
		add(p1);
				
		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				runSimple();
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
	
	private void get_alleles() {
		Vector<Object> possibleValues = new Vector<Object>();
		for (int i = 1; i < 9; i++) possibleValues.addElement(i);
		Object selectedValue = JOptionPane.showInputDialog(null,
		            "Choose first allele", "Input",
		             JOptionPane.INFORMATION_MESSAGE, null,
		             possibleValues.toArray(), possibleValues.elementAt(0));
		this.allele1 = (Integer) selectedValue;
		possibleValues.remove(possibleValues.indexOf(this.allele1));
		selectedValue = JOptionPane.showInputDialog(null,
		             "Choose second allele", "Input",
		             JOptionPane.INFORMATION_MESSAGE, null,
		             possibleValues.toArray(), possibleValues.elementAt(0));
		this.allele2 = (Integer) selectedValue;
	}
	
	private void exit() {
		if (runSimple != null) runSimple.exit();
		
		isOK = false;
		setVisible(false);
	}

	private void runSimple() {
		Utils.emptyScratch();
		File f = new File(Prefs.tools_scratch, "simple.qmm");
		qtlResult.WriteQMM(f);
		
		pBar.setMaximum(cmLength);
	
		// Create 
		if (allele1 == -1) {
			runSimple = new RunSnpQtlSimple(traitI);
		} else {
			runSimple = new RunSnpQtlSimple(traitI, allele1, allele2);
		}
		new RunSimpleMonitor().start();
		// Start the thread that will run it
		runSimple.start();
	}

	private class RunSimpleMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
					pBar.setValue(runSimple.position);
					markerLabel.setText(
						"Chromosome position: " + (runSimple.position));
				}
			};
			
			while (runSimple.isRunning) {
				SwingUtilities.invokeLater(r);
				
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("RunSimpleMonitor.run() interrupted..");
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (runSimple.error == false) {
				processResults();
			} else {
				isOK = false;
			}
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file;
		if (allele1 == -1) file = new File(Prefs.tools_scratch, "simpleqmm.out");
		else file = new File(Prefs.tools_scratch, "simpleqmm_add.out");
		ProcessSnpQtlSimpleResults p = new ProcessSnpQtlSimpleResults(currentTrait, file);
		
		isOK = p.process();
	}
	
	public boolean isOK() {
		return isOK;
	}
}