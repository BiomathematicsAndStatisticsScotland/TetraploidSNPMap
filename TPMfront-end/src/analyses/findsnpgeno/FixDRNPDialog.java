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

import java.awt.BorderLayout;
import java.awt.Insets;
import java.awt.GridLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JPanel;
import javax.swing.JCheckBox;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.BorderFactory;
import javax.swing.SwingUtilities;
import gui.AppFrame;
import gui.GradientPanel;
import gui.Prefs;
import gui.exporter.FileWriterSNPloc;
import data.LinkageGroup;
import doe.DoeLayout;
import doe.MsgBox;

public class FixDRNPDialog extends JDialog implements ActionListener {
	/** FixDRNPDialog asks if users wants DR and/or NP's fixed, and fixes the lGroup.
	 * 
	 */
	private static final long serialVersionUID = -5652648158826216271L;
	private JCheckBox cFixNP, cFixDR;
	
	private JButton bOK, bCancel;
	private boolean isOK = false;
	private int numDr, numNp;
	private LinkageGroup lGroup;
	private Runnable updateLg;
	private RunFixDRNP runfix;
	
	/** FixDRNPDialog asks if users wants DR and/or NP's fixed, and fixes the lGroup. 
	 * 
	 * @param appFrame the AppFrame.
	 * @param numDr the number of DR's in lGroup.
	 * @param numNp the number of NP's in lGroup.
	 * @param lGroup the linkage group to be fixed.
	 * @param r a Runnable that will update the UI after fixing the lGroup.
	 */
	public FixDRNPDialog(AppFrame appFrame, int numDr, int numNp, LinkageGroup lGroup, Runnable r) {
		super(appFrame, "Fix DR/NP", true);
		this.numDr = numDr;
		this.numNp = numNp;
		this.lGroup = lGroup;
		this.updateLg = r;
		add(new GradientPanel("Fix DR/NP"), BorderLayout.NORTH);
		add(createGenPanel(), BorderLayout.CENTER);
		add(createButtons(), BorderLayout.SOUTH);
		
		pack();
		
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}
	
	private JPanel createGenPanel() {
		cFixNP = new JCheckBox("Fix NP", false);
		cFixNP.addActionListener(this);
		cFixDR = new JCheckBox("Fix DR", true);
		cFixDR.addActionListener(this);
		
		DoeLayout layout = new DoeLayout();
		
		String txt = "";
		if (numNp > 0) {
			if (numNp > 1) {
				txt = "There are " + numNp + " markers marked as 'NP'";
			} else {
				txt = "There is 1 marker marked as 'NP'";
			}
			if (numDr > 0) {
				txt += ", and t";
			} else {
				txt += ".";
			}
		} else {
			txt = "T";
		}
		if (numDr > 0) {
			if (numDr > 1) {
				txt += "here are " + numDr + " markers marked as 'DR'";
			} else {
				txt += "here is 1 marker marked as 'DR'.";
			}
		}
				
		layout.add(new JLabel(txt), 0, 0, 1, 1, new Insets(5, 5, 5, 5));
			
		if (numNp > 0) layout.add(cFixNP, 0, 1, 1, 1, new Insets(5, 5, 5, 5));
		if (numDr > 0) layout.add(cFixDR, 0, 2, 1, 1, new Insets(0, 5, 5, 5));
		
		return layout.getPanel();
	}
	
	private JPanel createButtons() {
		bOK = new JButton("OK");
		bOK.addActionListener(this);
		bCancel = new JButton("Cancel");
		bCancel.addActionListener(this);
		
		JPanel p1 = new JPanel(new GridLayout(1, 2, 5, 5));
		p1.add(bOK);
		p1.add(bCancel);
		
		JPanel p2 = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
		p2.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p2.add(p1);
		
		return p2;
	}
	
	private void fix_npdr() {
		boolean fixnp = false;
		boolean fixdr = false;
		if (numNp > 0 && cFixNP.isSelected()) fixnp = true;
		if (numDr > 0 && cFixDR.isSelected()) fixdr = true;
		lGroup.fix_npdr(fixnp, fixdr);
		File file = new File(Prefs.tools_scratch, "recalc.loc");
		FileWriterSNPloc writer = new FileWriterSNPloc(file);
		writer.fixedOnly = true;
		if (writer.writeData(lGroup) == false) {
			MsgBox.msg("can't write recalc.loc", MsgBox.ERR);
			exit();
			return;
		}
		runfix = new RunFixDRNP();
		// Start the thread that will monitor its progress
		new RunFixMonitor().start();
		// Start the thread that will run it
		runfix.start();
	}
	
	private void exit() {
		if (runfix != null) {
			runfix.exit();
		}
		
		isOK = false;
		setVisible(false);
	}
	
	/**
	 * handles the OK and Cancel buttons.
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == bOK) {
			fix_npdr();
			isOK = true;
			setVisible(false);
		} else if (e.getSource() == bCancel) {
			setVisible(false);
		}
	}
	
	public boolean isOK() {
		return isOK;
	}

	private class RunFixMonitor extends Thread {
		public void run() {
			Runnable r = new Runnable() {
				public void run() {
				}
			};
			
			while (runfix.isRunning) {
				SwingUtilities.invokeLater(r);
				try {
					Thread.sleep(50);
				} catch (InterruptedException e) {
					System.out.println("FixDRNPDialog.runfixMonitor iterrupted");
				}
			}
			
			SwingUtilities.invokeLater(r);
			
			if (runfix.error == false) {
				processResults();
			} else {
				String err = "fix DRNP - fortran error.";
				System.out.println(err);
				MsgBox.msg("Critical error:" + err, MsgBox.ERR);
				isOK = false;
			}

			SwingUtilities.invokeLater(updateLg);
			setVisible(false);
		}
	}
	
	private void processResults() {
		File file = new File(Prefs.tools_scratch, "recalc.out");
		
		ProcessRecalcResults p = new ProcessRecalcResults(lGroup, file);
		
		while (p.isRunning) {
			try {
				Thread.sleep(50);
			} catch (InterruptedException e) {
				System.out.println("FixDRNPDialog.processResults iterrupted");
			}
		}
		
		isOK = !p.error;
		lGroup.fix_selection();
	}
}
