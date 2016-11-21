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

package gui;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.print.Printable;
import java.awt.print.PrinterJob;
import javax.print.attribute.HashPrintRequestAttributeSet;
import javax.print.attribute.PrintRequestAttributeSet;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.SwingUtilities;
import doe.MsgBox;

public class PrinterDialog extends JDialog {
	private static final long serialVersionUID = -2713153182763168277L;
	private static PrinterJob job = PrinterJob.getPrinterJob();
	private static PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();

	private JLabel label;

	// Components that are to be printed
	private Printable[] toPrint = null;

	/** PrinterDialog().
	 * 
	 * 
	 */
	public PrinterDialog(Printable[] toPrint) {
		super(MsgBox.frm, "Printing", true);
		System.out.println("printerdialog.printerdialog()");
		this.toPrint = toPrint;

		JLabel icon = new JLabel(Icons.PRINT);
		icon.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

		label = new JLabel("Attempting communication with printers...");
		label.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 5));
		add(label);
		add(icon, BorderLayout.WEST);

		addWindowListener(new WindowAdapter() {
			public void windowOpened(WindowEvent e) {
				new Printer().start();
			}
		});

		pack();
		setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		setLocationRelativeTo(MsgBox.frm);
		setResizable(false);
		setVisible(true);
	}

	// Display the Java Printer PageSetup dialog
	static void showPageSetupDialog(AppFrame appFrame) {
		appFrame.setCursor(new Cursor(Cursor.WAIT_CURSOR));
		job.pageDialog(aset);
		appFrame.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	private void updateLabel() {
		Runnable r = new Runnable() {
			public void run() {
				label.setText("TetraploidMap is printing. Please be patient.");

				pack();
				setLocationRelativeTo(MsgBox.frm);
			}
		};

		try {
			SwingUtilities.invokeAndWait(r);
		} catch (Exception e) {
			System.out.println("updateLabel()");
		}
	}

	private class Printer extends Thread {
		public void run() {
			if (job.printDialog(aset)) {
				updateLabel();

				try {
					for (Printable p : toPrint) {
						job.setPrintable(p);
						job.print(aset);

					}
				} catch (Exception e) {
					MsgBox.msg("An error occurred while printing:\n" + e, MsgBox.ERR);
				}
			}

			setVisible(false);
		}
	}
}
