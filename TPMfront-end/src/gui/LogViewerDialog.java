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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import doe.DoeLayout;
import doe.MsgBox;

class LogViewerDialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = -6809616938653376454L;
	private Project project;
	private PrintableEditorPane text;
	private JButton bClose, bPrint, bSave;

	LogViewerDialog(AppFrame appFrame, Project project) {
		super(appFrame, "Project Log", true);
		this.project = project;

		add(createControls());

		pack();
		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}

	private JPanel createControls() {
		text = new PrintableEditorPane("text/html", Project.logger.getLog());
		text.setPreferredSize(new Dimension(550, 300));
		text.setEditable(false);
		text.setFont(new Font("Monospaced", Font.PLAIN, 11));
		text.setMargin(new Insets(2, 5, 2, 5));

		JScrollPane sp = new JScrollPane(text);
		sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		bClose = new JButton("Close");
		bClose.addActionListener(this);
		bPrint = new JButton("Print");
		bPrint.addActionListener(this);
		bSave = new JButton("Save");
		bSave.addActionListener(this);

		JPanel p1 = new JPanel(new GridLayout(4, 1, 5, 5));
		p1.add(bClose);
		p1.add(new JPanel());
		p1.add(bPrint);
		p1.add(bSave);

		DoeLayout layout = new DoeLayout();
		layout.add(sp, 0, 0, 0, 1, new Insets(10, 10, 10, 5));
		layout.gbc.anchor = GridBagConstraints.NORTH;
		layout.add(p1, 1, 0, 1, 1, new Insets(10, 0, 10, 10));

		return layout.getPanel();
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == bClose) {
			setVisible(false);
		} else if (e.getSource() == bSave) {
			save();
		} else if (e.getSource() == bPrint) {
			print();
		}
	}

	private void save() {
		File file = new File(Project.filename.getParent(), project.getName() + ".html");
		BufferedWriter out = null;

		try {
			out = new BufferedWriter(new FileWriter(file));
			out.write(Project.logger.getLog());

			MsgBox.msg("Project log successfully written to " + file, MsgBox.INF);
		} catch (Exception e) {
			MsgBox.msg("There was an unexpected error while writing the " + "project log:\n" + e, MsgBox.ERR);
		}

		try {
			out.close();
		} catch (Exception e) {
			System.out.println("LogViewer save() out.close() failed.");
		}
	}

	private void print() {
		Printable[] toPrint = { text };
		new PrinterDialog(toPrint);
	}

	private class PrintableEditorPane extends JEditorPane implements Printable {
		private static final long serialVersionUID = -3819876451428441284L;

		PrintableEditorPane(String s1, String s2) {
			super(s1, s2);
		}

		public int print(Graphics graphics, PageFormat pf, int pageIndex) {
			Graphics2D g2 = (Graphics2D) graphics;

			double panelWidth = getSize().width; // width in pixels
			double panelHeight = getSize().height; // height in pixels

			double pageHeight = pf.getImageableHeight();
			double pageWidth = pf.getImageableWidth(); // width of printer page

			double scale = pageWidth / panelWidth;
			int totalNumPages = (int) Math.ceil(scale * panelHeight / pageHeight);

			// Make sure empty pages aren't printed
			if (pageIndex >= totalNumPages) return NO_SUCH_PAGE;

			// Shift Graphic to line up with beginning of print-imageable region
			g2.translate(pf.getImageableX(), pf.getImageableY());
			// Shift Graphic to line up with beginning of next page to print
			g2.translate(0f, -pageIndex * pageHeight);

			if (scale < 1) g2.scale(scale, scale);

			this.printAll(g2);
			return PAGE_EXISTS;
		}
	}
}