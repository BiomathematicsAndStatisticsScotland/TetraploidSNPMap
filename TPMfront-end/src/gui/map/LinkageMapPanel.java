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

package gui.map;

import java.awt.BorderLayout;
import java.awt.image.BufferedImage;
import java.awt.print.Printable;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import data.LinkageGroup;
import data.OrderedResult;
import doe.MsgBox;
import gui.Prefs;
import gui.PrinterDialog;

public class LinkageMapPanel extends JPanel {
	private static final long serialVersionUID = 5688589571157982492L;
	private MapPanel panel = null;
	private MapToolBar toolbar = null;
	private JScrollPane sp;

	/** LinkageMapPanel().
	 * 
	 */
	public LinkageMapPanel(OrderedResult order, LinkageGroup[] lGroups) {
		MapCreator creator = new MapCreator(order, lGroups);

		panel = new MapPanel(creator);
		toolbar = new MapToolBar(this, panel);

		sp = new JScrollPane(panel);
		sp.getVerticalScrollBar().setUnitIncrement(15);
		sp.getHorizontalScrollBar().setUnitIncrement(15);

		setLayout(new BorderLayout());
		add(sp);
		add(toolbar, BorderLayout.EAST);
	}

	public JScrollPane getSP() {
		return this.sp;
	}

	public void print() {
		Printable[] toPrint = { panel };
		new PrinterDialog(toPrint);
	}

	void save(boolean asImage) {
		JFileChooser fc = new JFileChooser();
		// fc.addChoosableFileFilter(Filters.getFileFilter(3));
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		fc.setDialogTitle("Save Linkage Map");

		while (fc.showSaveDialog(MsgBox.frm) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			String ext = asImage ? ".png" : ".txt";

			// Make sure it has an appropriate extension
			if (!file.exists()) {
				if (file.getName().indexOf(".") == -1) {
					file = new File(file.getPath() + ext);
				}
			}

			// Confirm overwrite
			if (file.exists()) {
				int response = MsgBox.yesnocan(file + " already exists.\nDo " + "you want to replace it?", 1);

				if (response == JOptionPane.NO_OPTION) {
					continue;
				} else if (response == JOptionPane.CANCEL_OPTION || response == JOptionPane.CLOSED_OPTION) {
					return;
				}
			}

			// Otherwise it's ok to save...
			Prefs.gui_dir = "" + fc.getCurrentDirectory();
			if (asImage) {
				saveImage(file);
			} else {
				saveText(file);
			}
			return;
		}
	}

	private void saveImage(File file) {
		BufferedImage image = panel.getSavableImage();
		try {
			ImageIO.write(image, "png", file);
			// Thread.sleep(999999);
			MsgBox.msg("Data successfully saved to " + file, MsgBox.INF);
		} catch (Exception e) {
			MsgBox.msg("There was an unexpected error while saving the image:" + "\n" + e, MsgBox.ERR);
		}
	}

	private void saveText(File file) {
		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			int i = panel.chromosomes.size();
			if (i > 2) {
				saveData(panel.chromosomes.get(0), out, "Overall");
			}
			for (int j = 1; j < i; j++) {
				saveData(panel.chromosomes.get(j), out, "C" + j);
			}

			out.close();

			MsgBox.msg("Data successfully saved to " + file, MsgBox.INF);
		} catch (Exception e) {
			MsgBox.msg("There was an unexpected error while saving the data:" + "\n" + e, MsgBox.ERR);
		}
	}

	private void saveData(GMarker[] data, BufferedWriter out, String title) throws Exception {
		out.write(title);
		out.newLine();
		if (panel.topDown) {
			for (GMarker marker : data) {
				out.write(marker.name + "\t" + Prefs.d3.format(marker.cm));
				out.newLine();
			}
		} else {
			for (int i = data.length - 1; i >= 0; i--) {
				GMarker marker = data[i];
				out.write(marker.name + "\t" + Prefs.d3.format(panel.totalDistance - marker.cm));
				out.newLine();
			}
		}

		out.newLine();
	}
}