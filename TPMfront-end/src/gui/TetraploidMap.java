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

import javax.swing.JOptionPane;
import javax.swing.UIManager;
import doe.MsgBox;

public class TetraploidMap {
	static AppFrame appFrame;

	/** asks which TPM MODE we should run. prevents running on non-64bit, non-windows platforms. 
	 * 
	 * @param args = if we start with the -nonsnp argument we make available the NONSNP tpmMode,
	 */
	public static void main(String[] args) {
		try {
			// UIManager.setLookAndFeel("net.java.plaf.windows.WindowsLookAndFeel");
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			// System.setProperty("winlaf.forceTahoma", "true");
			// net.java.plaf.LookAndFeelPatchManager.initialize();
			// UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel");
		} catch (Exception e) {
			System.out.println(e);
		}
		String os = System.getProperty("os.name");
		if (!os.contains("Windows")) {
			MsgBox.msg("TPM only works on Windows (64bit) platforms", MsgBox.ERR);
			return;
		}

		String arch = System.getenv("PROCESSOR_ARCHITECTURE");

		if (!arch.endsWith("64")) {
			String wow64Arch = System.getenv("PROCESSOR_ARCHITEW6432");
			if (wow64Arch == null || !wow64Arch.endsWith("64")) {
				MsgBox.msg("TPM only works on 64 bit Windows.", MsgBox.ERR);
				return;
			}
		}

		String bitness = System.getProperty("sun.arch.data.model");
		if (bitness.equals("32")) {
			MsgBox.msg(
					"You are running the 32-bit version of Java (JRE).\n"
							+ "The 32 bit JRE can only handle a very small\n"
							+ "heap size and may lead to TetraploidMap running\n" 
							+ "out of memory and crashing.",
					MsgBox.WAR);
		}

		// Load the icons
		new Icons();

		// Load the preferences
		Prefs prefs = new Prefs();

		prefs.loadPreferences(System.getProperty("user.home") + Prefs.sepF + ".TetraploidMap.txt");

		try {
			String selected;
			if (args.length > 0 && args[0].contentEquals("-nonsnp")) {
				Object[] options = { "SNP", "RFLP/AFLP/SSR", "SNP-QTL" };
				selected = (String) JOptionPane.showInputDialog(null,
						"What kind of marker data do you wish to analyse?", "Select TetraploidMap Mode",
						JOptionPane.QUESTION_MESSAGE, null, options, "SNP");
			} else {
				Object[] options = { "SNP", "SNP-QTL" };
				selected = (String) JOptionPane.showInputDialog(null,
						"What kind of marker data do you wish to analyse?", "Select TetraploidMap Mode",
						JOptionPane.QUESTION_MESSAGE, null, options, "SNP");
			}

			if (selected.equals("SNP")) {
				appFrame = new AppFrame(prefs, AppFrame.TPMMODE_SNP);
			} else if (selected.equals("SNP-QTL")) {
				appFrame = new AppFrame(prefs, AppFrame.TPMMODE_QTL);
			} else {
				appFrame = new AppFrame(prefs, AppFrame.TPMMODE_NONSNP);
			}
		} catch (Exception e) {
			System.out.println("EXIT TetraploidMap : " + e);
			System.exit(0);
		}

		new MsgBox(appFrame, "TetraploidMap");
	}
}