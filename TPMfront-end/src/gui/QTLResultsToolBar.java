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

import java.awt.Insets;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JToolBar;
import data.TraitFile;

@SuppressWarnings("serial")
class QTLResultsToolBar extends JToolBar {
	private QTLResultsPanel panel;

	AbstractAction aSavePNG, aSaveTXT, aRescan, aPerm;

	QTLResultsToolBar(QTLResultsPanel panel) {
		this.panel = panel;

		createActions();

		setFloatable(false);
		setMargin(new Insets(0, 0, 0, 0));
		setBorderPainted(false);
		setOrientation(JToolBar.VERTICAL);

		add(AppFrameToolBar.getButton(false, null, "Save Graph as PNG Image", Icons.SAVE_IMAGE, aSavePNG));
		add(AppFrameToolBar.getButton(false, null, "Save Graph as CSV Text File", Icons.SAVE_TEXT, aSaveTXT));
		addSeparator();
		add(AppFrameToolBar.getButton(false, null, "Test With Simple Model", Icons.RESCAN, aRescan));
		add(AppFrameToolBar.getButton(false, null, "Run Permutation Test", Icons.PERM, aPerm));

		enableButtons(false, null);
	}

	private void createActions() {
		aSavePNG = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.savePNG();
			}
		};

		aSaveTXT = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.saveTXT();
			}
		};

		if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
			aRescan = new AbstractAction("") {
				public void actionPerformed(ActionEvent e) {
					panel.runSNPQTLRescan();
				}
			};

			aPerm = new AbstractAction("") {
				public void actionPerformed(ActionEvent e) {
					panel.runSNPQTLPerm();
				}
			};
		} else {
			aRescan = new AbstractAction("") {
				public void actionPerformed(ActionEvent e) {
					panel.runRescan();
				}
			};

			aPerm = new AbstractAction("") {
				public void actionPerformed(ActionEvent e) {
					panel.runPerm();
				}
			};

		}
	}

	void enableButtons(boolean state, TraitFile traitFile) {
		aSavePNG.setEnabled(state);
		aSaveTXT.setEnabled(state);

		if (traitFile == null) {
			state = false;
		}

		aRescan.setEnabled(state);
		aPerm.setEnabled(state);
	}
}