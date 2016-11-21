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

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

public class AppFrameToolBar extends JToolBar {
	static final long serialVersionUID = 40097936765810605L;
	AppFrame appFrame;

	// different contructor is called for the different modes
	AppFrameToolBar(AppFrame appfr) {
		appFrame = appfr;
		setFloatable(false);
		// setMargin(new Insets(0, 0, 0, 0));
		setBorderPainted(false);

		add(getButton(false, "New Project", "Create a New Project", Icons.NEW_PROJECT,
				AppFrameMenuBar.aFileNewProject));
		add(getButton(false, "Open Project", "Open an Existing Project", Icons.OPEN, 
				AppFrameMenuBar.aFileOpenProject));
		addSeparator();
		add(getButton(false, null, "Save Current Project", Icons.SAVE, 
				AppFrameMenuBar.aFileSave));
		add(getButton(false, null, "Print", Icons.PRINT, 
				AppFrameMenuBar.aFilePrint));
		addSeparator();
		add(getButton(false, null, "Remove Analysis Results", Icons.DELETE, 
				AppFrameMenuBar.aAnalysisRemove));
		add(getButton(false, "Import markers", "import dataset into Project", Icons.IMPORT,
				AppFrameMenuBar.aFileImport));
		if (appFrame.tpmModeQTL()) {
			add(getButton(false, "Import map", "import dataset into Project", Icons.IMPORT,
					AppFrameMenuBar.aFileImportMap));
		}

		addSeparator();
		// add(getButton("", "", Icons.DELETE, null));
		// addSeparator();
		if (appFrame.tpmModeNONSNP()) {
			add(getButton(false, "Select Markers", "Select Markers for Analysis", Icons.SELECT_MARKERS,
					AppFrameMenuBar.aSelect));
			addSeparator();
			add(getButton(false, "Cluster", "Cluster Markers into Linkage Groups", Icons.RUN_CLUSTER,
					AppFrameMenuBar.aAnalysisCluster));
			add(getButton(false, "Order", "Compute Phase/Dosage and Order Markers", Icons.RUN_SIMANNEAL,
					AppFrameMenuBar.aAnalysisOrder));
			add(getButton(false, "Map", "Generate Linkage Map", Icons.RUN_MAP, 
					AppFrameMenuBar.aAnalysisMap));
			add(getButton(false, "ANOVA", "Run Analysis of Variance", Icons.RUN_ANOVA, 
					AppFrameMenuBar.aAnalysisAnova));
			add(getButton(false, "QTL", "Perform QTL Analysis", Icons.RUN_SIMANNEAL, 
					AppFrameMenuBar.aAnalysisQTL));
		} else if (appFrame.tpmModeSNP()) {
			add(getButton(false, "Cluster", "Cluster Markers into Linkage Groups", Icons.RUN_CLUSTER,
					AppFrameMenuBar.aAnalysisCluster));
			add(getButton(false, "Twopoint", "Run Twopoint", Icons.RUN_TWOPOINT, 
					AppFrameMenuBar.aAnalysisTWOPOINT));
			add(getButton(false, "MDS", "Run Multidimensional Scaling (MDS)", Icons.RUN_SIMANNEAL,
					AppFrameMenuBar.aAnalysisMDS));
			add(getButton(false, "Phase", "Run Phase Analysis", Icons.RUN_SIMANNEAL, 
					AppFrameMenuBar.aPhase));

			add(getButton(false, "Map", "Generate Linkage Map", Icons.RUN_MAP, 
					AppFrameMenuBar.aAnalysisMap));

		} else {
			// QTL
			add(getButton(false, "QTL", "Perform QTL Analysis", Icons.RUN_SIMANNEAL, 
					AppFrameMenuBar.aAnalysisQTL));
			add(getButton(false, "Map", "Generate Linkage Map", Icons.RUN_MAP, 
					AppFrameMenuBar.aAnalysisMap));
		}
	}

	/** Utility method to help create the buttons. 
	 * Sets their text, tooltip, and icon, actionListener, defining margings, etc.
	 */
	public static AbstractButton getButton(boolean toggle, 
			String title, String tt, ImageIcon icon, Action a) {
		AbstractButton button = null;

		if (toggle) {
			button = new JToggleButton(a);
		} else {
			button = new JButton(a);
		}

		button.setBorderPainted(false);
		button.setMargin(new Insets(1, 1, 1, 1));
		button.setToolTipText(tt);
		button.setIcon(icon);
		button.setFocusPainted(false);

		button.setText(title);

		return button;
	}
}
