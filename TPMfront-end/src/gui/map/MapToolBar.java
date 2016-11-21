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

import java.awt.Insets;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JScrollBar;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import gui.AppFrameToolBar;
import gui.Icons;

class MapToolBar extends JToolBar {
	private static final long serialVersionUID = -3676348683107196081L;
	private LinkageMapPanel panel;
	private MapPanel canvas;
	private JToggleButton bTopDown, bAntiAlias, bOverall;
	private AbstractAction aTopDown, aAntiAlias, aOverall, aSave;
	private AbstractAction aZoomIn, aZoomOut, aSaveText;
	private double zoomD;

	MapToolBar(LinkageMapPanel panel, MapPanel canvas) {
		this.panel = panel;
		this.canvas = canvas;

		createActions();

		setFloatable(false);
		setMargin(new Insets(0, 0, 0, 0));
		setBorderPainted(false);
		setOrientation(JToolBar.VERTICAL);

		bTopDown = (JToggleButton) AppFrameToolBar.getButton(true, null, 
				"Draw Chromosomes from Bottom-Up", Icons.FLIP, aTopDown);

		bAntiAlias = (JToggleButton) AppFrameToolBar.getButton(true, null, 
				"Anti-alias Display", Icons.AA, aAntiAlias);

		bOverall = (JToggleButton) AppFrameToolBar.getButton(true, null, 
				"Don't Draw Overall Chromosome", Icons.HIDE_OVERALL, aOverall);

		add(bTopDown);
		add(bAntiAlias);
		add(bOverall);
		addSeparator();
		add(AppFrameToolBar.getButton(false, null, "Zoom Display In", Icons.ZOOM_IN, aZoomIn));
		add(AppFrameToolBar.getButton(false, null, "Zoom Display Out", Icons.ZOOM_OUT, aZoomOut));
		addSeparator();
		add(AppFrameToolBar.getButton(false, null, "Save LinkageMap as PNG Image", Icons.SAVE_IMAGE, aSave));
		add(AppFrameToolBar.getButton(false, null, "Save LinkageMap as Text", Icons.SAVE_TEXT, aSaveText));
	}

	@SuppressWarnings("serial")
	private void createActions() {
		aTopDown = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.flip(bTopDown);
			}
		};

		aAntiAlias = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.toggleAntiAliasing();
			}
		};

		aOverall = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.toggleOverallShown();
			}
		};

		if (canvas.chromosomes.size() == 2) {
			aOverall.setEnabled(false);
		}

		aSave = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.save(true);
			}
		};

		aZoomIn = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				zoom(true);
			}
		};

		aZoomOut = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				zoom(false);
			}
		};
		aZoomOut.setEnabled(false);

		aSaveText = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.save(false);
			}
		};
	}

	private void zoom(boolean zoomin) {
		// int ph = this.panel.getSP().getHeight();
		JScrollBar vsb = this.panel.getSP().getVerticalScrollBar();
		zoomD = (double) ((vsb.getHeight() / 2) + vsb.getValue()) / canvas.getHeight();

		if (zoomin) {
			canvas.zoom(1);
		} else {
			canvas.zoom(-1);
		}
		if (canvas.zoom == 1) {
			aZoomOut.setEnabled(false);
		} else {
			aZoomOut.setEnabled(true);
		}

		Runnable r = new Runnable() {
			public void run() {
				JScrollBar vsb = panel.getSP().getVerticalScrollBar();
				int newsb = (int) Math.round(zoomD * canvas.getHeight()) - (vsb.getHeight() / 2);
				vsb.setValue(newsb);
			}
		};
		SwingUtilities.invokeLater(r);
	}
}
