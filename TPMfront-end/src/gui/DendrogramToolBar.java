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
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

/*
 * DendrogramToolBar defines the vertical toolbar on the right side of the DendrogramsPanel,
 * which it is used for manipulating the different display modes for the cluster visualisation.
 * 
 * TV latest UPDATE: +/- Zoom In/Out buttons added in the toolbar.
 * TODO: perhaps search for a marker in the Dendrogram or filter markers 
 */
class DendrogramToolBar extends JToolBar {
	static final long serialVersionUID = 1999755691469769L;
	private DendrogramsPanel panel;
	private DendrogramCanvas canvas;
	private DendrogramChart chart;

	private JToggleButton bScale, bLog;
	private AbstractAction aScale, aColor, aLog, aRange, aZoomIn, aZoomOut;

	private JButton bZoomIn, bZoomOut;

	DendrogramToolBar(DendrogramsPanel panel, DendrogramCanvas canvas, DendrogramChart chart) {
		this.panel = panel;
		this.canvas = canvas;
		this.chart = chart;

		createActions();

		setFloatable(false);
		setMargin(new Insets(0, 0, 0, 0));
		setBorderPainted(false);
		setOrientation(JToolBar.VERTICAL);

		bScale = (JToggleButton) AppFrameToolBar.getButton(true, null, 
				"Scale Dendrogram to Fit Window", Icons.SCALE, aScale);
		bLog = (JToggleButton) AppFrameToolBar.getButton(true, null, 
				"View Log Scales on Graph", Icons.LOG, aLog);

		// TV: Zoom In/Out buttons
		bZoomIn = (JButton) AppFrameToolBar.getButton(false, null, 
				"Zoom In Dendrogram", Icons.ZOOM_IN, aZoomIn);
		bZoomOut = (JButton) AppFrameToolBar.getButton(false, null, 
				"Zoom Out Dendrogram", Icons.ZOOM_OUT, aZoomOut);

		add(bScale);
		add(bZoomIn);
		add(bZoomOut);
		add(AppFrameToolBar.getButton(false, null, 
				"Colour Dendrogram via Similarity Score", Icons.COLOR_DENDROGRAM, aColor));
		addSeparator();
		add(bLog);
		add(AppFrameToolBar.getButton(false, null, 
				"Set Domain-Range Scaling", Icons.GRAPH_SCALE, aRange));
	}

	@SuppressWarnings("serial")
	private void createActions() {
		aScale = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.setSizeToFit(bScale.isSelected());
				bZoomIn.setEnabled(!bScale.isSelected());
				bZoomOut.setEnabled(!bScale.isSelected());
			}
		};

		aColor = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.updateColouriser(true, 0);
			}
		};

		aLog = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				chart.setVisibleChart(bLog.isSelected());
			}
		};

		aRange = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.setChartScaling(chart);
			}
		};

		// TV: Zoom In/Out actions performed
		aZoomIn = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.zoomIn();
			}
		};
		aZoomOut = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				canvas.zoomOut();
			}
		};

	}
}
