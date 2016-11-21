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
import java.awt.print.Printable;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import data.Cluster;
import data.Dendrogram;
import doe.MsgBox;

/** DendrogramsPanel.
 * 
 * <p>TV: This class is called from DendrogramsNode in order to construct the panel which
 * displays the Dendrogram and the DendrogramChart on a DendrogramCanvas. This panel is
 * displayed when the option "Dendrograms" is selected in the NavPanel.
 */
public class DendrogramsPanel extends JPanel {
	static final long serialVersionUID = 27226499926292L;
	private Dendrogram snDen = null;
	private Dendrogram avDen = null;
	
	private DendrogramCanvas snCanvas;
	private DendrogramChart snChart;
	private DendrogramCanvas avCanvas;
	private DendrogramChart avChart;
	private int tpmmode;
		
	/** TV Second constructor for DendrogramsPanel to be called when clustering SNP marker data.
	 * 
	 */
	public DendrogramsPanel(Cluster cluster, int tpmmode) {
		avDen = cluster.getAvLnkDendrogram();
		this.tpmmode = tpmmode;
		JPanel p2 = getResultPanel(avDen, "Average Linkage Clustering", false);
		setLayout(new BorderLayout());
		if (tpmmode != AppFrame.TPMMODE_NONSNP) {
			add(p2);
		} else {
			snDen = cluster.getSnLnkDendrogram();
			JPanel p1 = getResultPanel(snDen, "Single Linkage Clustering", true);
			JSplitPane splits = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
			splits.setOneTouchExpandable(true);
			splits.setTopComponent(p1);
			splits.setBottomComponent(p2);
			splits.setResizeWeight(0.5);	
			setLayout(new BorderLayout());
			add(splits);
		}
	}
	
	
	private JPanel getResultPanel(Dendrogram d, String title, boolean sn) {
		JScrollPane sp = new JScrollPane();
		JLabel label = new JLabel();
		sp.getVerticalScrollBar().setUnitIncrement(15);
		DendrogramCanvas canvas = new DendrogramCanvas(sp, d, label);
		sp.getViewport().setView(canvas);
		
		JPanel p1 = new JPanel(new BorderLayout());
		p1.add(sp);
		p1.add(label, BorderLayout.SOUTH);
		
		DendrogramChart chart = new DendrogramChart(d, false);

		JSplitPane splits = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splits.setLeftComponent(p1);
		splits.setRightComponent(chart);
		splits.setResizeWeight(0.5);
		
		DendrogramToolBar toolbar = new DendrogramToolBar(this, canvas, chart);

		JPanel p2 = new JPanel(new BorderLayout(0, 0));
		p2.add(toolbar, BorderLayout.EAST);
		p2.add(splits);
		
		JPanel p3 = new JPanel(new BorderLayout(0, 0));
		p3.add(new GradientPanel(title), BorderLayout.NORTH);		
		p3.add(p2);
		
		if (sn) {
			snCanvas = canvas;
			snChart = chart;
		} else {
			avCanvas = canvas;
			avChart = chart;
		}
		
		return p3;
	}
	
	/** opens the printerdialog.
	 * 
	 */
	public void print() {
		//System.out.println("dendrogramspanel.print()");
		if (tpmmode == AppFrame.TPMMODE_NONSNP) {
			Printable[] toPrint = { snCanvas, snChart, avCanvas, avChart };
			new PrinterDialog(toPrint);
		} else {
			Printable[] toPrint = {  avCanvas, avChart };
			new PrinterDialog(toPrint);
		}
		
	}
	
	void setChartScaling(DendrogramChart chart) {
		// TV: Don't give the 3rd option for SNP mode, since there is only one graph
		Object[] values = {
			"Autorange",
			"Scaled to between 0 and 1",
			null
		};		
		if (this.tpmmode == AppFrame.TPMMODE_NONSNP) { 
			values[0] = "Autorange";
			values[1] = "Scaled to between 0 and 1";
			values[2] = "Scaled to the minimum value from both graphs";
		}
		
		Object selectedValue = JOptionPane.showInputDialog(MsgBox.frm,
			"Select the type of graph scaling to be applied.\n(Note that this "
			+ "only affects the graphs when showing non-log values)",
			"Set Domain-Range Scaling", JOptionPane.QUESTION_MESSAGE, null,
			values, values[Prefs.gui_graph_scale_type]);
	
		if (selectedValue == null) return;
		
		
		if (selectedValue == values[0]) {
			chart.setScaling(true, 0, 1);
			Prefs.gui_graph_scale_type = 0;
		} else if (selectedValue == values[1]) {
			chart.setScaling(false, 0, 1);
			Prefs.gui_graph_scale_type = 1;
		} else if (selectedValue == values[2]) {
			double minimum = getMinimumSimilarity();
			snChart.setScaling(false, minimum, 1);
			avChart.setScaling(false, minimum, 1);
			
			Prefs.gui_graph_scale_type = 2;
		}
	}
	
	private double getMinimumSimilarity() {
		double num1 = avDen.getRootSimilarity();
		double num2 = snDen.getRootSimilarity();
		
		// Return the smallest value
		return num1 <= num2 ? num1 : num2;
	}
}