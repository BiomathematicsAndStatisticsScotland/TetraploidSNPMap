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

import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.RenderingHints;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.text.DecimalFormat;

import javax.swing.JPanel;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.LogarithmicAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import data.Dendrogram;

class DendrogramChart extends JPanel implements Printable {
	static final long serialVersionUID = 5773262026713363L;
	private Dendrogram d;

	private CardLayout card;
	private ChartPanel panelNormal, panelLogged;

	private boolean isLogged = false;

	DendrogramChart(Dendrogram d, boolean log) {
		this.d = d;

		card = new CardLayout();
		setLayout(card);

		panelNormal = getChartPanel(d, false);
		panelLogged = getChartPanel(d, true);

		add("false", panelNormal);
		add("true", panelLogged);
	}

	ChartPanel getChartPanel(Dendrogram d, boolean log) {
		JFreeChart chart = ChartFactory.createXYLineChart(null, "Similarity", "No. of Groups", null,
				PlotOrientation.VERTICAL, true, true, false);

		setChartData(chart, log);

		RenderingHints rh = new RenderingHints(RenderingHints.KEY_ANTIALIASING, 
				RenderingHints.VALUE_ANTIALIAS_OFF);
		chart.setRenderingHints(rh);
		chart.removeLegend();

		XYPlot plot = chart.getXYPlot();
		plot.setBackgroundPaint(new Color(255, 255, 220));
		plot.setDomainGridlinePaint(new Color(128, 128, 128));
		plot.setRangeGridlinePaint(new Color(128, 128, 128));
		if (log == false) {
			NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
			rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());
			rangeAxis.setLowerBound(0);
			rangeAxis.setNumberFormatOverride(new DecimalFormat("0"));
			rangeAxis.setLabelFont(Prefs.labelFont);
		} else {
			// LogarithmicAxis logXAxis = new LogarithmicAxis("Similarity");
			// logXAxis.setAllowNegativesFlag(true);
			LogarithmicAxis logYAxis = new LogarithmicAxis("No. Of Groups");
			logYAxis.setAllowNegativesFlag(false);

			// plot.setDomainAxis(logXAxis);
			plot.setRangeAxis(logYAxis);
		}

		ChartPanel chartPanel = new ChartPanel(chart);
		// chartPanel.setPopupMenu(null);
		return chartPanel;
	}

	private void setChartData(JFreeChart chart, boolean log) {
		XYSeries series = new XYSeries("Similarity vs No. of Groups");

		double rSim = d.getRootSimilarity();
		double diff = 1 - rSim;

		// TODO: What causes this?
		if (diff == 0) return;

		for (double sim = rSim; sim <= 1.0; sim += (diff / 50)) {
			series.add(sim, d.getGroupCount(sim));
		}

		XYSeriesCollection data = new XYSeriesCollection(series);
		chart.getXYPlot().setDataset(data);
	}

	void setVisibleChart(boolean log) {
		card.show(this, "" + log);
		isLogged = log;
	}

	public int print(Graphics graphics, PageFormat pf, int pageIndex) {
		return getCurrentChartPanel().print(graphics, pf, pageIndex);
	}

	void setScaling(boolean autorange, double min, double max) {
		JFreeChart chart = ((ChartPanel) getComponent(0)).getChart();
		ValueAxis axis = chart.getXYPlot().getDomainAxis();

		axis.setAutoRange(autorange);
		if (!autorange) {
			axis.setLowerBound(min);
			axis.setUpperBound(max);
		}
	}

	private ChartPanel getCurrentChartPanel() {
		if (isLogged == false) {
			return ((ChartPanel) getComponent(0));
		} else {
			return ((ChartPanel) getComponent(1));
		}
	}
}