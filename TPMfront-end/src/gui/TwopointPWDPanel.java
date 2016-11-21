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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.RenderingHints;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.renderer.category.BarRenderer;
import org.jfree.chart.renderer.category.StandardBarPainter;
import org.jfree.data.category.DefaultCategoryDataset;
import data.CMarker;
import data.OrderedResult;
import data.PhasePair;
import doe.MsgBox;

class TwopointPWDPanel extends JPanel implements ListSelectionListener {
	private static final long serialVersionUID = 651804115102524864L;
	private OrderedResult order;
	private JTable markerTable, phaseTable;
	private PWDTableModel markerModel;
	private PWDTableModel2 phaseModel;
	private PWDChartPanel rfqChart, lodChart;
	private JLabel m1Label, m2Label;
	private boolean editingTable = false;
	private String lastEdit;

	TwopointPWDPanel(OrderedResult order) {
		this.order = order;
		setLayout(new BorderLayout(5, 5));
		add(createControls());
	}

	private JPanel createControls() {
		// Marker model (for ordered list of markers)
		markerModel = new PWDTableModel();

		// Populate the marker table
		markerTable = new JTable(markerModel);
		markerTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		markerTable.getColumnModel().getColumn(1).setPreferredWidth(175);
		markerTable.getColumnModel().getColumn(2).setPreferredWidth(60);
		for (CMarker cm : order.getLinkageGroup().getMarkers()) {
			markerModel.addRow(new Object[] { cm.marker.getPrefix(), cm, cm.marker.getRatio() });
		}
		markerTable.getSelectionModel().addListSelectionListener(this);

		// Phase model (for scores of each ordered marker against the others)
		phaseModel = new PWDTableModel2();
		phaseModel.setColumnIdentifiers(new Object[] { "Graph Code", "Marker Name", 
			"Recom Freq", "LOD Score" });

		phaseTable = new JTable(phaseModel);
		phaseTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		phaseTable.getSelectionModel().addListSelectionListener(this);

		rfqChart = new PWDChartPanel("Recombination Freq");
		lodChart = new PWDChartPanel("LOD Score");

		m1Label = new JLabel("");
		m1Label.setFont(new Font("Monospaced", Font.PLAIN, 11));
		m2Label = new JLabel("");
		m2Label.setFont(new Font("Monospaced", Font.PLAIN, 11));

		JScrollPane mSP = new JScrollPane(markerTable);
		mSP.setPreferredSize(new Dimension(300, 10));

		JPanel p1 = new JPanel(new BorderLayout(5, 0));
		p1.add(new JLabel("Ordered Markers:"), BorderLayout.NORTH);
		p1.add(mSP);

		JPanel p2 = new JPanel(new BorderLayout(5, 0));
		p2.add(new JLabel("Scores:"), BorderLayout.NORTH);
		p2.add(new JScrollPane(phaseTable));

		JPanel p3 = new JPanel(new GridLayout(2, 1, 2, 2));
		p3.setBorder(BorderFactory.createLoweredBevelBorder());
		p3.add(m1Label);
		p3.add(m2Label);

		JPanel p4 = new JPanel(new GridLayout(1, 2, 5, 5));
		p4.add(rfqChart);
		p4.add(lodChart);

		JPanel p5 = new JPanel(new BorderLayout(5, 5));
		p5.add(p3, BorderLayout.NORTH);
		p5.add(p4, BorderLayout.CENTER);

		JSplitPane splits = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splits.setResizeWeight(0.5);
		splits.setLeftComponent(p1);
		splits.setRightComponent(p2);

		JPanel p6 = new JPanel(new BorderLayout(5, 5));
		p6.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		p6.add(splits);
		p6.add(p5, BorderLayout.SOUTH);

		return p6;
	}

	// Reacts to a change in the selected Marker in the list. For that marker...
	// finds (in turn) each other marker (in the correct ordering, and adds its
	// RFQ and LOD scores to the data table.
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting()) return;

		if (e.getSource() == markerTable.getSelectionModel()) {
			processMarkerTableClick(e);
		} else {
			processPhaseTableClick(e);
		}
	}

	private void processMarkerTableClick(ListSelectionEvent e) {
		// Reset the display areas
		while (phaseModel.getRowCount() > 0) {
			phaseModel.removeRow(0);
		}
		rfqChart.clearDataset();
		lodChart.clearDataset();

		// The selected marker...
		ListSelectionModel lsm = (ListSelectionModel) e.getSource();
		CMarker selM = null;
		if (lsm.isSelectionEmpty() == false) {
			int selectedRow = lsm.getMinSelectionIndex();
			selM = (CMarker) markerTable.getValueAt(selectedRow, 1);
		}

		if (selM != null) {
			// ...against all the other markers (in the correct order)
			if (AppFrame.tpmmode != AppFrame.TPMMODE_NONSNP) {
				order.freezeSafeNames();
			}
			for (CMarker lnkM : order.getLinkageGroup().getMarkers()) {
				// Self against self -> add an empty row
				if (selM.marker == lnkM.marker) {
					phaseModel.addRow(new Object[] { "", selM, "", "" });
				} else {
					int i = order.getLinkageGroup().getMarkerIndexBySafeName(selM.safeName);
					int j = order.getLinkageGroup().getMarkerIndexBySafeName(lnkM.safeName);
					int i2 = i, j2 = j;
					if (order.translateIndexes != null) {
						i2 = order.translatePPIndexes[i];
						j2 = order.translatePPIndexes[j];
					}
					if (order.getPhasePairArray()[i2][j2] != null) {
						appendMarker(lnkM, order.getPhasePairArray()[i2][j2]);
					}
				}
			}
		}

		m1Label.setText("");
		m2Label.setText("");
	}

	private void processPhaseTableClick(ListSelectionEvent e) {
		ListSelectionModel lsm = (ListSelectionModel) e.getSource();
		if (lsm.isSelectionEmpty()) return;

		// Which two markers are selected?
		CMarker cm1 = (CMarker) markerTable.getValueAt(markerTable.getSelectedRow(), 1);
		CMarker cm2 = (CMarker) phaseTable.getValueAt(phaseTable.getSelectedRow(), 1);

		if (cm1 == cm2) {
			m1Label.setText("");
			m2Label.setText("");
		} else {
			int cm1i = order.getLinkageGroup().getMarkerIndexBySafeName(cm1.safeName);
			int cm2i = order.getLinkageGroup().getMarkerIndexBySafeName(cm2.safeName);
			if (order.translateIndexes != null) {
				cm1i = order.translatePPIndexes[
				    order.getLinkageGroup().getMarkerIndexBySafeName(cm1.safeName)];
				cm2i = order.translatePPIndexes[
	                order.getLinkageGroup().getMarkerIndexBySafeName(cm2.safeName)];
			}

			PhasePair pp = order.getPhasePairArray()[cm1i][cm2i];

			// First determine the marker with the longest name
			int length1 = cm1.marker.getName().length();
			int length2 = cm2.marker.getName().length();
			int max = length1 >= length2 ? length1 : length2;

			// Need to determine *which way round* the phases are
			// stored/displayed
			if (pp.compare(cm1, cm2) == 1) {
				m1Label.setText(cm1.marker.getName(max) + " " + pp.phase1);
				m2Label.setText(cm2.marker.getName(max) + " " + pp.phase2);
			} else {
				m1Label.setText(cm1.marker.getName(max) + " " + pp.phase2);
				m2Label.setText(cm2.marker.getName(max) + " " + pp.phase1);
			}
		}
	}

	private void appendMarker(CMarker m, PhasePair pp) {
		Object[] row = { Integer.parseInt(m.safeName.substring(3)) + "", 
			m, new Float(pp.rfq), new Float(pp.lod) };

		phaseModel.addRow(row);

		rfqChart.dataset.addValue(pp.rfq, "", m.safeName.substring(3));
		lodChart.dataset.addValue(pp.lod, "", m.safeName.substring(3));
	}

	private class PWDChartPanel extends JPanel {
		private static final long serialVersionUID = -1691465469177391053L;
		DefaultCategoryDataset dataset = new DefaultCategoryDataset();

		PWDChartPanel(String title) {
			JFreeChart chart = ChartFactory.createBarChart(null, // chart title
					"Position", // domain axis label
					title, // range axis label
					dataset, // data
					PlotOrientation.VERTICAL, false, // include legend
					true, false);
			chart.removeLegend();
			RenderingHints rh = new RenderingHints(RenderingHints.KEY_ANTIALIASING, 
					RenderingHints.VALUE_ANTIALIAS_OFF);
			chart.setRenderingHints(rh);

			BarRenderer ren = (BarRenderer) chart.getCategoryPlot().getRenderer();
			ren.setSeriesPaint(0, new Color(140, 165, 214));
			ren.setBarPainter(new StandardBarPainter());
			ren.setShadowVisible(false);
			CategoryPlot plot = chart.getCategoryPlot();
			plot.setBackgroundPaint(new Color(255, 255, 220));
			plot.setRangeGridlinePaint(new Color(128, 128, 128));

			NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
			rangeAxis.setNumberFormatOverride(new java.text.DecimalFormat("0.0"));
			if (title.startsWith("Rec")) rangeAxis.setUpperBound(0.5);

			setLayout(new BorderLayout());
			ChartPanel panel = new ChartPanel(chart);
			panel.setPreferredSize(new java.awt.Dimension(10, 200));
			add(panel);
		}

		void clearDataset() {
			while (dataset.getColumnCount() > 0) dataset.removeColumn(0);
		}
	}

	private class PWDTableModel extends DefaultTableModel {
		private static final long serialVersionUID = 6559448500205641978L;

		String[] titles = new String[] { "SC Group", "Marker Name", "Ratio" };

		@SuppressWarnings("rawtypes")
		Class[] types = new Class[] { String.class, data.Marker.class, String.class };

		PWDTableModel() {
			for (int i = 0; i < titles.length; i++) addColumn(titles[i]);

			this.addTableModelListener(new TableModelListener() {
				public void tableChanged(TableModelEvent e) {
					editingTable = false;
					if (editingTable) {
						int r = e.getFirstRow();
						int c = e.getColumn();

						if (error((String) getValueAt(r, c)) == false) {
							AppFrameMenuBar.aFileSave.setEnabled(true);
						} else {
							setValueAt(lastEdit, r, c);
						}

						editingTable = false;
					}
				}
			});
		}

		private boolean error(String str) {
			boolean error = false;

			if (str.length() != 4) error = true;

			try {
				Integer.parseInt(str);
			} catch (NumberFormatException e) {
				e.printStackTrace(System.out);
				error = true;
			}

			if (error) {
				MsgBox.msg("String formatting should be in the form 0000, " 
						+ "where each digit is in the range 0-9.",
						MsgBox.ERR);
			}

			return error;
		}

		public String getColumnName(int c) {
			return titles[c];
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Class getColumnClass(int c) {
			return types[c];
		}

		public boolean isCellEditable(int r, int c) {
			/*
			 * if ((c > 3)&&(c < 6)) { editingTable = true; lastEdit = (String)
			 * getValueAt(r, c); return true; } else
			 */
			return false;
		}
	}

	private class PWDTableModel2 extends DefaultTableModel {
		private static final long serialVersionUID = 1608375666567540833L;

		public boolean isCellEditable(int r, int c) {
			return false;
		}
	}
}