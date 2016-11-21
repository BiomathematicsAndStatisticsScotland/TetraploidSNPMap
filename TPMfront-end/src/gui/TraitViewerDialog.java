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

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.chart.renderer.category.BoxAndWhiskerRenderer;
import org.jfree.data.statistics.BoxAndWhiskerCalculator;
import org.jfree.data.statistics.BoxAndWhiskerItem;
import org.jfree.data.statistics.DefaultBoxAndWhiskerCategoryDataset;
import data.TraitFile;

class TraitViewerDialog extends JDialog implements ActionListener, ListSelectionListener {
	private static final long serialVersionUID = -8305279754268031485L;
	private TraitFile tFile;
	private TraitTableModel model;
	private JTable table;
	private JButton bClose;
	private JSplitPane splits;// , rightsplits;
	private JTextArea traitstats = new JTextArea();

	TraitViewerDialog(AppFrame appFrame, TraitFile tFile) {
		super(appFrame, "Trait Selection", true);
		this.tFile = tFile;

		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				setVisible(false);
			}
		});

		add(createControls());
		add(createButtons(), BorderLayout.SOUTH);
		// setSize(300, 250);
		setSize(500, 450);

		// pack();

		setLocationRelativeTo(appFrame);
		setResizable(false);
		setVisible(true);
	}

	private JPanel createButtons() {
		bClose = new JButton("Close");
		bClose.addActionListener(this);

		JPanel p1 = new JPanel(new GridLayout(1, 1, 5, 5));
		p1.add(bClose);

		JPanel p2 = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
		p2.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		p2.add(p1);

		return p2;
	}

	private JPanel createControls() {
		model = new TraitTableModel();
		for (int i = 0; i < tFile.getNames().size(); i++) {
			model.addRow(new Object[] { i + 1, tFile.getEnabled().get(i), tFile.getNames().get(i) });
		}

		table = new JTable(model);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.getColumnModel().getColumn(0).setPreferredWidth(13);
		table.getColumnModel().getColumn(1).setPreferredWidth(30);
		table.getColumnModel().getColumn(2).setPreferredWidth(130);
		table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		table.getSelectionModel().addListSelectionListener(this);

		splits = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splits.setLeftComponent(new JScrollPane(table));

		splits.setDividerLocation(250);
		splits.setResizeWeight(0.5);
		splits.setRightComponent(new JPanel());
		JPanel p1 = new JPanel(new BorderLayout());
		p1.setBorder(BorderFactory.createEmptyBorder(10, 10, 0, 10));
		p1.add(splits);

		return p1;
	}

	public void actionPerformed(ActionEvent e) {
		setVisible(false);
	}

	public void processTableClick(ListSelectionEvent e) {
		ListSelectionModel lsm = (ListSelectionModel) e.getSource();
		if (lsm.isSelectionEmpty() == true) return;
		int selectedRow = lsm.getMinSelectionIndex();
		splits.setRightComponent(getChart(selectedRow));

	}

	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting()) return;

		if (e.getSource() == table.getSelectionModel()) processTableClick(e);
	}

	private String showBoxAndWhiskerItem(BoxAndWhiskerItem a) {
		String r = "";
		r += "mean: " + a.getMean() + "\n";
		r += "median: " + a.getMedian() + "\n";
		r += "25%: " + a.getQ1() + "\n";
		r += "75%: " + a.getQ3() + "\n";
		r += "minreg: " + a.getMinRegularValue() + "\n";
		r += "maxreg: " + a.getMaxRegularValue() + "\n";
		r += "minoutl: " + a.getMinOutlier() + "\n";
		r += "maxoutl: " + a.getMaxOutlier() + "\n";
		if (a.getOutliers() != null) {
			r += "outliers: " + a.getOutliers().toString() + "\n";
		}
		return r;
	}

	@SuppressWarnings("rawtypes")
	private ChartPanel getChart(int selectedRow) {
		
		ArrayList<Float> data = new ArrayList<Float>();
		for (float[] row : tFile.getRows()) {
			if (row[selectedRow + 1] != (float) -99.0) {
				data.add(row[selectedRow + 1]);
			}
		}
		BoxAndWhiskerItem a = BoxAndWhiskerCalculator.calculateBoxAndWhiskerStatistics(data);
		java.util.List l = new ArrayList(0);

		a = new BoxAndWhiskerItem(a.getMean(), a.getMedian(), a.getQ1(), a.getQ3(), a.getMinRegularValue(),
				a.getMaxRegularValue(), a.getMinRegularValue(), a.getMaxRegularValue(), l);
		traitstats.setText(showBoxAndWhiskerItem(a));

		DefaultBoxAndWhiskerCategoryDataset ds2 = new DefaultBoxAndWhiskerCategoryDataset();

		ds2.add(a, (Comparable) 1, (Comparable) 1);
		JFreeChart chart = ChartFactory.createBoxAndWhiskerChart(null, null, null, ds2, false);

		chart.removeLegend();

		// XYPlot plot = chart.getXYPlot();
		CategoryPlot plot = chart.getCategoryPlot();
		plot.getDomainAxis().setVisible(false);
		BoxAndWhiskerRenderer b = (BoxAndWhiskerRenderer) plot.getRenderer();
		// b.setFillBox(false);
		b.setSeriesPaint(0, new Color(236, 55, 169));
		b.setSeriesOutlinePaint(1, new Color(131, 79, 112));
		b.setBaseOutlineStroke(new BasicStroke(1.0f));
		// b.get
		b.setWhiskerWidth(0.8);
		b.setBaseOutlinePaint(new Color(84, 144, 201));
		b.setDefaultEntityRadius(2);
		b.setMaximumBarWidth(0.18);

		ChartPanel chartPanel = new ChartPanel(chart);
		chartPanel.setPopupMenu(null);
		return chartPanel;
	}

	private class TraitTableModel extends DefaultTableModel {
		private static final long serialVersionUID = 4369454315581118152L;

		String[] titles = new String[] { "Nr", "Selected", "Trait Name" };

		@SuppressWarnings("rawtypes")
		Class[] types = new Class[] { Integer.class, Boolean.class, String.class };

		TraitTableModel() {
			for (int i = 0; i < titles.length; i++) addColumn(titles[i]);

			this.addTableModelListener(new TableModelListener() {
				public void tableChanged(TableModelEvent e) {
					if (e.getType() != TableModelEvent.UPDATE) return;

					int r = e.getFirstRow();
					int c = e.getColumn();

					boolean selected = (Boolean) getValueAt(r, c);
					tFile.getEnabled().set(r, selected);
					AppFrameMenuBar.aFileSave.setEnabled(true);
				}
			});
		}

		public String getColumnName(int c) {
			return titles[c];
		}

		@SuppressWarnings({ "rawtypes", "unchecked" })
		public Class getColumnClass(int c) {
			return types[c];
		}

		public boolean isCellEditable(int r, int c) {
			return (c == 1);
		}
	}
}