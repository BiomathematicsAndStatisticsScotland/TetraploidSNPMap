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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.RenderingHints;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import javax.swing.DefaultListModel;
import javax.swing.JFileChooser;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.ValueMarker;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import analyses.altqtl.AltQtlDialog;
import analyses.perm.PermDialog;
import analyses.snpperm.SNPPermDialog;
import analyses.snpqtlsimple.SnpQtlSimpleDialog;
import data.OrderedResult;
import data.PermResult;
import data.QTLResult;
import data.Trait;
import doe.MsgBox;

public class QTLResultsPanel extends JPanel implements ListSelectionListener {
	private static final long serialVersionUID = -641427706834428089L;
	private OrderedResult order;
	private QTLResult qtlResult;
	private JList<Trait> traitList;
	private JList<String> modelList;
	private DefaultListModel<Trait> traitModel;
	private JSplitPane splits, simplesplit;
	private JTextArea details, lodDetails, simpleright;
	private JScrollPane simpleDetails;
	private QTLResultsToolBar toolbar;
	private ChartPanel chartPanel;
	private Trait currentTrait, prevtrait;
	private int currentTraitI = -1;
	private int prevselectedsimplemodelindex = -1;

	/** QTLResultsPanel().
	 * 
	 * @param qtlResult = the QTL results to show.
	 * @param order = the ordered result data this QTL was created from. 
	 */
	public QTLResultsPanel(QTLResult qtlResult, OrderedResult order) {
		this.qtlResult = qtlResult;
		this.order = order;

		// Trait listbox
		traitModel = new DefaultListModel<Trait>();
		for (Trait trait : qtlResult.getTraits()) {
			traitModel.addElement(trait);
		}
		traitList = new JList<Trait>(traitModel);
		traitList.addListSelectionListener(this);
		traitList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		JScrollPane sp1 = new JScrollPane(traitList);
		sp1.setPreferredSize(new Dimension(125, 50));

		// Details text box
		details = new JTextArea();
		details.setFont(new Font("Monospaced", Font.PLAIN, 11));
		details.setMargin(new Insets(2, 5, 2, 5));
		details.setEditable(false);
		details.setTabSize(6);
		JScrollPane sp4;
		if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
			simpleDetails = new JScrollPane();
			simpleDetails.setFont(new Font("Monospaced", Font.PLAIN, 11));
			simplesplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
			simpleright = new JTextArea();
			simpleright.setFont(new Font("Monospaced", Font.PLAIN, 11));
			simpleright.setMargin(new Insets(2, 5, 2, 5));
			simpleright.setEditable(false);
			simpleright.setTabSize(6);
			simplesplit.setRightComponent(new JScrollPane(simpleright));

			simplesplit.setLeftComponent(simpleDetails);
			sp4 = new JScrollPane(simplesplit);
		} else {
			// TPM MODE NONSNP
			simpleright = new JTextArea();
			simpleright.setFont(new Font("Monospaced", Font.PLAIN, 11));
			simpleright.setMargin(new Insets(2, 5, 2, 5));
			simpleright.setEditable(false);
			sp4 = new JScrollPane(simpleright);
		}

		lodDetails = new JTextArea();
		lodDetails.setFont(new Font("Monospaced", Font.PLAIN, 11));
		lodDetails.setMargin(new Insets(2, 5, 2, 5));
		lodDetails.setEditable(false);
		lodDetails.setTabSize(6);
		JScrollPane sp3 = new JScrollPane(lodDetails);
		JTabbedPane tabs = new JTabbedPane();
		JScrollPane sp2 = new JScrollPane(details);
		tabs.add(sp2, "Full Model");
		tabs.add(sp4, "Simple Model");
		tabs.add(sp3, "LOD Details");

		// The splitpane
		splits = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		splits.setTopComponent(new JPanel());
		splits.setBottomComponent(tabs);
		splits.setResizeWeight(0.5);

		// pane2
		JSplitPane splits2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splits2.setLeftComponent(sp1);
		splits2.setRightComponent(splits);

		setLayout(new BorderLayout());
		add(new GradientPanel("QTL Analysis Results"), BorderLayout.NORTH);
		// add(sp1, BorderLayout.WEST);
		// add(splits);
		add(splits2);
		add(toolbar = new QTLResultsToolBar(this), BorderLayout.EAST);
	}

	/** listens for change to selected trait.
	 * 
	 */
	public void valueChanged(ListSelectionEvent e) {
		if (e.getValueIsAdjusting()) return;
		Trait trait = (Trait) traitList.getSelectedValue();
		if (trait != prevtrait) {
			displayTrait(trait);
			prevtrait = trait;
		}
		if (currentTraitI == -1) return;
		if (AppFrame.tpmmode == AppFrame.TPMMODE_NONSNP) return;
		int i = modelList.getSelectedIndex();
		if (i != prevselectedsimplemodelindex) {
			prevselectedsimplemodelindex = i;
			if (i < 1) {
				modelList.clearSelection();
				return;
			}
			String l = modelList.getSelectedValue().toString();
			String modelid = l.split(" ")[0];
			String info = trait.get_modelCoefficient(modelid.trim());
			if (!info.isEmpty()) {
				simpleright.setText(info);
			} else {
				modelList.clearSelection();
			}
		}

	}

	private void displayTrait(Trait trait) {
		// modelPanel.setTrait(trait);
		simpleright.setText("");
		if (trait == null) {
			details.setText("");
			lodDetails.setText("");
			splits.setTopComponent(new JPanel());
			currentTrait = null;
			currentTraitI = -1;
			toolbar.enableButtons(false, null);
			return;
		}

		StringBuffer text = new StringBuffer(1000);

		text.append("QTL position:         " + Prefs.d3.format(trait.qtlPosition) + " cM");
		if (trait.qtlEffects2 != null) {
			text.append("\t" + Prefs.d3.format(trait.qtlPosition2) + " cM");
		}
		text.append("\n");

		text.append("Maximum LOD score:    " + Prefs.d3.format(trait.maxLOD));
		if (trait.qtlEffects2 != null) {
			text.append("\t\t" + Prefs.d3.format(trait.maxLOD2));
		}
		text.append("\n");

		text.append("% variance explained: " + Prefs.d3.format(trait.varExplained));
		if (trait.qtlEffects2 != null) {
			text.append("\t\t" + Prefs.d3.format(trait.varExplained2));
		}
		text.append("\n");

		text.append("Error mean square:    " + Prefs.d3.format(trait.errMS));
		if (trait.qtlEffects2 != null) {
			text.append("\t\t" + Prefs.d3.format(trait.errMS2));
		}
		text.append("\n\n");

		if (trait.qtlEffects != null) {
			text.append("QTL Effects\n");
			if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
				text.append("Genotype\tMean\t\t\ts.e.");
				text.append("\n");
				String[] labels = { "Const", "M2", "M3", "M4", "M6", "M7", "M8" };
				for (int i = 0; i < 7; i++) {
					text.append(labels[i]);
					text.append("\t\t" + trait.qtlEffects[i]);
					text.append("\t\t" + trait.seEffects[i]);
					text.append("\n");
				}
			} else {
				// NONSNP
				text.append("Genotype   Mean      s.e.");
				if (trait.qtlEffects2 != null) {
					text.append("\t\tGenotype   Mean      s.e.");
				}
				text.append("\n");

				for (int i = 0; i < 6; i++) {
					text.append("  " + trait.qtlEffects[i]);
					if (trait.qtlEffects2 != null && i < 2) {
						text.append("\t\t  " + trait.qtlEffects2[i]);
					}
					text.append("\n");
				}
			}
		}

		// Lod details
		PermResult result = trait.getPermResult();
		if (result != null) {
			StringBuffer lod = new StringBuffer(1000);

			lod.append("90% = " + Prefs.d2.format(result.getSig90()) + "\n");
			lod.append("95% = " + Prefs.d2.format(result.getSig95()) + "\n\n");

			for (int i = 0; i < result.lodScores.length; i++) {
				if (result.lodScores[i] != 0.0) {
					lod.append(Prefs.d2.format(result.lodScores[i]) + "\n");
				}
			}

			lodDetails.setText(lod.toString());
			lodDetails.setCaretPosition(0);
		} else {
			lodDetails.setText("");
		}

		setsimple(trait);
		details.setText(text.toString());
		currentTrait = trait;
		currentTraitI = traitList.getSelectedIndex();
		toolbar.enableButtons(true, qtlResult.getTraitFile());

		splits.setTopComponent(getChart(trait));
	}

	private void setsimple(Trait trait) {
		if (AppFrame.tpmmode == AppFrame.TPMMODE_QTL) {
			DefaultListModel<String> simplemodelModel = new DefaultListModel<String>();
			simplemodelModel.addElement("Model       Adj R^2   SIC");
			for (String l : trait.getBestSimpleModels()) {
				simplemodelModel.addElement(l);
			}
			modelList = new JList<String>(simplemodelModel);
			modelList.addListSelectionListener(this);
			modelList.setFont(new Font("Monospaced", Font.PLAIN, 11));
			modelList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			simpleDetails = new JScrollPane(modelList);
			// simple_details.setFont(new Font("Monospaced", Font.PLAIN, 11));
			simpleDetails.setPreferredSize(new Dimension(200, 50));
			simplesplit.setLeftComponent(simpleDetails);
		} else {
			setTrait_simple(trait, simpleright);
		}
	}

	private void setTrait_simple(Trait trait, JTextArea ta) {
		if (trait == null || trait.qtlEffects == null) {
			ta.setText("");
			return;
		}

		StringBuffer text = new StringBuffer(1000);

		text.append("Model           sig     lod     R2      mean_1  se_1    mean_2  se_2\n");
		for (int i = 0; i < 10; i++) {
			String line = (i + 1) + " " + getModel(i + 1);
			while (line.length() < 15) {
				line += " ";
			}
			for (int j = 0; j < 7; j++) {
				if (j <= 5) {
					if (trait.modelScores[i][j] >= 0) {
						line += " ";
					}
					line += Prefs.d3.format(trait.modelScores[i][j]);
				} else if (trait.modelScoresExtra != null) {
					if (trait.modelScoresExtra[i][0] >= 0) {
						line += " ";
					}
					line += Prefs.d3.format(trait.modelScoresExtra[i][0]);
				} else {
					line += "N/A";
				}
				while (line.length() < 15 + (j + 1) * 8) {
					line += " ";
				}
			}
			text.append(line + "\n");
		}
		ta.setText(text.toString());
	}

	private JPanel getChart(Trait trait) {
		JFreeChart chart = ChartFactory.createXYLineChart(null, "Position (cM)", "LOD Score", null,
				PlotOrientation.VERTICAL, true, true, false);

		setChartData(chart, trait);

		RenderingHints rh = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_OFF);
		chart.setRenderingHints(rh);
		chart.removeLegend();

		XYPlot plot = chart.getXYPlot();
		plot.setBackgroundPaint(new Color(255, 255, 220));
		plot.setDomainGridlinePaint(new Color(128, 128, 128));
		plot.setRangeGridlinePaint(new Color(128, 128, 128));

		ValueAxis axis = plot.getRangeAxis();
		if (trait.maxLOD <= 3) {
			axis.setUpperBound(3);
		}

		PermResult result = trait.getPermResult();
		if (result != null) {
			float[] dashPattern = { 5, 5 };
			BasicStroke s1 = new BasicStroke(1, BasicStroke.CAP_BUTT, 
					BasicStroke.JOIN_MITER, 10, dashPattern, 0);

			ValueMarker m1 = new ValueMarker(result.getSig90(), new Color(0, 0, 60), s1, null, null, 1.0f);
			ValueMarker m2 = new ValueMarker(result.getSig95(), new Color(0, 0, 60), s1, null, null, 1.0f);

			plot.addRangeMarker(m1);
			plot.addRangeMarker(m2);

			if (result.getSig95() > trait.maxLOD && result.getSig95() >= 3) {
				axis.setUpperBound(result.getSig95() * (1.05));
			}
		}

		chartPanel = new ChartPanel(chart);
		chartPanel.setPopupMenu(null);
		return chartPanel;
	}

	private void setChartData(JFreeChart chart, Trait trait) {
		XYSeriesCollection data = new XYSeriesCollection();
		XYSeries series1 = new XYSeries("Position vs LOD Score");
		XYSeries series2 = new XYSeries("Position vs 'simple'");

		for (int i = 0; i < trait.getPositions().size(); i++) {
			series1.add(trait.getPositions().get(i), trait.getLODs().get(i));
		}

		chart.getXYPlot().setDataset(data);

		// If altQTL has been run, plot its data too
		if (trait.getLODs2() != null && trait.getLODs2().size() > 0) {
			for (int i = 0; i < trait.getPositions().size(); i++) {
				series2.add(trait.getPositions().get(i), trait.getLODs2().get(i));
			}

			data.addSeries(series2);
		}

		data.addSeries(series1);
	}

	void savePNG() {
		try {
			chartPanel.doSaveAs();
		} catch (Exception e) {
			doe.MsgBox.msg("TetraploidMap could not save the chart due to the " + "following error:\n" + e,
					doe.MsgBox.ERR);
		}
	}

	void saveTXT() {
		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		fc.setDialogTitle("Save QTL Results");

		while (fc.showSaveDialog(MsgBox.frm) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			// Make sure it has an appropriate extension
			if (!file.exists()) {
				if (file.getName().indexOf(".") == -1) {
					file = new File(file.getPath() + ".csv");
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
			saveTXTFile(file);
			return;
		}
	}

	void saveTXTFile(File file) {
		Trait trait = currentTrait;

		try {
			BufferedWriter out = new BufferedWriter(new FileWriter(file));

			for (int i = 0; i < trait.getPositions().size(); i++) {
				out.write(trait.getPositions().get(i) + ", " + trait.getLODs().get(i));
				if (trait.getLODs2() != null) {
					out.write(", " + trait.getLODs2().get(i));
				}
				out.newLine();
			}

			out.close();
		} catch (Exception e) {
			doe.MsgBox.msg("TetraploidMap could not save the chart due to the " + "following error:\n" + e,
					doe.MsgBox.ERR);
		}
	}

	private String getModel(int i) {
		switch (i) {
			case 0:
				return "Full Model";
			case 1:
				return "C1 vs rest";
			case 2:
				return "C2 vs rest";
			case 3:
				return "C3 vs rest";
			case 4:
				return "C4 vs rest";
			case 5:
				return "Q12 vs rest";
			case 6:
				return "Q13 vs rest";
			case 7:
				return "Q14 vs rest";
			case 8:
				return "Q23 vs rest";
			case 9:
				return "Q24 vs rest";
			case 10:
				return "Q34 vs rest";
			default:
				return "";
		}
	}

	void runRescan() {
		int model = getSelectedModel(false, getBestModel());
		if (model == -1) return;

		int cmLength = (int) order.getDistanceTotal();

		AltQtlDialog dialog = new AltQtlDialog(MsgBox.frm, 
				qtlResult.getTraitFile(), currentTrait, model, cmLength);
		if (dialog.isOK()) {
			displayTrait(currentTrait);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}

	}

	void runSNPQTLRescan() {
		int cmLength = (int) order.getDistanceTotal();
		SnpQtlSimpleDialog dialog = new SnpQtlSimpleDialog(MsgBox.frm, currentTraitI, currentTrait, cmLength,
				qtlResult);
		if (dialog.isOK()) {
			displayTrait(currentTrait);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}

	}

	void runPerm() {
		Object[] values = { "Full Model", "Reduced Model" };
		Object selected = JOptionPane.showInputDialog(MsgBox.frm, 
				"Please select which model to use:", "Select Model",
				JOptionPane.QUESTION_MESSAGE, null, values, values[0]);

		if (selected == null) return;

		// Full model or reduced?
		boolean fullModel = selected == values[0];

		int cmLength = (int) order.getDistanceTotal();
		String tName = currentTrait.getName();

		PermDialog dialog = new PermDialog(MsgBox.frm, qtlResult.getTraitFile(), fullModel, tName, cmLength);
		if (dialog.isOK()) {
			PermResult result = dialog.getResult();
			currentTrait.setPermResult(result);

			displayTrait(currentTrait);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}

	}

	void runSNPQTLPerm() {

		String tName = currentTrait.getName();

		SNPPermDialog dialog = new SNPPermDialog(MsgBox.frm, qtlResult.getTraitFile(), tName, order);
		if (dialog.isOK()) {
			PermResult result = dialog.getResult();
			currentTrait.setPermResult(result);

			displayTrait(currentTrait);
			AppFrameMenuBar.aFileSave.setEnabled(true);
		}

	}

	// Works out which of the simple models is a suitable replacement for the
	// Full Model (if any)
	private int getBestModel() {
		int bestModel = 0;
		float bestScore = 0;

		for (int i = 0; i < 10; i++) {
			float score = currentTrait.modelScores[i][0];

			if (score >= 0.05 && score > bestScore) {
				bestModel = i;
				bestScore = score;
			}
		}

		// System.out.println("Best is " + (bestModel));

		return bestModel;
	}

	// Displays a dialog on screen that allows the user to select from the
	// available models. allowZero=Full Model available, best=initial selection
	private int getSelectedModel(boolean allowZero, int best) {
		int length = allowZero ? 11 : 10;
		Object[] values = new Object[length];

		int m = allowZero ? 0 : 1;
		for (int i = 0; i < length; i++) {
			values[i] = getModel(m++);
		}

		Object selected = JOptionPane.showInputDialog(MsgBox.frm, 
				"Please select which model to use:", "Select Model",
				JOptionPane.QUESTION_MESSAGE, null, values, values[best]);

		if (selected == null) return -1;

		for (int i = 0; i < length; i++) {
			if (selected == values[i]) {
				if (allowZero) {
					return i;
				} else {
					return i + 1;
				}
			}
		}

		return -1;
	}
}