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

import java.awt.Font;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Hashtable;
import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.TreeNode;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.layout.LayoutTransition;
import edu.uci.ics.jung.visualization.util.Animator;

public class GraphToolBar extends JToolBar {
	private static final long serialVersionUID = -450854356193609865L;
	private GraphsPanel panel;
	private JToggleButton /* bScale, */ bBalloon, bRadial/* , bTreeLayout */;
	private AbstractAction /* aScale, */ aZoomIn, aZoomOut, aSaveJpeg;// aRadial
	private JButton bZoomIn, bZoomOut, bSaveJpeg;
	final int SIM_MIN = 0;
	final int SIM_MAX = 1000;
	final int SIM_INIT = 500;
	JSlider simSlider;
	JSlider senSlider;
	
	JLabel l00;
	JLabel l01;
	JLabel l02 = new JLabel("0.2");
	JLabel l03 = new JLabel("0.3");
	JLabel l04 = new JLabel("0.4");
	JLabel l05 = new JLabel("0.5");
	JLabel l06 = new JLabel("0.6");
	JLabel l07 = new JLabel("0.7");
	JLabel l08 = new JLabel("0.8");
	JLabel l09 = new JLabel("0.9");
	JLabel l10 = new JLabel("1.0");

	// TV JLabel simLabel;

	GraphToolBar(final GraphsPanel panel) {
		this.panel = panel;

		createActions();

		setFloatable(false);
		setMargin(new Insets(0, 0, 0, 0));
		setBorderPainted(false);
		setOrientation(JToolBar.VERTICAL);

		bZoomIn = (JButton) AppFrameToolBar.getButton(false, null, "Zoom In Graph", 
				Icons.ZOOM_IN, aZoomIn);
		bZoomOut = (JButton) AppFrameToolBar.getButton(false, null, "Zoom Out Graph", 
				Icons.ZOOM_OUT, aZoomOut);

		// TV: Save as JPEG button
		bSaveJpeg = (JButton) AppFrameToolBar.getButton(false, null, "Save Graph as JPEG", 
				Icons.SAVE_IMAGE, aSaveJpeg);

		// BALLOON
		bBalloon = new JToggleButton(Icons.BALLOON);
		bBalloon.setToolTipText("Stretched Tree View");

		bBalloon.addItemListener(new ItemListener() {

			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					panel.getVV().removePreRenderPaintable(panel.getRadialRings());
					bRadial.setSelected(false);
					bRadial.setEnabled(true);
					LayoutTransition<TreeNode, TreeNode> lt = 
							new LayoutTransition<TreeNode, TreeNode>(panel.getVV(), 
							panel.getRadialLayout(), panel.getBalloonLayout());
					Animator animator = new Animator(lt);
					animator.start();
					panel.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(
							Layer.LAYOUT).setToIdentity();
					panel.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(
							Layer.VIEW).setToIdentity();
					panel.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(
							Layer.VIEW).setScale(1.0, 1.0, panel.getVV().getCenter());
					panel.getVV().addPreRenderPaintable(panel.getBalloonRings());
					bBalloon.setEnabled(false);
				}
				panel.getVV().repaint();
				panel.zoomScrollPane.repaint();
			}
		});

		// RADIAL
		bRadial = new JToggleButton(Icons.RADIAL);
		bRadial.setToolTipText("Radial View");
		bRadial.setSelected(true);
		panel.getVV().addPreRenderPaintable(panel.getRadialRings());
		bRadial.setEnabled(false);

		bRadial.addItemListener(new ItemListener() {

			public void itemStateChanged(ItemEvent e) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					panel.getVV().removePreRenderPaintable(panel.getBalloonRings());
					bBalloon.setSelected(false);
					bBalloon.setEnabled(true);
					LayoutTransition<TreeNode, TreeNode> lt = 
							new LayoutTransition<TreeNode, TreeNode>(panel.getVV(), 
							panel.getBalloonLayout(), panel.getRadialLayout());
					Animator animator = new Animator(lt);
					animator.start();
					panel.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT)
							.setToIdentity();
					panel.getVV().getRenderContext().getMultiLayerTransformer().getTransformer(Layer.VIEW)
							.setToIdentity();
					panel.getVV().getRenderContext().getMultiLayerTransformer()
						.getTransformer(Layer.VIEW).setScale(1.0, 1.0, panel.getVV().getCenter());
					panel.getVV().addPreRenderPaintable(panel.getRadialRings());
					bRadial.setEnabled(false);
				}

				panel.getVV().repaint();
				panel.zoomScrollPane.repaint();
			}
		});

		simSlider = new JSlider(JSlider.HORIZONTAL, SIM_MIN, SIM_MAX, SIM_INIT);
		simSlider.setOrientation(VERTICAL);
		simSlider.setMajorTickSpacing(10);
		simSlider.setMinorTickSpacing(1);
		simSlider.setPaintLabels(true);
		simSlider.setPaintTicks(true);

		senSlider = new JSlider(JSlider.HORIZONTAL, SIM_MIN, SIM_MAX, SIM_INIT);
		senSlider.setOrientation(VERTICAL);
		senSlider.setMajorTickSpacing(10);
		senSlider.setMinorTickSpacing(1);
		senSlider.setPaintLabels(true);
		senSlider.setPaintTicks(true);

		l00 = new JLabel("0.0");
		l01 = new JLabel("0.1");
		l02 = new JLabel("0.2");
		l03 = new JLabel("0.3");
		l04 = new JLabel("0.4");
		l05 = new JLabel("0.5");
		l06 = new JLabel("0.6");
		l07 = new JLabel("0.7");
		l08 = new JLabel("0.8");
		l09 = new JLabel("0.9");
		l10 = new JLabel("1.0");
		// L00.setPreferredSize(new Dimension(3,3));
		Font f = new Font("Helvetica", Font.PLAIN, 10);
		l00.setFont(f);
		l01.setFont(f);
		l02.setFont(f);
		l03.setFont(f);
		l04.setFont(f);
		l05.setFont(f);
		l06.setFont(f);
		l07.setFont(f);
		l08.setFont(f);
		l09.setFont(f);
		l10.setFont(f);
		// L01.setFont(new Font(L01.getFont().getName(), Font.PLAIN,
		// Math.min((int)(L01.getFont().getSize() *
		// (double)L01.getWidth()/(double)L01.getFontMetrics(L01.getFont()).stringWidth(L01.getText())),
		// L01.getHeight())));

		Hashtable<Integer, JLabel> labelTable = new Hashtable<Integer, JLabel>();
		labelTable.put(new Integer(0), l00);
		// labelTable.put( new Integer( 50 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(100), l01);
		// labelTable.put( new Integer( 150 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(200), l02);
		// labelTable.put( new Integer( 250 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(300), l03);
		// labelTable.put( new Integer( 350 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(400), l04);
		// labelTable.put( new Integer( 450 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(500), l05);
		// labelTable.put( new Integer( 550 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(600), l06);
		// labelTable.put( new Integer( 650 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(700), l07);
		// labelTable.put( new Integer( 750 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(800), l08);
		// labelTable.put( new Integer( 850 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(900), l09);
		// labelTable.put( new Integer( 950 ), new
		// JLabel("1/2").setPreferredSize(new Dimension(8,8)) );
		labelTable.put(new Integer(1000), l10);

		simSlider.setLabelTable(labelTable);
		// simSlider.setPreferredSize(new Dimension(50, 7));
		simSlider.setMinorTickSpacing(10);
		simSlider.setMajorTickSpacing(50);
		// add listener
		simSlider.addChangeListener(new SliderListener());
		simSlider.setUI(new CustomSliderUI(simSlider));

		// TV: same configuration for second slider
		senSlider.setLabelTable(labelTable);
		senSlider.setMinorTickSpacing(10);
		senSlider.setMajorTickSpacing(50);
		// add listener
		senSlider.addChangeListener(new SliderListener2());
		senSlider.setUI(new CustomSlider2UI(senSlider));

		new Icons();
		Icon icon = Icons.WARNING;
		UIDefaults defaults = UIManager.getDefaults();
		defaults.put("Slider.verticalThumbIcon", icon);

		add(bZoomIn);
		add(bZoomOut);
		add(bRadial);
		add(bBalloon);

		add(bSaveJpeg);
		add(simSlider);
		add(senSlider);
	}

	class SliderListener implements ChangeListener {
		public void stateChanged(ChangeEvent e) {
			JSlider source = (JSlider) e.getSource();
			if (source.getValueIsAdjusting()) {

				int val = (int) source.getValue();
				float sim = (float) val / 1000;
				source.setToolTipText("Similarity = " + sim);
				panel.setThreshold(sim);

				panel.setLabelState();

				panel.colorizeTree(panel.getCluster().getAvLnkDendrogram().getNodes().get(0), sim);
				panel.getVV().repaint();
			}
		}
	}

	// Listener for second Slider
	class SliderListener2 implements ChangeListener {
		public void stateChanged(ChangeEvent e) {
			JSlider source = (JSlider) e.getSource();
			// This excecutes only after the mouse button gets released!!!
			// *** if (!source.getValueIsAdjusting()) {
			if (source.getValueIsAdjusting()) {
				int val = (int) source.getValue();
				float sen = (float) val / 1000;
				source.setToolTipText("Distinctiveness = " + sen);
				panel.setSensitivity(sen);
				panel.setLabelState();
				// TV*** panel.setLabelState2(sen);
				// TV
				// panel.colorizeTree(panel.getCluster().getAvLnkDendrogram().getNodes().get(0),
				// sen);
				// TV TODO: change edge look to indicate weak edges
				panel.detectWeakEdges(panel.getCluster().getAvLnkDendrogram().getNodes().get(0), sen);
				panel.getVV().repaint();
			}
		}
	}

	@SuppressWarnings("serial")
	private void createActions() {
		aZoomIn = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.scaler.scale(panel.getVV(), 1.1f, panel.getVV().getCenter());

			}
		};
		aZoomOut = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
				panel.scaler.scale(panel.getVV(), 1 / 1.1f, panel.getVV().getCenter());
			}
		};

		aSaveJpeg = new AbstractAction("") {
			public void actionPerformed(ActionEvent e) {
			}
		};
	}

}
