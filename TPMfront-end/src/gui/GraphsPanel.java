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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import org.apache.commons.collections15.Factory;
import org.apache.commons.collections15.Transformer;
import data.Cluster;
import data.Dendrogram.DenNode;
import edu.uci.ics.jung.algorithms.layout.BalloonLayout;
import edu.uci.ics.jung.algorithms.layout.PolarPoint;
import edu.uci.ics.jung.algorithms.layout.RadialTreeLayout;
import edu.uci.ics.jung.graph.Forest;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.visualization.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.Layer;
import edu.uci.ics.jung.visualization.VisualizationImageServer;
import edu.uci.ics.jung.visualization.VisualizationServer;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.control.EditingModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ScalingControl;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.transform.MutableTransformer;
import edu.uci.ics.jung.visualization.transform.MutableTransformerDecorator;

/** GraphsPanel().
 * 
 * <p>TV: This class is called from DendrogramsNode in order to construct the panel which
 * displays the Dendrogram and the DendrogramChart on a DendrogramCanvas. This panel is
 * displayed when the option "Dendrograms" is selected in the NavPanel.
 */
public class GraphsPanel extends JPanel {

	private static final long serialVersionUID = -4147730595796800970L;
	private Forest<TreeNode, TreeNode> graph;
	private VisualizationViewer<TreeNode, TreeNode> vv;
	private VisualizationServer.Paintable balloonrings;
	private VisualizationServer.Paintable radialrings;
	private BalloonLayout<TreeNode, TreeNode> balloonLayout;
	private RadialTreeLayout<TreeNode, TreeNode> radialLayout;
	private Float thresh = 0.5f;
	private Float sensitivity = 0.5f;
	private JLabel label;
	public GraphZoomScrollPane zoomScrollPane;
	GraphToolBar toolbar;
	Cluster cluster;
	public ScalingControl scaler = new CrossoverScalingControl();

	/** TV constructor for DendrogramsPanel to be called when clustering SNP marker data.
	 * 
	 */
	public GraphsPanel(Cluster cluster, int tpmmode) {
		this.cluster = cluster;
		graph = cluster.getGraph(); 
		
		final Factory<TreeNode> vertexFactory = new Factory<TreeNode>() {
			TreeNode treeVertice;

			public TreeNode create() {
				return treeVertice;
			}
		};
		final Factory<TreeNode> edgeFactory = new Factory<TreeNode>() {
			TreeNode treeEdge;

			public TreeNode create() {
				return treeEdge;
			}
		};

		balloonLayout = new BalloonLayout<TreeNode, TreeNode>(graph);
		radialLayout = new RadialTreeLayout<TreeNode, TreeNode>(
				(Forest<TreeNode, TreeNode>) graph, 10000, 10000);
		vv = new VisualizationViewer<TreeNode, TreeNode>(radialLayout);
		vv.setBackground(Color.white);
		vv.setVertexToolTipTransformer(vertexString);
		vv.setEdgeToolTipTransformer(edgeString);
		vv.getRenderContext().setArrowFillPaintTransformer(arrowFillColor);
		vv.getRenderContext().setArrowDrawPaintTransformer(arrowColor);
		vv.getRenderContext().setEdgeDrawPaintTransformer(edgeColor);

		balloonrings = new BalloonRings(balloonLayout);
		radialrings = new RadialRings(radialLayout);

		EditingModalGraphMouse<TreeNode, TreeNode> gm = new EditingModalGraphMouse<TreeNode, TreeNode>(
				vv.getRenderContext(), vertexFactory, edgeFactory);
		gm.add(new ReEditingPopupGraphMousePlugin<TreeNode, TreeNode>(vertexFactory, edgeFactory));
		gm.setMode(ModalGraphMouse.Mode.TRANSFORMING);
		vv.setGraphMouse(gm);

		vv.getRenderContext().setVertexFillPaintTransformer(vertexColor);
		vv.getRenderContext().setVertexShapeTransformer(vertexSize);
		vv.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line<TreeNode, TreeNode>());

		// final GraphZoomScrollPane
		zoomScrollPane = new GraphZoomScrollPane(this.vv);
		this.setLayout(new BorderLayout());
		this.add(zoomScrollPane, BorderLayout.CENTER);

		this.toolbar = new GraphToolBar(this);// , canvas, chart);
		this.add(toolbar, BorderLayout.EAST);

		this.label = new JLabel("<html><table><tr><td> Similarity: " 
				+ Prefs.d3.format(this.getThreshold())
				+ "</td><td> Groups: " 
				+ cluster.getAvLnkDendrogram().getGroupCount(this.getThreshold())
				+ "</td><td>        </td><td> Distinctiveness: " 
				+ Prefs.d3.format(this.getSensitivity())
				+ "</td><td> Subgroups: " 
				+ cluster.getAvLnkDendrogram().getWeakEdgeCount(this.getSensitivity())
				+ "</td></tr></table></html>");
		this.label.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
		this.add(label, BorderLayout.SOUTH);

		this.setVisible(true);
	}

	// TV vertex to String transformer for displaying ToolTip on Mouse Over
	Transformer<TreeNode, String> vertexString = new Transformer<TreeNode, String>() {
		public String transform(TreeNode i) {
			DenNode dNode = (DenNode) ((DefaultMutableTreeNode) i).getUserObject();
			if (i.isLeaf()) {
				return "(" + dNode.getCMarker().marker.getPrefix() + ") : "
						+ dNode.getCMarker().marker.getName();
			} else {
				return "similarity = " + dNode.getSimilarity();
			}
		}
	};

	// TV edge to String transformer for displaying ToolTip on Mouse Over
	Transformer<TreeNode, String> edgeString = new Transformer<TreeNode, String>() {
		public String transform(TreeNode i) {
			DenNode dNode = (DenNode) ((DefaultMutableTreeNode) i).getUserObject();

			DenNode pdNode = (DenNode) ((DefaultMutableTreeNode) i.getParent()).getUserObject();
			
			if (i.isLeaf()) {
				return "(" + dNode.getCMarker().marker.getPrefix() + ") : " 
						+ dNode.getCMarker().marker.getName();
			} else {
				return "distinctiveness = " + (dNode.getSimilarity() - pdNode.getSimilarity());
			}
		}
	};

	Transformer<TreeNode, Paint> arrowColor = new Transformer<TreeNode, Paint>() {
		public Paint transform(TreeNode i) {
			return Color.LIGHT_GRAY;
		}
	};
	Transformer<TreeNode, Paint> arrowFillColor = new Transformer<TreeNode, Paint>() {
		public Paint transform(TreeNode i) {
			return Color.LIGHT_GRAY;
		}
	};

	/** TODO: re-implement the algorithm from vertexColor.
	 * Vertice Coloring algorithm executed before the vertexColor Transformer is
	 * called
	 * TV same algorithm initially set as a vertexColor Transformer COPY FROM
	 * PROCESSCLUSTERRESULTS
	 */
	public void colorizeTree(TreeNode root, float threshold) {
		// System.out.println("___GraphsPanel: COLORIZE TREE!");
		Color customColor = Color.WHITE;

		// int groupCount =
		// cluster.getAvLnkDendrogram().getGroupCount(threshold);
		// int counter = groupCount;

		// traverese the tree
		// Enumeration en =
		// ((DefaultMutableTreeNode)root).depthFirstEnumeration();
		@SuppressWarnings("unchecked")
		Enumeration<DefaultMutableTreeNode> en = ((DefaultMutableTreeNode) root).breadthFirstEnumeration();
		while (en.hasMoreElements()) {
			// Unfortunately the enumeration isn't genericised so we need to
			// downcast
			// when calling nextElement():
			DefaultMutableTreeNode node = en.nextElement();
			DenNode dNode = (DenNode) node.getUserObject();
			Integer level = node.getLevel();

			if (level == 0) {
				dNode.setColor(customColor);
			} else if (level != 0) {
				DenNode pdNode = (DenNode) ((DefaultMutableTreeNode) (node.getParent())).getUserObject();
				if ((dNode.getSimilarity() > threshold) && (pdNode.getColor().equals(Color.WHITE))) {
					dNode.setColor(Color.getHSBColor((float) 
							(Math.cos(dNode.getSimilarity() * 90) * 360), 0.8f, 0.8f));
				} else {
					dNode.setColor(pdNode.getColor());
				}
				// TV
				if (((dNode.getSimilarity() - pdNode.getSimilarity()) > this.getSensitivity())) {
					dNode.setEdgeColor(Color.BLACK);
				}
			}
		}
	}

	/** TV method which returns a unique color every time it is called.
	 * 
	 */
	public Color getUniqueColor(int colorSeed) {
		Random r = new Random(colorSeed);
		Color c = new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255));
		return c;
	}

	/** detectWeakEdges().
	 * 
	 */
	public void detectWeakEdges(TreeNode root, float sensitivity) {
		Color customColor = Color.WHITE;

		// TV get a snapshot of original colors for each node with singleton
		Color prevColor = null;

		@SuppressWarnings("unchecked")
		Enumeration<DefaultMutableTreeNode> en = ((DefaultMutableTreeNode) root).breadthFirstEnumeration();
		while (en.hasMoreElements()) {
			DefaultMutableTreeNode node = en.nextElement();
			DenNode dNode = (DenNode) node.getUserObject();
			Integer level = node.getLevel();

			if (prevColor == null) {
				prevColor = dNode.getColor();
			}

			if (level == 0) {
				dNode.setColor(customColor);
			} else if (level != 0) {
				DenNode pdNode = (DenNode) ((DefaultMutableTreeNode) (node.getParent())).getUserObject();
				if ((dNode.getSimilarity() > this.getThreshold()) 
						&& (pdNode.getColor().equals(Color.WHITE))) {
					dNode.setColor(Color.getHSBColor((float) 
							(Math.cos(dNode.getSimilarity() * 90) * 360), 0.8f, 0.8f));
				} else {
					dNode.setColor(pdNode.getColor());
				}
				dNode.resetEdgeColor();
				if (((dNode.getSimilarity() - pdNode.getSimilarity()) > sensitivity)) {
					dNode.setEdgeColor(Color.RED);
				}
			}
		}
	}

	Transformer<TreeNode, Paint> edgeColor = new Transformer<TreeNode, Paint>() {
		public Paint transform(TreeNode i) {
			DenNode dNode = (DenNode) ((DefaultMutableTreeNode) i).getUserObject();
			return dNode.getEdgeColor();
		}
	};

	/*
	 * Transforms the shape of the edge arrow Transformer<TreeNode, Stroke>
	 * edgeShape = new Transformer<TreeNode, Stroke>() { public Stroke
	 * transform(TreeNode i) { DenNode dNode = (DenNode)
	 * ((DefaultMutableTreeNode)i).getUserObject(); return
	 * dNode.getEdgeStroke(); } };
	 */

	// TV vertex to paint transformer
	/*
	 * DONE: Write TV Algorithm for Visual Clustering with the following steps:
	 * 1. Initially assume 16 clusters for level==4 2. Merge clusters that have
	 * the same parent and its similarity > 0.5 (or a user input value) 3.
	 * Repeat step 2 until no more merging occurs. 4. Paint the new clusters
	 * with the same color
	 * 
	 * RRTODO: First apply transformation to all nodes and in the end return the
	 * color ???? RRTODO: everytime initialize the graph so that all dNodes are
	 * set to white ???? TODO: make the Transformer to complete faster by
	 * correcting the false recursive behaviour
	 */
	Transformer<TreeNode, Paint> vertexColor = new Transformer<TreeNode, Paint>() {
		public Paint transform(TreeNode i) {
			DenNode dNode = (DenNode) ((DefaultMutableTreeNode) i).getUserObject();
			return dNode.getColor();
		}
	};

	Transformer<TreeNode, Shape> vertexSize = new Transformer<TreeNode, Shape>() {
		public Shape transform(TreeNode i) {

			DenNode dNode = (DenNode) ((DefaultMutableTreeNode) i).getUserObject();
			Ellipse2D circle = new Ellipse2D.Double(-5, -5, 10, 10);
			Rectangle2D rectang = new Rectangle2D.Double(-5, -5, 10, 10);
			if (!i.isLeaf()) {
				return AffineTransform.getScaleInstance(3 - 2 * dNode.getSimilarity(), 
						3 - 2 * dNode.getSimilarity()).createTransformedShape(circle);
			} else if (i.isLeaf()) {
				return AffineTransform.getScaleInstance(3, 3).createTransformedShape(rectang);
			} else {
				return circle;
			}
		}
	};
	// TV new stroke Transformer for the edges
	float[] dash = { 10.0f };
	final Stroke edgeStroke = new BasicStroke(1.0f, BasicStroke.CAP_BUTT, 
			BasicStroke.JOIN_MITER, 10.0f, dash, 0.0f);
	Transformer<String, Stroke> edgeStrokeTransformer = new Transformer<String, Stroke>() {
		public Stroke transform(String s) {
			return edgeStroke;
		}
	};

	/** Save Graph as JPEG.
	 * 
	 */
	public void writeJpegImage(File file) {
		int width = vv.getWidth();
		int height = vv.getHeight();

		BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
		Graphics2D graphics = bi.createGraphics();
		vv.paint(graphics);
		graphics.dispose();

		try {
			ImageIO.write(bi, "jpeg", file);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/** Save Full Graph as JPEG.
	 * 
	 */
	public void writeFullJpegImage(File file) {

		VisualizationImageServer<TreeNode, TreeNode> vis = new VisualizationImageServer<TreeNode, TreeNode>(
				vv.getGraphLayout(), vv.getGraphLayout().getSize());

		// Configure the VisualizationImageServer the same way as the
		// VisualizationViewer
		vis.setBackground(Color.white);
		// vis.setVertexToolTipTransformer(vertexString);
		vis.getRenderContext().setArrowFillPaintTransformer(arrowFillColor);
		vis.getRenderContext().setArrowDrawPaintTransformer(arrowColor);
		vis.getRenderContext().setEdgeFillPaintTransformer(edgeColor);
		// TV: apply color and shape transformers
		vis.getRenderContext().setVertexFillPaintTransformer(vertexColor);
		vis.getRenderContext().setVertexShapeTransformer(vertexSize);
		vis.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line<TreeNode, TreeNode>());

		// TV: scale the tree graph to fit the panel area when first costructed
		//
		// ScalingControl scaler = new CrossoverScalingControl();
		vis.scaleToLayout(scaler);

		// Create the buffered image
		BufferedImage image = (BufferedImage) vis
				.getImage(
						new Point2D.Double(vv.getGraphLayout().getSize().getWidth() / 2,
								vv.getGraphLayout().getSize().getHeight() / 2),
						new Dimension(vv.getGraphLayout().getSize()));

		// Write image to a png file
		// File outputfile = new File("graph.png");

		try {
			ImageIO.write(image, "png", file); // outputfile);
		} catch (IOException e) {
			// Exception handling
			e.printStackTrace();
		}
	}

	/** open PrinterDialog().
	 * 
	 */
	public void print() {
		class VVP implements Printable {
			VisualizationViewer<TreeNode, TreeNode> vv;

			public VVP(VisualizationViewer<TreeNode, TreeNode> vv) {
				this.vv = vv;
			}

			public int print(Graphics g, PageFormat pf, int pageIndex) {
				if (pageIndex > 0) {
					return Printable.NO_SUCH_PAGE;
				} else {
					Graphics2D g2d = (Graphics2D) g;
					vv.setDoubleBuffered(false);
					g2d.translate(pf.getImageableX(), pf.getImageableX());
					vv.paint(g2d);
					vv.setDoubleBuffered(true);
					return (Printable.PAGE_EXISTS);
				}
			}
		}

		Printable[] toPrint = { new VVP(vv) };
		new PrinterDialog(toPrint);
	}

	public VisualizationViewer<TreeNode, TreeNode> getVV() {
		return this.vv;
	}

	public RadialTreeLayout<TreeNode, TreeNode> getRadialLayout() {
		return this.radialLayout;
	}

	public BalloonLayout<TreeNode, TreeNode> getBalloonLayout() {
		return this.balloonLayout;
	}

	public VisualizationServer.Paintable getBalloonRings() {
		return this.balloonrings;
	}

	public VisualizationServer.Paintable getRadialRings() {
		return this.radialrings;
	}

	public void setThreshold(Float thresh) {
		this.thresh = thresh;
		// this.colorizeGraph(root, thresh);
	}

	public Float getThreshold() {
		return this.thresh;
	}

	// TV Sensitivity
	public void setSensitivity(Float sensitivity) {
		this.sensitivity = sensitivity;
		// this.colorizeGraph(root, thresh);
	}

	public Float getSensitivity() {
		return this.sensitivity;
	}

	public Graph<TreeNode, TreeNode> getGraph() {
		return this.graph;
	}

	/*
	 * public int print(Graphics graphics, PageFormat pageFormat, int pageIndex)
	 * throws PrinterException { if (pageIndex > 0) { return
	 * Printable.NO_SUCH_PAGE; } else { Graphics2D g2d = (Graphics2D) graphics;
	 * vv.setDoubleBuffered(false); g2d.translate(pageFormat.getImageableX(),
	 * pageFormat.getImageableY()); vv.paint(g2d); vv.setDoubleBuffered(true);
	 * 
	 * return (Printable.PAGE_EXISTS); } }
	 */

	// RADIAL RINGS
	class RadialRings implements VisualizationServer.Paintable {

		Collection<Double> depths;
		private RadialTreeLayout<TreeNode, TreeNode> radialLayout;

		public RadialRings(RadialTreeLayout<TreeNode, TreeNode> radialLayout) {
			this.radialLayout = radialLayout;
			depths = getDepths(this.radialLayout);
		}

		private Collection<Double> getDepths(RadialTreeLayout<TreeNode, TreeNode> radialLayout) {
			Set<Double> depths = new HashSet<Double>();
			// TV System.out.println("____GraphsPanel.RadialRings.getDepths()
			// depths = " + depths.size());
			Map<TreeNode, PolarPoint> polarLocations = radialLayout.getPolarLocations();
			// TV System.out.println("____GraphsPanel.RadialRings.getDepths()
			// polarLocations = " + polarLocations.size());
			for (TreeNode v : graph.getVertices()) {
				PolarPoint pp = polarLocations.get(v);
				depths.add(pp.getRadius());
			}
			// TV System.out.println("____GraphsPanel.RadialRings.getDepths()
			// depths = " + depths.size());
			return depths;
		}

		public void paint(Graphics g) {
			g.setColor(Color.lightGray);

			Graphics2D g2d = (Graphics2D) g;
			Point2D center = radialLayout.getCenter();

			Ellipse2D ellipse = new Ellipse2D.Double();
			for (double d : depths) {
				ellipse.setFrameFromDiagonal(center.getX() - d, center.getY() - d, center.getX() + d,
						center.getY() + d);
				Shape shape = vv.getRenderContext().getMultiLayerTransformer().getTransformer(Layer.LAYOUT)
						.transform(ellipse);
				g2d.draw(shape);
			}
		}

		public boolean useTransform() {
			return true;
		}
	}

	// BALLOON RINGS
	class BalloonRings implements VisualizationServer.Paintable {

		BalloonLayout<TreeNode, TreeNode> layout;

		public BalloonRings(BalloonLayout<TreeNode, TreeNode> layout) {
			this.layout = layout;
		}

		public void paint(Graphics g) {
			g.setColor(Color.gray);

			Graphics2D g2d = (Graphics2D) g;

			Ellipse2D ellipse = new Ellipse2D.Double();
			for (TreeNode v : layout.getGraph().getVertices()) {
				Double radius = layout.getRadii().get(v);
				if (radius == null) continue;
				Point2D p = layout.transform(v);
				ellipse.setFrame(-radius, -radius, 2 * radius, 2 * radius);
				AffineTransform at = AffineTransform.getTranslateInstance(p.getX(), p.getY());
				Shape shape = at.createTransformedShape(ellipse);

				MutableTransformer viewTransformer = vv.getRenderContext().getMultiLayerTransformer()
						.getTransformer(Layer.VIEW);

				if (viewTransformer instanceof MutableTransformerDecorator) {
					shape = vv.getRenderContext().getMultiLayerTransformer().transform(shape);
				} else {
					shape = vv.getRenderContext().getMultiLayerTransformer().transform(Layer.LAYOUT, shape);
				}

				g2d.draw(shape);
			}
		}

		public boolean useTransform() {
			return true;
		}
	}

	public Cluster getCluster() {
		return this.cluster;
	}

	/** setLabelState().
	 * 
	 */
	public void setLabelState() {
		label.setText("<html><table><tr><td> Similarity: " 
				+ Prefs.d3.format(this.getThreshold()) + "</td><td> Groups: "
				+ cluster.getAvLnkDendrogram().getGroupCount(this.getThreshold())
				+ "</td><td>        </td><td> Distinctiveness: " + Prefs.d3.format(this.getSensitivity())
				+ "</td><td> Subgroups: " 
				+ cluster.getAvLnkDendrogram().getWeakEdgeCount(this.getSensitivity())
				+ "</td></tr></table></html>");
	}

}
