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

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Point2D;
import java.util.Set;
import javax.swing.AbstractAction;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import org.apache.commons.collections15.Factory;
import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.DirectedGraph;
import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.UndirectedGraph;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.VisualizationViewer;
import edu.uci.ics.jung.visualization.control.AbstractPopupGraphMousePlugin;
import edu.uci.ics.jung.visualization.picking.PickedState;

/**
 * a plugin that uses popup menus to create vertices, undirected edges, and
 * directed edges.
 * 
 * @author Tom Nelson
 *
 */
public class ReEditingPopupGraphMousePlugin<V, E> extends AbstractPopupGraphMousePlugin 
	implements MouseListener {

	protected Factory<V> vertexFactory;
	protected Factory<E> edgeFactory;
	protected JPopupMenu popup = new JPopupMenu();

	public ReEditingPopupGraphMousePlugin(Factory<V> vertexFactory, Factory<E> edgeFactory) {
		this.vertexFactory = vertexFactory;
		this.edgeFactory = edgeFactory;
	}

	@SuppressWarnings({ "unchecked", "serial" })
	protected void handlePopup(MouseEvent e) {
		final VisualizationViewer<V, E> vv = (VisualizationViewer<V, E>) e.getSource();
		final Layout<V, E> layout = vv.getGraphLayout();
		final Graph<V, E> graph = layout.getGraph();
		final Point2D p = e.getPoint();
		final Point2D ivp = p;
		GraphElementAccessor<V, E> pickSupport = vv.getPickSupport();

		if (pickSupport != null) {
			final V vertex = pickSupport.getVertex(layout, ivp.getX(), ivp.getY());
			final E edge = pickSupport.getEdge(layout, ivp.getX(), ivp.getY());
			final PickedState<V> pickedVertexState = vv.getPickedVertexState();
			final PickedState<E> pickedEdgeState = vv.getPickedEdgeState();

			if (vertex != null) {
				Set<V> picked = pickedVertexState.getPicked();
				if (picked.size() > 0) {
					if (graph instanceof UndirectedGraph == false) {
						JMenu directedMenu = new JMenu("Create Directed Edge");
						popup.add(directedMenu);
						for (final V other : picked) {
							directedMenu.add(new AbstractAction("[" + other + "," + vertex + "]") {
								public void actionPerformed(ActionEvent e) {
									graph.addEdge(edgeFactory.create(), other, vertex, EdgeType.DIRECTED);
									vv.repaint();
								}
							});
						}
					}
					if (graph instanceof DirectedGraph == false) {
						JMenu undirectedMenu = new JMenu("Create Undirected Edge");
						popup.add(undirectedMenu);
						for (final V other : picked) {
							undirectedMenu.add(new AbstractAction("[" + other + "," + vertex + "]") {
								public void actionPerformed(ActionEvent e) {
									graph.addEdge(edgeFactory.create(), other, vertex);
									vv.repaint();
								}
							});
						}
					}
				}
				popup.add(new AbstractAction("Delete TETRA Vertex") {
					public void actionPerformed(ActionEvent e) {
						pickedVertexState.pick(vertex, false);
						graph.removeVertex(vertex);
						vv.repaint();
					}
				});
			} else if (edge != null) {
				popup.add(new AbstractAction("Delete Edge") {
					public void actionPerformed(ActionEvent e) {
						pickedEdgeState.pick(edge, false);
						graph.removeEdge(edge);
						vv.repaint();
					}
				});
			} else {
				popup.add(new AbstractAction("Create TETRA Vertex") {
					public void actionPerformed(ActionEvent e) {
						V newVertex = vertexFactory.create();
						graph.addVertex(newVertex);
						layout.setLocation(newVertex,
								vv.getRenderContext().getMultiLayerTransformer().inverseTransform(p));
						vv.repaint();
					}
				});
			}
			if (popup.getComponentCount() > 0) {
				popup.show(vv, e.getX(), e.getY());
			}
		}
	}

}
