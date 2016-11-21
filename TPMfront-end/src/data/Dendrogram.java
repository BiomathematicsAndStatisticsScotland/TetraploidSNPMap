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

package data;

import java.awt.Color;
import java.io.PushbackReader;
import java.io.Serializable;
import java.io.StringReader;
import java.util.LinkedList;
import java.util.Random;
import java.util.StringTokenizer;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import pal.gui.NameColouriser;
import pal.misc.Identifier;
import pal.tree.Node;
import pal.tree.ReadTree;
import pal.tree.TreeParseException;

/** Dendrogram. SNP & NONSNP.
 * 
 * <p>Class that represents (in various forms) a dendrogram, both internally (using
 * the Swing tree classes for its structure), and visually (via PAL). The PAL
 * representation of the class is formed by converting the similarity-output
 * from CLUSTER into New Hampshire tree format.
 */
public class Dendrogram implements Serializable {
	static final long serialVersionUID = 4071739595558341040L;

	// Swing-structure tree
	private DefaultMutableTreeNode root;

	// PAL-structure tree
	private ReadTree palTree;

	// StringBuffer to (temporarily) hold the NH representation
	private StringBuffer nhStr = new StringBuffer(1000);

	private int colorSeed = 0;

	// TV
	public LinkedList<DefaultMutableTreeNode> nodes;

	/** Builds up a dendrogram tree using output from CLUSTER.
	 * 
	 * @param lGroup = linkage group containing the markers to use.
	 * @param denList = the pairwise similarity scores.
	 * @throws CreationException = exception thrown by createPALTree.
	 */
	public Dendrogram(LinkageGroup lGroup, LinkedList<String> denList) throws CreationException {
		// We build up groups of Nodes, adding them to the linked-list. Nodes
		// will be removed from the list and readded as a single parent node,
		// eventually leading to the list only containing the root node
		// LinkedList<DefaultMutableTreeNode> nodes =
		nodes = new LinkedList<DefaultMutableTreeNode>();

		// Create a node for each marker and add it to the list
		int i = 1;
		// System.out.println("lgroup sel count: " +
		// lGroup.getSelectedMarkerCount());
		for (CMarker cm : lGroup.getMarkers()) {
			if (cm.checked == false) continue;

			DenNode dNode = new DenNode(i++, cm, 0);
			nodes.add(new DefaultMutableTreeNode(dNode));
		}

		// Now traverse the list of similarity scores to group pairs of nodes
		// TV: It actually reconstructs the distance matrix, creating for each
		// marker a single parent (root) node with 2 children which are linked.
		// NOTE: DenNodes when first created and added to the single root tree
		// have the default Color.WHITE which is also
		// the color of the graph when first drawn. Later on the vertices take
		// their color from the execution of the Transformer
		//
		// System.out.println(i + " markers added to nodes..");
		for (String s : denList) {
			// System.out.println("dendo: " + s);

			StringTokenizer st = new StringTokenizer(s);
			st.nextToken();

			float dblDistance = Float.parseFloat(st.nextToken());
			// System.out.println(Prefs.d3.format(dblDistance));
			DefaultMutableTreeNode newNode = 
					new DefaultMutableTreeNode(new DenNode(i++, null, dblDistance));
			newNode.add(getNode(nodes, Integer.parseInt(st.nextToken())));
			newNode.add(getNode(nodes, Integer.parseInt(st.nextToken())));

			nodes.add(newNode);

		}
		// System.out.println("total nodes: " + i);

		// Now that we have a single node, use it to create the PAL tree
		root = nodes.get(0);
		// System.out.println("nodes.get(0)");
		createPALTree(lGroup);
		// System.out.println("createPalTree()");
	}

	// TV: method that returns the constructed tree of markers with a single
	// root nodes.get(0)
	// Except from using it to create the PAL dendrogram, we can also export it
	// and use it to
	// create the same structure as Graph using JUNG -> see
	// ProcessClusterResults.processGraph()
	public LinkedList<DefaultMutableTreeNode> getNodes() {
		return this.nodes;
	}

	// Returns the node whose "number" parameter matches the given index
	private DefaultMutableTreeNode getNode(LinkedList<DefaultMutableTreeNode> nodes, int index) {
		int i = 0;
		for (DefaultMutableTreeNode node : nodes) {
			if (((DenNode) node.getUserObject()).number == index) {
				return nodes.remove(i);
			}
			i++;
		}

		return null;
	}

	private void createPALTree(LinkageGroup lGroup) throws CreationException {
		// Build up the NH
		traverseNode(root, -1);

		// Get PAL to read it in
		try {
			StringReader reader = new StringReader(nhStr.toString());
			palTree = new ReadTree(new PushbackReader(reader));

			testPal(lGroup);
		} catch (TreeParseException e) {
			System.out.println(e);
			throw new CreationException(CreationException.NO_PAL_TREE);
		}

		// Clear the StringBuffer once finished with it
		nhStr.delete(0, nhStr.length() - 1);
		nhStr.trimToSize();
	}

	// Traverses the tree in order to build up a NH representation with suitable
	// branch lengths (determined from CLUSTER'S dblDistance (dd) scores)
	private void traverseNode(TreeNode node, float dd) {
		DenNode dNode = (DenNode) ((DefaultMutableTreeNode) node).getUserObject();

		// Child node: branch len = parent's dd / 2
		if (node.getChildCount() == 0) {
			nhStr.append(dNode.cm.safeName + ":" + (dd / 2));
			// System.out.println(dNode.cm.safeName + ":" + (dd/2));
		} else {
			nhStr.append("(");
			// System.out.println("(");
			traverseNode(node.getChildAt(0), dNode.dblDistance);
			nhStr.append(",");
			// System.out.println(",");
			traverseNode(node.getChildAt(1), dNode.dblDistance);
			// System.out.println(")");
			nhStr.append(")");

			if (dd != -1) {
				nhStr.append(":" + ((dd - dNode.dblDistance) / 2));
				// System.out.println(":" + ((dd - dNode.dblDistance)/2));
			} else {
				nhStr.append(";");
				// System.out.println(";");
			}
		}
		// TV
		// System.out.println("____Dendrogram.traverseNode() -> " +
		// nhStr.toString());
	}

	public ReadTree getPALTree() {
		return palTree;
	}

	public double getRootSimilarity() {
		return ((DenNode) root.getUserObject()).similarity;
	}

	public double getRootDistance() {
		return ((DenNode) root.getUserObject()).dblDistance;
	}

	// TV TODO: Expand the countGroupCount method to also take into account the
	// distinctiveness threshold,
	// that acts in a different dimension, for calculating the actual number of
	// clusters.
	public int getWeakEdgeCount(double h) {
		return countWeakEdges(root, 0, h);
	}

	// TV
	private int countWeakEdges(DefaultMutableTreeNode node, int count, double het) {
		DenNode dNode = (DenNode) node.getUserObject();

		// TV DenNode that stores the Parent node
		DenNode pdNode;

		if (!node.isRoot()) {
			pdNode = (DenNode) ((DefaultMutableTreeNode) (node.getParent())).getUserObject();
		} else {
			pdNode = dNode;
		}

		// Each time that a node with larger similarity is found, the
		// calculation stops
		// and the counter is increased by one. Now we will also have to check
		// the similarity
		// distance this node has with its parent node. If this is larger than
		// the 'het' threshold,
		// it means that the branch should be added to the groups number only
		// once and not twice.
		// Therefore weather this subcluster has been identified already or not,
		// should be stored in an
		// additional counter for overlaps.
		if (dNode.getSimilarity() - pdNode.getSimilarity() > het) {
			// TV TODO: add subcluster overlap counter when overlap occurs
			return ++count;
		} else if (dNode.cm == null) {
			// Traverse left
			count = countWeakEdges((DefaultMutableTreeNode) node.getChildAt(0), count, het);
			// Traverse right
			count = countWeakEdges((DefaultMutableTreeNode) node.getChildAt(1), count, het);
		}

		// System.out.println("Weak Edges = " + count);

		return count;
	}

	// Returns the number of unique groups at the given level of similarity
	public int getGroupCount(double s) {
		return countGroups(root, 0, s, null);
	}

	private int countGroups(DefaultMutableTreeNode node, int count, double sim, NameColouriser nc) {
		DenNode dNode = (DenNode) node.getUserObject();

		if (dNode.similarity >= sim) {
			if (nc != null) {
				setColouriserNames(node, nc);
				colorSeed++;
			}

			return ++count;
		} else if (dNode.cm == null) {
			// Traverse left
			count = countGroups((DefaultMutableTreeNode) node.getChildAt(0), count, sim, nc);
			// Traverse right
			count = countGroups((DefaultMutableTreeNode) node.getChildAt(1), count, sim, nc);
		}

		return count;
	}

	/** getColouriser(double s).
	 * 
	 *  <p>Builds a PAL NameColouriser object that contains marker-name/colour pairs
	 *  based on how many groups the dendrogram is to be split into.
	 *  
	 */
	public NameColouriser getColouriser(double s) {
		NameColouriser nc = new NameColouriser();
		colorSeed = 1;
		countGroups(root, 0, s, nc);

		return nc;
	}

	// TV: This is the method that calculates unique colors for each group
	// The sequence until the number of groups is used as a colorSeed to
	// Random()
	private void setColouriserNames(DefaultMutableTreeNode node, NameColouriser nc) {
		DenNode dNode = (DenNode) node.getUserObject();

		if (dNode.cm != null) {
			Random r = new Random(colorSeed);
			Color c = new Color(r.nextInt(255), r.nextInt(255), r.nextInt(255));

			// nc.addMapping(dNode.cm.marker.getName(), c);
			nc.addMapping("" + dNode.cm.marker.getDendrogramName(), c);
		} else {
			setColouriserNames((DefaultMutableTreeNode) node.getChildAt(0), nc);
			setColouriserNames((DefaultMutableTreeNode) node.getChildAt(1), nc);
		}
	}

	public class DenNode implements Serializable {
		private static final long serialVersionUID = 7945949299504671913L;
		int number;
		float dblDistance;
		float similarity;
		CMarker cm;
		Color color;
		Color edgeColor;
		// Stroke edgeStroke;

		DenNode(int number, CMarker cm, float dblDistance) {
			this.number = number;
			this.cm = cm;
			this.dblDistance = dblDistance;
			this.similarity = 1 - dblDistance;
			this.color = Color.WHITE;
			this.edgeColor = Color.BLACK;
			// this.edgeStroke = new BasicStroke(10);
		}

		public CMarker getCMarker() {
			return cm;
		}

		public float getSimilarity() {
			return similarity;
		}

		public int getNumber() {
			return number;
		}

		public void setColor(Color color) {
			this.color = color;
		}

		public Color getColor() {
			return color;
		}

		public void resetColor() {
			color = Color.WHITE;
		}

		// TV: The color of the edge that points to that node
		public void setEdgeColor(Color color) {
			this.edgeColor = color;
		}

		public Color getEdgeColor() {
			return this.edgeColor;
		}

		public void resetEdgeColor() {
			this.edgeColor = Color.BLACK;
		}

		/*
		 * TV: the shape of the edge that points to that node public void
		 * setEdgeStroke(Stroke stroke) { this.edgeStroke = stroke; } public
		 * Stroke getEdgeStroke() { return this.edgeStroke; } public void
		 * resetEdgeStroke() { this.edgeStroke = new BasicStroke(10); }
		 */
	}

	// Converts SafeNames in the PAL Tree (MKR003, etc) back to their original
	// markers names
	private void testPal(LinkageGroup lGroup) {
		// int count = palTree.getExternalNodeCount();

		for (int i = 0; i < palTree.getExternalNodeCount(); i++) {
			Node node = palTree.getExternalNode(i);

			String safeName = node.getIdentifier().getName();
			// System.out.println("safename: " + safeName);
			CMarker cm = lGroup.getMarkerBySafeName(safeName);
			// System.out.println("name: " + cm.marker.getName());
			node.setIdentifier(new Identifier(cm.marker.getDendrogramName()));
			// new Identifier(lGroup.getMarkerName(safeName)));
		}
	}
}