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

package gui.nav;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.Enumeration;
import java.util.LinkedList;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import data.AnovaResult;
import data.Cluster;
import data.LinkageGroup;
import data.LinkageMapGraph;
import data.OrderedResult;
import data.QTLResult;
import gui.AppFrame;
import gui.AppFrameMenuBar;
import gui.GradientPanel;
import gui.IPrintable;
import gui.Project;

public class NavPanel extends JPanel implements TreeSelectionListener {
	private static final long serialVersionUID = 3807165873448936923L;
	private DefaultMutableTreeNode root = null;
	private DefaultTreeModel model = null;
	private JTree tree;
	private JScrollPane sp;
	public AppFrame appFrame;
	private JSplitPane splits;
	private JPanel blankPanel;
	private GradientPanel titlePanel;
	boolean debugging = false;

	/**  NavPanel().
	 * 
	 */
	public NavPanel(AppFrame appFrame, JSplitPane splits) {
		this.appFrame = appFrame;
		this.splits = splits;

		root = new DefaultMutableTreeNode("root");
		model = new DefaultTreeModel(root);

		tree = new JTree(model);
		tree.setRootVisible(false);
		tree.addTreeSelectionListener(this);
		tree.setCellRenderer(new NavPanelRenderer());
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);

		sp = new JScrollPane(tree);
		setLayout(new BorderLayout());
		titlePanel = new GradientPanel("Datasets");
		add(titlePanel, BorderLayout.NORTH);
		add(sp);

		splits.setLeftComponent(this);
		blankPanel = new JPanel();
		blankPanel.setBackground(Color.white);
		splits.setRightComponent(blankPanel);
		splits.setDividerLocation(140);
	}

	public void writetree() {
		writetree(root, "+");
	}

	private void writetree(MutableTreeNode n, String pf) {
		System.out.println(pf + " " + n.toString());
		if (!n.isLeaf()) {
			MutableTreeNode nn;
			@SuppressWarnings("rawtypes")
			Enumeration e = n.children();
			while (e.hasMoreElements()) {
				nn = (MutableTreeNode) e.nextElement();
				writetree(nn, pf + "-+");
			}
		}
	}

	/** clears the tree structure.
	 * 
	 */
	public void clear() {
		while (root.getChildCount() > 0) {
			model.removeNodeFromParent((MutableTreeNode) root.getChildAt(0));
		}
		titlePanel.setTitle("Datasets");

		int location = splits.getDividerLocation();
		splits.setRightComponent(blankPanel);
		splits.setDividerLocation(location);
	}

	/** Clears and updates the tree to display a recently opened Project file.
	 * 
	 */
	public void displayProject(LinkedList<LinkageGroup> groups) {
		clear();

		for (LinkageGroup lGroup : groups) {
			addRootLinkageGroup(lGroup);
		}

	}

	/** inspect the REAL datastructure starting with Project-Linkagegroups.
	 * 
	 */
	public void inspectProject() {
		System.out.println("PROJECT");
		LinkedList<LinkageGroup> groups = appFrame.getProject().getLinkageGroups();
		for (LinkageGroup lGroup : groups) {
			inspectLinkageGroup(" - ", lGroup);
		}
	}

	private void inspectLinkageGroup(String pref, LinkageGroup lGroup) {
		System.out.println(pref + ". (lGroup)" + lGroup.toString());
		inspectMarkerGroup(pref + "- ", lGroup);
	}

	private void inspectMarkerGroup(String pref, LinkageGroup lGroup) {
		System.out.println(pref + " (mGroup) " + lGroup.toString());
		for (Object obj : lGroup.getResults()) {
			if (obj instanceof Cluster) {
				inspectCluster(pref + "- ", (Cluster) obj);
			} else if (obj instanceof OrderedResult) {
				inspectOrderedResult(pref + "- ", (OrderedResult) obj);
			} else if (obj instanceof AnovaResult) {
				inspectAnovaResult(pref + "- ", (AnovaResult) obj);
			}
		}
	}

	private void inspectCluster(String pref, Cluster cluster) {
		System.out.println(pref + " - cFolder");
		System.out.println(pref + " _ Cl.summary, dendo's");
		for (LinkageGroup lGroup : cluster.getGroups()) {
			inspectMarkerGroup(pref + "- ", lGroup);
		}
	}

	private void inspectAnovaResult(String pref, AnovaResult result) {
		System.out.println(pref + "(anova)" + result.toString());
	}

	private void inspectOrderedResult(String pref, OrderedResult order) {
		System.out.println(pref + " (OrderedResult).." + order.toString());
		if (order.getQTLResults() != null) {
			for (QTLResult qtlResult : order.getQTLResults()) {
				inspectQTLResult(pref + "- ", qtlResult);
			}
		}
		if (order.getLinkageMaps() != null) {
			for (LinkageMapGraph map : order.getLinkageMaps()) {
				inspectLinkageMap(pref + "- ", map);
			}
		}
		if (order.getLinkageGroup() != null) {
			if (!order.getLinkageGroup().getResults().isEmpty()) {
				for (Object obj : order.getLinkageGroup().getResults()) {
					if (obj instanceof Cluster) {
						inspectCluster(pref + "- ", (Cluster) obj);
					} else if (obj instanceof OrderedResult) {
						inspectOrderedResult(pref + "- ", (OrderedResult) obj);
					} else if (obj instanceof AnovaResult) {
						inspectAnovaResult(pref + "- ", (AnovaResult) obj);
					}
				}
			}
			if (order.getPhaseResults() != null) {
				for (OrderedResult or : order.getPhaseResults()) {
					inspectPhaseResult(pref + "- ", or);
				}
			}
		}
	}

	private void inspectQTLResult(String pref, QTLResult qtlResult) {
		System.out.println(pref + "(QTLres) " + qtlResult.toString());
	}

	private void inspectLinkageMap(String pref, LinkageMapGraph map) {
		System.out.println(pref + " (map) " + map.toString());
	}

	private void inspectPhaseResult(String pref, OrderedResult or) {
		System.out.println(pref + " (phase) " + or.toString());
	}

	/** addRootLinkageGroup() - used  by import(SNP)DataSet.
	 * 
	 */
	public void addRootLinkageGroup(LinkageGroup lGroup) {
		DefaultMutableTreeNode dataNode = new DefaultMutableTreeNode(lGroup);
		DefaultMutableTreeNode node = addMarkerGroup(dataNode, lGroup);
		model.insertNodeInto(dataNode, root, root.getChildCount());
		tree.setSelectionPath(new TreePath(node.getPath()));
		tree.scrollPathToVisible(new TreePath(node.getPath()));

		titlePanel.setTitle("Datasets (" + root.getChildCount() + ")");
	}

	private DefaultMutableTreeNode addMarkerGroup(DefaultMutableTreeNode parent, LinkageGroup lGroup) {
		DefaultMutableTreeNode node = new DefaultMutableTreeNode(new MarkersNode(lGroup));
		parent.add(node);

		// Add any result clusters/twopoint to it
		for (Object obj : lGroup.getResults()) {
			if (obj instanceof Cluster) {
				addCluster((Cluster) obj, lGroup, parent);
			} else if (obj instanceof OrderedResult) {
				addOrderedResult((OrderedResult) obj, lGroup, parent);
			} else if (obj instanceof AnovaResult) {
				addAnovaResult((AnovaResult) obj, lGroup, parent);
			}
		}

		return node;
	}

	/** addCluster() - used by run(SNP)Cluster.
	 * 
	 */
	public void addCluster(Cluster cluster, LinkageGroup owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		// Create a folder for it
		DefaultMutableTreeNode cFolder = new DefaultMutableTreeNode(new ClusterNode(cluster, owner));

		// Add the summary and dendrogram nodes
		cFolder.add(new DefaultMutableTreeNode(new SummaryNode(cluster.getSummary())));
		cFolder.add(new DefaultMutableTreeNode(new DendrogramsNode(cluster)));

		// TV: Add a graph node
		if (!appFrame.tpmModeNONSNP()) {
			cFolder.add(new DefaultMutableTreeNode(new GraphsNode(cluster)));
		}

		// Then the individual linkage groups
		for (LinkageGroup lGroup : cluster.getGroups()) {
			DefaultMutableTreeNode gNode = new DefaultMutableTreeNode(new GroupsNode(lGroup));
			addMarkerGroup(gNode, lGroup);
			cFolder.add(gNode);
		}

		model.insertNodeInto(cFolder, parent, parent.getChildCount());
		tree.expandPath(new TreePath(cFolder.getPath()));
		tree.scrollPathToVisible(new TreePath(cFolder.getPath()));
	}

	/** addAnoveResult() - used by runAnova().
	 * 
	 */
	public void addAnovaResult(AnovaResult result, LinkageGroup owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		DefaultMutableTreeNode node = new DefaultMutableTreeNode(new AnovaNode(result));

		model.insertNodeInto(node, parent, parent.getChildCount());
		tree.setSelectionPath(new TreePath(node.getPath()));
		tree.scrollPathToVisible(new TreePath(node.getPath()));
	}

	/** addOrderedResult() - used by AppFrame (.twopoint, ordering, MDS, importSNPmap).
	 * 
	 */
	public void addOrderedResult(OrderedResult order, LinkageGroup owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		// Create a folder for it
		DefaultMutableTreeNode oFolder = new DefaultMutableTreeNode(new OrderFolderNode(order, owner));

		// Add the nodes
		oFolder.add(new DefaultMutableTreeNode(new SummaryNode(order.getSummary())));
		oFolder.add(new DefaultMutableTreeNode(new OrderedNode(order, this)));

		// if (order.is_readqtl())
		// oFolder.add(new DefaultMutableTreeNode(new
		// MarkersNode(order.getLinkageGroup())));
		if (order.getQTLResults() != null) {
			for (QTLResult qtlResult : order.getQTLResults()) {
				addQTLResult(qtlResult, order, oFolder);
			}
		}
		if (order.getLinkageMaps() != null) {
			for (LinkageMapGraph map : order.getLinkageMaps()) {
				addLinkageMap(map, order, oFolder);
			}
		}
		if (order.getLinkageGroup() != null) {
			if (!order.getLinkageGroup().getResults().isEmpty()) {
				for (Object obj : order.getLinkageGroup().getResults()) {
					if (obj instanceof Cluster) {
						addCluster((Cluster) obj, order.getLinkageGroup(), oFolder);
					} else if (obj instanceof OrderedResult) {
						addOrderedResult((OrderedResult) obj, order.getLinkageGroup(), oFolder);
					} else if (obj instanceof AnovaResult) {
						addAnovaResult((AnovaResult) obj, order.getLinkageGroup(), oFolder);
					}
				}
			}
			if (order.getPhaseResults() != null) {
				for (OrderedResult or : order.getPhaseResults()) {
					addPhaseResult(or, order.getLinkageGroup(), oFolder);
				}
			}
		}
		model.insertNodeInto(oFolder, parent, parent.getChildCount());
		tree.setSelectionPath(new TreePath(oFolder.getPath()));
		tree.scrollPathToVisible(new TreePath(oFolder.getPath()));
	}

	/** addLinkageMap() - used by AppFrame.makeLinkageMap().
	 * 
	 */
	public void addLinkageMap(LinkageMapGraph map, OrderedResult owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		DefaultMutableTreeNode node = new DefaultMutableTreeNode(new MapNode(owner, map));

		model.insertNodeInto(node, parent, parent.getChildCount());
		tree.setSelectionPath(new TreePath(node.getPath()));
		tree.scrollPathToVisible(new TreePath(node.getPath()));
	}

	/** addQTLResult() - used by AppFrame.run(SNP)QTL().
	 * 
	 */
	public void addQTLResult(QTLResult qtlResult, OrderedResult owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		DefaultMutableTreeNode node = new DefaultMutableTreeNode(new QTLNode(qtlResult, owner));

		model.insertNodeInto(node, parent, parent.getChildCount());
		tree.setSelectionPath(new TreePath(node.getPath()));
		tree.scrollPathToVisible(new TreePath(node.getPath()));
	}

	/** addPhaseResult() - used by AppFrame.runPhase().
	 * 
	 */
	public void addPhaseResult(OrderedResult phaseResult, LinkageGroup owner, DefaultMutableTreeNode parent) {
		// What node should we add it to?
		if (parent == null) {
			parent = (DefaultMutableTreeNode) getSelectedTreeNode().getParent();
		}

		// Create a folder for it
		DefaultMutableTreeNode oFolder = new DefaultMutableTreeNode(new OrderFolderNode(phaseResult, owner));

		// Add the nodes
		oFolder.add(new DefaultMutableTreeNode(new SummaryNode(phaseResult.getSummary())));
		oFolder.add(new DefaultMutableTreeNode(new PhaseNode(phaseResult)));

		// if (order.is_readqtl())
		// oFolder.add(new DefaultMutableTreeNode(new
		// MarkersNode(order.getLinkageGroup())));
		if (phaseResult.getLinkageMaps() != null) {
			for (LinkageMapGraph map : phaseResult.getLinkageMaps()) {
				addLinkageMap(map, phaseResult, oFolder);
			}
		}

		model.insertNodeInto(oFolder, parent, parent.getChildCount());
		tree.setSelectionPath(new TreePath(oFolder.getPath()));
		tree.scrollPathToVisible(new TreePath(oFolder.getPath()));

	}

	/**
	 *  Called when auto-selection of markers has taken place.
	 *  
	 *  <p>The correct table containing these markers must be retrieved and updated
	 */
	public void markersUpdated() {
		MarkersNode node = (MarkersNode) getSelectedTreeNode().getUserObject();
		if (appFrame.isSNP()) {
			appFrame.markerSNPDetails.selectedMarkersChanged(node.lGroup);
		} else {
			appFrame.markerDetails.selectedMarkersChanged(node.lGroup);
		}
	}

	/** for debugging only..
	 * 
	 */
	public void debugprint(String s) {
		if (debugging) {
			System.out.println(s);
		}
	}

	/** listen to which treenode is selected.
	 * 
	 */
	public void valueChanged(TreeSelectionEvent e) {
		appFrame.menubar.setInitialState();
		appFrame.menubar.setProjectOpenedState();
		AppFrameMenuBar.aAnalysisRemove.setEnabled(true);
		AppFrameMenuBar.aFileImport.setEnabled(true);
		if (!appFrame.tpmModeSNP()) {
			AppFrameMenuBar.aTrait.setEnabled(true);
			AppFrameMenuBar.aTraitView.setEnabled(true);
		}
		DefaultMutableTreeNode n = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();
		if (n == null) {
			return;
		}

		Object nodeInfo = n.getUserObject();

		int location = splits.getDividerLocation();

		if (nodeInfo instanceof MarkersNode) {
			debugprint("MarkersNode");
			MarkersNode node = (MarkersNode) nodeInfo;
			appFrame.menubar.setMenusForMarkersNode();
			if (appFrame.isSNP()) {
				appFrame.markerSNPDetails.displayLinkageGroup(node.lGroup);
				splits.setRightComponent(appFrame.markerSNPDetails);
			} else {
				appFrame.markerDetails.displayLinkageGroup(node.lGroup);
				splits.setRightComponent(appFrame.markerDetails);
			}
		} else if (nodeInfo instanceof ClusterNode) {
			debugprint("cluster node");
			ClusterNode node = (ClusterNode) nodeInfo;
			splits.setRightComponent(new JLabel("" + node, JLabel.CENTER));
		} else if (nodeInfo instanceof AnovaNode) {
			debugprint("anova node");
			AnovaNode node = (AnovaNode) nodeInfo;
			splits.setRightComponent(node.panel);
		} else if (nodeInfo instanceof OrderFolderNode) {
			debugprint("order folder node");
			OrderFolderNode node = (OrderFolderNode) nodeInfo;
			splits.setRightComponent(new JLabel("" + node, JLabel.CENTER));
		} else if (nodeInfo instanceof GroupsNode) {
			debugprint("groupsnode");
			GroupsNode node = (GroupsNode) nodeInfo;
			splits.setRightComponent(node.getLabel());
		} else if (nodeInfo instanceof OrderedNode) {
			debugprint("orderednode");
			OrderedNode node = (OrderedNode) nodeInfo;
			splits.setRightComponent(node.panel);
			debugprint("node.order: " + node.order);
			appFrame.menubar.setMenusForOrderedNode(node.order.orderedresulttype);
		} else if (nodeInfo instanceof DendrogramsNode) {
			debugprint("dendrogramsnode");
			DendrogramsNode node = (DendrogramsNode) nodeInfo;
			splits.setRightComponent(node.panel);
			AppFrameMenuBar.aAnalysisRemove.setEnabled(false);
		} else if (nodeInfo instanceof GraphsNode) {
			debugprint("graphsnode");
			GraphsNode node = (GraphsNode) nodeInfo;
			splits.setRightComponent(node.panel);
			AppFrameMenuBar.aAnalysisRemove.setEnabled(false);
		} else if (nodeInfo instanceof SummaryNode) {
			debugprint("summarynode");
			SummaryNode node = (SummaryNode) nodeInfo;
			splits.setRightComponent(node.panel);
			AppFrameMenuBar.aAnalysisRemove.setEnabled(false);
		} else if (nodeInfo instanceof MapNode) {
			debugprint("mapnode");
			MapNode node = (MapNode) nodeInfo;
			splits.setRightComponent(node.panel);
		} else if (nodeInfo instanceof QTLNode) {
			debugprint("QTLnode");
			QTLNode node = (QTLNode) nodeInfo;
			splits.setRightComponent(node.panel);
		} else if (nodeInfo instanceof PhaseNode) {
			debugprint("PhaseNode");
			AppFrameMenuBar.aFileSaveSel.setEnabled(true);
			PhaseNode node = (PhaseNode) nodeInfo;
			splits.setRightComponent(node.panel);
		} else {
			debugprint("ELSE node");
			splits.setRightComponent(new JLabel("" + n, JLabel.CENTER));
		}

		// Can the user print now?
		if (nodeInfo instanceof IPrintable && ((IPrintable) nodeInfo).isPrintable()) {
			AppFrameMenuBar.aFilePrint.setEnabled(true);
		} else {
			AppFrameMenuBar.aFilePrint.setEnabled(false);
		}

		splits.setDividerLocation(location);
	}

	/** Only if the selected node is a MarkersNode, return its LinkageGroup.
	 * 
	 *  <p>otherwise return NULL.
	 */
	public LinkageGroup getSelectedLinkageGroup() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		Object obj = node.getUserObject();
		if (obj instanceof MarkersNode) {
			return ((MarkersNode) obj).lGroup;
		}
		return null;
	}

	/** return the UserObject of the selected node.
	 * 
	 */
	public Object getSelectedNode() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		Object obj = node.getUserObject();
		return obj;
	}

	/** used only to describe the linkage group path in the logger.
	 * 
	 */
	public String getSelectedLinkageGroupPathStr() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		String path = "";
		TreeNode[] nodes = node.getPath();
		for (int i = 1; i < nodes.length - 1; i++) {
			path += "" + nodes[i];
			if (i != (nodes.length - 2)) path += "->";
		}

		return path;
	}

	/** which cluster are we in.
	 * 
	 */
	public Cluster getCurrentClusterHeadNode() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		while (true) {
			if (node.getParent() == null) return null;

			Object o = node.getUserObject();
			if (o instanceof ClusterNode) {
				return ((ClusterNode) node.getUserObject()).cluster;
			} else {
				node = (DefaultMutableTreeNode) node.getParent();
			}
		}
	}

	/** Which OrderFolder are we in.
	 * 
	 */
	public OrderFolderNode getCurrentOrderedHeadNode() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();
		while (true) {
			if (node.getParent() == null) {
				return null;
			}
			Object o = node.getUserObject();
			if (o instanceof OrderFolderNode) {
				return (OrderFolderNode) node.getUserObject();
			} else {
				node = (DefaultMutableTreeNode) node.getParent();
			}
		}
	}

	/** Only if current selected treenode is OrderedNode, return its OrderedResult.
	 * 
	 * <p>otherwise return null.
	 */
	public OrderedResult getSelectedOrderedResult() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();
		Object o = node.getUserObject();
		if (!(o instanceof OrderedNode)) {
			return null;
		}
		OrderedNode on = (OrderedNode) o;

		return on.order;
	}

	/** used by AppFrame.runPhase().
	 * 
	 */
	public OrderedResult getTwopointForSelectedMDS() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();
		Object o = node.getUserObject();
		if (!(o instanceof OrderedNode)) {
			return null;
		}
		node = (DefaultMutableTreeNode) node.getParent().getParent();
		OrderFolderNode ofn = (OrderFolderNode) node.getUserObject();

		return ofn.order;
	}

	/** used by AppFrame.runMDS and .runPhase().
	 * 
	 */
	public LinkageGroup getParentLinkageGroup() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		while (true) {
			if (node.getParent() == null) {
				return null;
			}

			Object o = node.getUserObject();
			if (o instanceof GroupsNode || node.getParent().equals(root)) {
				DefaultMutableTreeNode leafnode = node.getFirstLeaf();
				return ((MarkersNode) leafnode.getUserObject()).lGroup;
			} else {
				node = (DefaultMutableTreeNode) node.getParent();
			}

		}
	}

	/** Get the root linkage group for the currently selected dataset.
	 * 
	 */
	public LinkageGroup getRootLinkageGroupForDataSet() {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath()
				.getLastPathComponent();

		TreeNode[] path = node.getPath();
		Object obj = ((DefaultMutableTreeNode) path[1].getChildAt(0)).getUserObject();
		return ((MarkersNode) obj).lGroup;
	}

	/** debug - show the nav tree.
	 * 
	 */
	public void inspect() {
		writetree();
	}

	/** get the selected tree node.
	 * 
	 */
	public DefaultMutableTreeNode getSelectedTreeNode() {
		return (DefaultMutableTreeNode) tree.getSelectionModel().getSelectionPath().getLastPathComponent();
	}

	/** return the selected treenode as an IPrintable.
	 * 
	 * <p>This can only be called when NavPanel has allowed the aFilePrint 
	 * action to be enabled, hence this method can ONLY return an IPrintable 
	 * object.
	 * 
	 */
	public IPrintable getSelectedPrintableNode() {
		DefaultMutableTreeNode node = getSelectedTreeNode();
		return (IPrintable) node.getUserObject();
	}

	/** return the orderedResult for the selected treenode.
	 * 
	 */
	public OrderedResult getCurrentOrderedResult() {
		DefaultMutableTreeNode node = getSelectedTreeNode();
		return ((OrderedNode) node.getUserObject()).order;
	}

	/** remove the selected analysis.
	 * 
	 * @param project = we need the project to be able to remove top-level linkagegroups.
	 */
	public void removeAnalysis(Project project) {
		DefaultMutableTreeNode node = getSelectedTreeNode();
		Object obj = node.getUserObject();

		if (obj instanceof OrderedNode || obj instanceof MarkersNode || obj instanceof PhaseNode) {
			node = (DefaultMutableTreeNode) node.getParent();
			obj = node.getUserObject();
		}

		DefaultMutableTreeNode pnode = (DefaultMutableTreeNode) node.getParent();
		Object pObj = pnode.getUserObject();

		if (obj instanceof ClusterNode) {
			debugprint("removing clusternode");
			ClusterNode cNode = (ClusterNode) obj;
			cNode.lGroup.getResults().remove(cNode.cluster);
			model.removeNodeFromParent(node);
		} else if (obj instanceof OrderFolderNode) {
			OrderFolderNode oNode = (OrderFolderNode) obj;
			if (pObj instanceof GroupsNode) {
				oNode.lGroup.getResults().remove(oNode.order);
			} else if (pObj instanceof OrderFolderNode) {
				((OrderFolderNode) pObj).getResult().removeResult(oNode.getResult());
			} else {
				oNode.lGroup.getResults().remove(oNode.order);
			}
			model.removeNodeFromParent(node);
		} else if (obj instanceof AnovaNode) {
			AnovaNode aNode = (AnovaNode) obj;
			// parent is always is gNode
			GroupsNode gNode = (GroupsNode) pObj;
			gNode.lGroup.getResults().remove(aNode.result);
			model.removeNodeFromParent(node);
		} else if (obj instanceof DendrogramsNode) {
			return;
		} else if (obj instanceof GraphsNode) {
			return;
		} else if (obj instanceof GroupsNode) {
			GroupsNode gNode = (GroupsNode) obj;
			((ClusterNode) pnode.getUserObject()).cluster.getGroups().remove(gNode.lGroup);
			model.removeNodeFromParent(node);
		} else if (obj instanceof MapNode) {
			MapNode mNode = (MapNode) obj;
			OrderFolderNode ofNode = (OrderFolderNode) pnode.getUserObject();
			ofNode.getResult().getLinkageMaps().remove(mNode.map);
			model.removeNodeFromParent(node);
		} else if (obj instanceof OrderedNode) {
			System.out.println("OrderedNode.. should be changed to remove its parent..");
			return;
		} else if (obj instanceof PhaseNode) {
			System.out.println("PhaseNode.. should be changed to remove its parent..");
			return;
		} else if (obj instanceof QTLNode) {
			OrderFolderNode ofNode = (OrderFolderNode) pObj;
			ofNode.order.getQTLResults().remove(((QTLNode) obj).qtlResult);
			model.removeNodeFromParent(node);
		} else if (obj instanceof SummaryNode) {
			System.out.println("don't know how to remove summarynode.. (DON't DELETE)" + obj.toString());
			return;
		} else {
			project.getLinkageGroups().remove(obj);
			model.removeNodeFromParent(node);
		}

		AppFrameMenuBar.aFileSave.setEnabled(true);

		int location = splits.getDividerLocation();
		splits.setRightComponent(blankPanel);
		splits.setDividerLocation(location);
	}

	/** refresh the tree.
	 * 
	 */
	public void refreshTree() {
		tree.updateUI();
	}
}
