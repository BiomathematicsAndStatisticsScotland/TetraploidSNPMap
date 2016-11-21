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

package analyses.cluster;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.StringTokenizer;
import java.util.Enumeration;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import data.LinkageGroup;
import data.Cluster;
import data.CMarker;
import data.Dendrogram;
import data.Dendrogram.DenNode;
import doe.MsgBox;
import edu.uci.ics.jung.graph.DelegateTree;
import edu.uci.ics.jung.graph.Graph;

class ProcessClusterResults extends Thread {
	// Is the thread still running?
	boolean isRunning = true;
	// Have any errors occurred?
	boolean error = false;
	// Clustering method in use
	int method;
	// Processing phase (1 or 2)
	int phase = 1;
	
	// The linkage group that was processed
	private LinkageGroup lGroup;
	// The cluster group that will be created from the analysis
	private Cluster cluster;

	// Stores results from the average linkage clustering
	LinkedList<String> avList = new LinkedList<String>();
	// Stores results from the single linkage clustering
	LinkedList<String> snList = new LinkedList<String>();
	// Stores distance matrix
	
	
	private File file;
	
	ProcessClusterResults(LinkageGroup lGroup, Cluster cluster, File file, int method) {
		this.lGroup = lGroup;
		lGroup.freezeSafeNames();
		this.cluster = cluster;
		this.file = file;
		this.method = method;
		start();
	}
		
	// This method must run in two phases (over two methods)
	//  Phase 1, method 1 must read the snDendrogram
	//  Phase 1, method 2 must read both dendrograms
	//  Phase 2 for both methods is the same - read the linkage groups
	public void run() {
		try {
			
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			processDendrogram(in, snList);	
			if (method == 2) {
				processDendrogram(in, avList);
			}	
			Dendrogram snDendrogram = null; 				
			Dendrogram avDendrogram = null;
			// Single linkage always done...
			snDendrogram = new Dendrogram(lGroup, snList);
			cluster.setSnLnkDendrogram(snDendrogram);
					
			// Average linkage only done if 2nd clustering method in use...
			if (method == 2) {
				avDendrogram = new Dendrogram(lGroup, avList);
				cluster.setAvLnkDendrogram(avDendrogram);
			}
				
			processLinkageGroups(in);
			processGraph(cluster);
				
			in.close();			
						
		} catch (Exception e) {
			error = true;
			e.printStackTrace();
			MsgBox.msg("The clustering process has failed due to the following "
				+ "error:\n" + e, MsgBox.ERR);
		}
				
		isRunning = false;
	}
	
	private void processDendrogram(BufferedReader in, LinkedList<String> list)
		throws IOException {

		String term = null;
		if (list == snList)	term = " Single linkage clustering";
		else term = " Average linkage clustering";
		String line = in.readLine();
		while (line != null && !line.equals(term)) {
			line = in.readLine();
		}

		line = in.readLine();
		boolean somethingread = false;
		while (line != null 
				&& ( !somethingread || (line.length() > 0 
				&& !line.equals(" ")))) {
			if (line.length() > 0 && !line.equals(" ")) {
				list.add(line);
				somethingread = true;
			}
			line = in.readLine();
		} 
	}

	private void processLinkageGroups(BufferedReader in)
		throws IOException {
		int groupCount = 1;
		
		String line = in.readLine();
		while (line != null) {
			if (line.startsWith(" Linkage group")) {
				LinkageGroup newGroup = processLinkageGroup(in, groupCount++);
				if (newGroup.getMarkerCount() > 0) {
					cluster.addLinkageGroup(newGroup);
				}
			}
			line = in.readLine();
		}
	}
	
	private LinkageGroup processLinkageGroup(BufferedReader in, int groupCount)
		throws IOException {
		LinkageGroup group = new LinkageGroup("Group " + groupCount);
		
		String line = in.readLine();
		while (line.length() > 0) {
			StringTokenizer st = new StringTokenizer(line);
			String markerName = st.nextToken();
			int alleleCount = 1;
			alleleCount = Integer.parseInt(st.nextToken());
			CMarker cm = null;
			cm = lGroup.getMarkerBySafeName(markerName);
			group.addMarker(cm);
			
			
			for (int a = 0; a < alleleCount; a++) {
				line = in.readLine();
			}
			
			if (line == null) break;
			if (line.startsWith(" 0 ") 
					|| line.startsWith(" 1 ") 
					|| line.startsWith(" 2 ") 
					|| line.startsWith(" 3 ") 
					|| line.startsWith(" 4 ")) {
				line = in.readLine();
			}
			if (line == null || line.equals(" ")) {
				break;
			}
			
		}
		return group;
	}
	
	/*
	 * TV: method that constructs the cluster's graph from the subset of selected markers
	 * (LinkageGroup) and the Distance Matrix which could easily be printed in the 
	 * *.SNPchi file from SNPcluster.exe
	 * 
	 * Each Vertex is a CMarker and each Edge is a Float average distance
	 * 
	 * This method should be called after the processDendrogram()
	 * 
	 * 
	 */
	private Graph<TreeNode, TreeNode> processGraph(Cluster cluster) {
		// TV : this version tries to create a graph from this recursively ordered list 
		// of nodes that contain the DenNodes of the Dendrogram
		// NOTE: DenNodes when first created and added to the single root tree have the 
		// default Color.WHITE which is also the color of the graph when first drawn. 
		// Later on the vertices take their color from the execution of the Transformer
		// 
		DefaultMutableTreeNode root = cluster.getAvLnkDendrogram().getNodes().get(0);
		// gets an empty graph and sets its root
		//		cluster.getGraph().setRoot(root);
		//TV: first set the color for each Vertex
		float th = 0.5f; // The first time it is 0.5f
		colorizeTree(root, th); 
		// This is a duplicate method normally running in 
		// GraphsPanel each time Similarity is set
		cluster.setGraph(getGraphFromTree(root));
		return cluster.getGraph();
	}
	
	
	/** TV: Alternative way to custruct the graph.
	 * 
	 */
	public DelegateTree<TreeNode,TreeNode> getGraphFromTree(TreeNode root) {
		
		DelegateTree<TreeNode,TreeNode> g = new DelegateTree<TreeNode,TreeNode>();
		g.setRoot(root);
		
		@SuppressWarnings("rawtypes")
		Enumeration en = ((DefaultMutableTreeNode)root).breadthFirstEnumeration();
		// from root to leafs
		while (en.hasMoreElements()) {
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) en.nextElement();
			if (!node.isLeaf()) {
				g.addChild(node.getChildAt(0), node, node.getChildAt(0));
				g.addChild(node.getChildAt(1), node, node.getChildAt(1));
			}
		}
		return g;
	}
	
	public void colorizeTree(TreeNode root, float threshold) {
		Color customColor = Color.WHITE;
		@SuppressWarnings("rawtypes")
		Enumeration en = ((DefaultMutableTreeNode)root).breadthFirstEnumeration();
		while (en.hasMoreElements())  {
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) en.nextElement();  
			DenNode dNode = (DenNode) node.getUserObject();
			Integer level = node.getLevel();
		  
			if (level == 0) {
				dNode.setColor(customColor);
			} else if (level != 0) {
				DenNode pdNode = 
					(DenNode) 
						((DefaultMutableTreeNode)
							(node.getParent())).getUserObject();
				if ((dNode.getSimilarity() > threshold)
						&& (pdNode.getColor().equals(Color.WHITE))) {
					dNode.setColor(
						Color.getHSBColor((float) 
						(Math.cos(dNode.getSimilarity() * 90) * 360),
						0.8f, 0.8f));
				} else {
					dNode.setColor(pdNode.getColor());
				}
			}
		}
	}	
}