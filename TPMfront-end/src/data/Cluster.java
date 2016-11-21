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

import java.io.Serializable;
import java.util.LinkedList;

import javax.swing.tree.TreeNode;

import edu.uci.ics.jung.graph.DelegateTree;

/** Cluster. SNP & NONSNP.
 * 
 * 
 *
 */
public class Cluster implements Serializable {
	static final long serialVersionUID = -2652444294541417903L;

	private Summary summary;
	private String name;

	private LinkedList<LinkageGroup> groups = new LinkedList<LinkageGroup>();

	// The Average Linkage Dendrogram which is used for the SNP Markers, gets
	// updated every time that
	// the user changes the similarity level using the JSlider in GraphToolBar.
	// When this happens the
	// tree graph needs to be redrawn, otherwise the Transformer for vertexColor
	// can not complete in real time.
	private Dendrogram avLnkDendrogram = null;
	private Dendrogram snLnkDendrogram = null;

	// private Graph<TreeNode, String> graph;
	private DelegateTree<TreeNode, TreeNode> graph;

	public Cluster() {
		// graph = new UndirectedSparseMultigraph<TreeNode, String>();
		graph = new DelegateTree<TreeNode, TreeNode>();
	}

	// setGraph() is never used which means that the graph never gets
	// updated!!!!!!!!!! ALERT!!!
	public void setGraph(DelegateTree<TreeNode, TreeNode> graph) {
		this.graph = graph;
	}

	// public Graph<TreeNode, String> getGraph()
	public DelegateTree<TreeNode, TreeNode> getGraph() {
		return this.graph;
	}

	public void setSummary(Summary summary) {
		this.summary = summary;
	}

	public Summary getSummary() {
		return summary;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public String toString() {
		return name;
	}

	public LinkedList<LinkageGroup> getGroups() {
		return groups;
	}

	public void addLinkageGroup(LinkageGroup lGroup) {
		groups.add(lGroup);
	}

	public void setAvLnkDendrogram(Dendrogram dendrogram) {
		// System.out.println("set avLinkDendrogram");
		avLnkDendrogram = dendrogram;
	}

	public void setSnLnkDendrogram(Dendrogram dendrogram) {
		snLnkDendrogram = dendrogram;
	}

	public Dendrogram getAvLnkDendrogram() {
		return avLnkDendrogram;
	}

	public Dendrogram getSnLnkDendrogram() {
		return snLnkDendrogram;
	}
}