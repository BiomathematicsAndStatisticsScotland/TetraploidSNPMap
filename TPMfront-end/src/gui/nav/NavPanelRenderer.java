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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.UIManager;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import gui.Icons;

class NavPanelRenderer extends JLabel implements TreeCellRenderer {
	private static final long serialVersionUID = -322426230608891931L;
	private static Color bColor = UIManager.getColor("Tree.selectionBackground");
	private boolean selected = false;

	public Component getTreeCellRendererComponent(JTree tree, Object value, 
			boolean selected, boolean expanded,
			boolean leaf, int row, boolean hasFocus) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
		Object obj = node.getUserObject();
		setText(obj.toString());

		if (obj instanceof MarkersNode) {
			setIcon(Icons.LINKAGE_GROUP);
		} else if (obj instanceof GroupsNode) {
			if (expanded) {
				setIcon(Icons.GROUP_OPEN);
			} else {
				setIcon(Icons.GROUP_FOLDER);
			}
		} else if (obj instanceof ClusterNode) {
			if (expanded) {
				setIcon(Icons.CLUSTER_OPEN);
			} else {
				setIcon(Icons.CLUSTER_FOLDER);
			}
		} else if (obj instanceof SummaryNode) {
			setIcon(Icons.SUMMARY);
		} else if (obj instanceof OrderedNode) {
			setIcon(Icons.ORDER);
		} else if (obj instanceof DendrogramsNode) {
			setIcon(Icons.DENDROGRAM);
		} else if (obj instanceof MapNode) {
			setIcon(Icons.MAP);
		} else if (obj instanceof QTLNode) {
			setIcon(Icons.QTL);
		} else if (obj instanceof AnovaNode) {
			setIcon(Icons.ANOVA);
		} else if (expanded) {
			setIcon(Icons.FOLDER_OPEN);
		} else if (leaf) {
			setIcon(new DefaultTreeCellRenderer().getLeafIcon());
		} else {
			setIcon(Icons.FOLDER);
		}

		this.selected = selected;

		if (selected) {
			setForeground((Color) UIManager.get("Tree.selectionForeground"));
		} else {
			setForeground((Color) UIManager.get("Tree.foreground"));
		}

		return this;
	}

	public void paintComponent(Graphics g) {
		Icon icon = getIcon();

		int offset = 0;
		if (icon != null && getText() != null) {
			offset = (icon.getIconWidth() + getIconTextGap());
		}

		if (selected) {
			g.setColor(bColor);
			g.fillRect(offset, 0, 500 - offset, getHeight() - 1);
		}

		super.paintComponent(g);
	}
}
