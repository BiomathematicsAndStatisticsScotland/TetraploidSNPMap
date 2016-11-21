// TreePainterNormal.java
//


// (c) 1999-2001 PAL Development Core Team
//
// This package may be distributed under the
// terms of the Lesser GNU General Public License (LGPL)


package gui.pal;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import gui.Prefs;
import pal.gui.LabelDisplayer;
import pal.gui.LayoutTracker;
import pal.gui.PositionedNode;
import pal.io.FormattedOutput;
import pal.tree.Tree;

/**
 * A class that can paint a tree into a Graphics object.
 *
 * @version $Id: TreePainterNormal.java,v 1.17 2003/08/16 23:48:26 matt Exp $
 *
 * @author Alexei Drummond
 * @note
 *     <ul>
 *       <li> 14 August 2003 - Changed to reflect NameColouriser changes
 *     </ul>
 */
public class TetraploidTreePainter extends TreePainter {

	public static final int RIGHTBORDER = 10;
	public static final int LEFTBORDER = 10;
	public static final int TOPBORDER = 20;
	public static final int BOTTOMBORDER = 30;

	public static final int FONT_SIZE = 11;
	public static final int YSPACER = 20;
	public static final int XSPACER = 4;

	private double xScale = 1.0;
	private double yScale = 1.0;

	private Font labelFont = new Font("Dialog", Font.PLAIN, FONT_SIZE);
	int maxLabelWidth = -1;

	public TetraploidTreePainter(Tree toDisplay, String title, boolean showTitle) {
		super(toDisplay,title,showTitle);
	}

	/**
	 * Returns the preferred size for drawing.
	 * 
	 * <p>(that is the size that will show everything nicely)
	 */
	public Dimension getPreferredSize() {
		return new Dimension(100 + LEFTBORDER + RIGHTBORDER,
			(int)Math.round(width * FONT_SIZE) + TOPBORDER + BOTTOMBORDER);
	}

	protected void paint(PositionedNode node, Graphics g,
		int displayWidth, int displayHeight, LayoutTracker lt, boolean isRoot) {

		Point p = getPoint(node,displayWidth, displayHeight);
		g.setColor(FOREGROUND);
		if (isRoot) {
			g.fillRect(p.x - 4, p.y - 1, 4, 3); //Cheap hack!
		}
		if (node.hasChildren()) {
			for (int i = 0; i < node.getChildCount(); i++) {
				paintLeafBranch(p, getPoint((PositionedNode)node.getChild(i),
					displayWidth,displayHeight), node, g,lt);
			}

			for (int i = 0; i < node.getChildCount(); i++) {
				paint((PositionedNode)node.getChild(i), g,displayWidth, displayHeight,lt,false);
			}
			int bootStrapValue = getBootstrapValue(node);
			if (bootStrapValue >= 50) {
				g.setColor(BOOTSTRAP_SUPPORT_COLOUR);
				g.drawString(bootStrapValue + "", p.x + XSPACER,
							p.y + (FONT_SIZE / 2));
			}
		} else {

			if ((maxLeafTime > 0.0) && isUsingColor()) {
				g.setColor(Color.getHSBColor((float)(
						maxLeafTime - node.getNodeHeight()) / (float)maxLeafTime, 1.0f, 1.0f));
			} else {
				g.setColor(NORMAL_LABEL_COLOR);
			}

			if (isUsingColor()) {
				int halfWidth = getPenWidth() / 2;
				g.fillRect(p.x - halfWidth, p.y - halfWidth, getPenWidth(), getPenWidth());
			}
			if (isUsingSymbols() && getTimeOrderCharacterData() != null) {

				drawSymbol(g, p.x + XSPACER, p.y - (FONT_SIZE / 2), FONT_SIZE,
					getTimeOrderCharacterData().getTimeOrdinal(
							getTimeOrderCharacterData().whichIdNumber(node.getIdentifier().getName())));
			} else {
				String name = getNodeName(node);
				int width = g.getFontMetrics().stringWidth(name);
				if (isUsingColor()) {
					g.drawString(name, p.x + XSPACER,
							p.y + (FONT_SIZE / 2));
					if (node.isHighlighted()) {
						g.setColor(Color.red);
						g.drawOval(p.x - 4 + XSPACER, p.y - FONT_SIZE / 2 - 5, width + 10, FONT_SIZE + 8 );
					}
				} else {
					LabelDisplayer defaultDisplay = (
							node.isHighlighted() ? HILITED_LABEL_DISPLAY : NORMAL_LABEL_DISPLAY );
					getNodeDisplay(node,defaultDisplay).display(
							g,name, p.x + XSPACER, p.y + (FONT_SIZE / 2));
				}
				//Inform layout tracker of new String
				if (lt != null) {
					lt.addMapping(name,new Rectangle(p.x + XSPACER, p.y - (FONT_SIZE / 2), width,FONT_SIZE));
				}
			}
		}

	}
	
	public void paint(Graphics g, int displayWidth, int displayHeight) {
		paint(g,displayWidth,displayHeight,null);
	}

	/** paint.
	 * 
	 */
	public void paint(Graphics g, int displayWidth, int displayHeight, LayoutTracker lt) {
		g.setFont(labelFont);
		if (maxLabelWidth < 0) {
			maxLabelWidth = getLongestIdentifierPixelWidth(g.getFontMetrics());
		}

		double h = height;
		if (maxHeight != -1.0) h = maxHeight;
		xScale = (double)(displayWidth - LEFTBORDER - RIGHTBORDER  - maxLabelWidth) / h;
		yScale = (double)(displayHeight - TOPBORDER - BOTTOMBORDER) / width;

		g.setColor(BACKGROUND);
		g.fillRect(0, 0, displayWidth, displayHeight);
		paint(treeNode, g, displayWidth - maxLabelWidth, displayHeight,lt, true);

		doTitle(g,LEFTBORDER, TOPBORDER - 8);
		doSimilarityScale(g,xScale,LEFTBORDER,displayHeight - BOTTOMBORDER + 10);
	}


	/** getPoint().
	 * 
	 */
	public Point getPoint(PositionedNode node, int displayWidth, int displayHeight) {

		return new Point(
			displayWidth - (int)Math.round(node.getNodeHeight() * xScale) - RIGHTBORDER,
			(int)Math.round(node.getX() * yScale) + TOPBORDER);
	}

	private void paintLeafBranch(Point p, Point lp, PositionedNode node, Graphics g, LayoutTracker lt) {

		int halfWidth = getPenWidth() / 2;

		// paint join to parent
		g.fillRect(p.x - halfWidth, Math.min(p.y, lp.y) - halfWidth,
				getPenWidth(), Math.abs(lp.y - p.y) + getPenWidth());

		// paint branch
		g.fillRect(Math.min(p.x, lp.x) - halfWidth, lp.y - halfWidth,
				 Math.abs(lp.x - p.x) + getPenWidth(), getPenWidth());

		if (isShowingNodeHeights()) {

			String label = FormattedOutput.getInstance().getDecimalString(node.getNodeHeight(), 4);
			int width = g.getFontMetrics().stringWidth(label);

			int x = Math.min(p.x, lp.x) - (width / 2);

			g.drawString(label, x, p.y - halfWidth - 1);

		}

		if (isShowingInternalLabels()) {
			String label = getNodeName(node);
			int width = g.getFontMetrics().stringWidth(label);

			int x = Math.min(p.x, lp.x) - (width / 2);

			g.drawString(label, x, p.y - halfWidth - 1);
		}

		Object att = null;
		if (attName != null) {
			if (attName.equals("node height")) {
				att = new Double(node.getNodeHeight());
			} else if (attName.equals("branch length")) {
				att = new Double(node.getBranchLength());
			} else {
				att = node.getAttribute(attName);
			}
			if (att != null) {
				String label = null;
				if (att instanceof Double) {
					label = FormattedOutput.getInstance().getDecimalString(((Double)att).doubleValue(), 3);
				} else {
					label = att.toString();
				}

				//int width = g.getFontMetrics().stringWidth(label);
				int height = g.getFontMetrics().getAscent();
				int x = Math.min(p.x, lp.x) + halfWidth + 1;
				g.drawString(label, x, p.y + (height / 2));
			}
		}
	}
	
	public double min;
	public double max = 1.0;
	
	private int rightWidth = -1;
	private int simWidth = -1;

	private void doSimilarityScale(Graphics g, double xScale, int x1, int y) {
		int x2 = x1 + (int)Math.round((min / 2) * xScale);
		
		if (rightWidth == -1) {
			FontMetrics fm = g.getFontMetrics(g.getFont());
			
			rightWidth = fm.stringWidth("" + max) - 2;
			simWidth = (int) Math.round(fm.stringWidth("Similarity") / 2.0);
		}
		
		g.setColor(FOREGROUND);
		// Main line
		g.drawLine(x1, y, x2, y);
		// End ticks
		g.drawLine(x1, y + 1, x1, y - 1);
		g.drawLine(x2, y + 1, x2, y - 1);
		
		// Scale numbers
		g.drawString("" + Prefs.d5.format(1 - min), x1,y + 13);
		g.drawString("" + max, x2 - rightWidth, y + 13);
		g.drawString("Similarity", x1 + ((x2 - x1) / 2) - simWidth, y + 13);
	}
	
	/** getSimilarityForX().
	 * 
	 */
	public double getSimilarityForX(int x, int displayWidth) {
		double h = height;
		if (maxHeight != -1.0) h = maxHeight;
		xScale = (double)(displayWidth - LEFTBORDER - RIGHTBORDER  - maxLabelWidth) / h;
		
		
		double sim = (2 * ( (x - LEFTBORDER) / xScale));
		
		return 1 - (min - sim);
	}
}
