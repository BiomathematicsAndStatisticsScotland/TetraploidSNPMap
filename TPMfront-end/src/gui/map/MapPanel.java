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

package gui.map;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.image.BufferedImage;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.text.DecimalFormat;
import java.util.Vector;
import javax.swing.AbstractButton;
import javax.swing.JPanel;

public class MapPanel extends JPanel implements Printable {
	private static final long serialVersionUID = -9037665141607439725L;
	// Chromosome height
	int ch = 300;
	// 1/2 the chromosome width
	int cw = 5;
	// y-offset before chromosome is painted
	int cOffset = 30;
	// Current canvas width
	int w = 600;
	// Font height
	int fontHeight;
	// Font descent
	int fontAscent;
	// Widest string
	int fontWidth;
	// Minimum canvas dimensions required
	int minWidth, minHeight;
	// Width of fan arms
	int fan = 12;
	// Total number of markers that will be drawn
	int count = 0;
	int zoom = 1;
	DecimalFormat d = new DecimalFormat("0.0");

	Font mkrFont = new Font("SansSerif", Font.PLAIN, 10);
	Font ttlFont = new Font("SansSerif", Font.BOLD, 11);

	boolean topDown = true;
	boolean isAntiAliased = false;
	boolean isOverallShown = true;

	GMarker highlightedMarker = null;
	float totalDistance;

	Vector<GMarker[]> chromosomes = null;

	/** MapPanel().
	 * 
	 */
	public MapPanel(MapCreator creator) {
		setBackground(Color.white);

		chromosomes = creator.chromosomes;
		if (chromosomes.size() == 2) {
			isOverallShown = false;
		}
		count = creator.chromoAll.length;
		totalDistance = creator.totalDistance;

		addMouseMotionListener(new MouseMotionAdapter() {
			public void mouseMoved(MouseEvent e) {
				processMouseMovement(e);
			}
		});

		initialise();
	}

	// Tracks the mouse cursor, and attempts to match markers with its current
	// position, redrawing the display (and highlighting that marker) if so
	private void processMouseMovement(MouseEvent e) {
		int x = e.getPoint().x;
		int y = e.getPoint().y;

		if (highlightedMarker != null) {
			highlightedMarker.highlight = false;
		}

		// If we're above or below the chromosome, ignore
		if (y < cOffset || y > (cOffset + ch)) {
			setToolTipText(null);
			repaint();
			return;
		}

		// Just search within the chromosome under the cursor to speed things up
		int num = isOverallShown ? chromosomes.size() : chromosomes.size() - 1;
		int currentChromosome = (int) (x / (w / (num + 1)));

		if (currentChromosome < num) {
			if (isOverallShown == false) {
				currentChromosome++;
			}

			GMarker[] markers = chromosomes.get(currentChromosome);
			for (int i = 0; i < markers.length; i++) {
				if (markers[i].rec.contains(x, y)) {
					highlightedMarker = markers[i];
					highlightedMarker.highlight = true;

					setToolTipText(markers[i].name + " (" + markers[i].cm + ")");

					repaint();
					return;
				}
			}
		}

		setToolTipText(null);
		repaint();
	}

	private void initialise() {
		Graphics g = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB).getGraphics();
		FontMetrics fm = g.getFontMetrics(mkrFont);

		fontHeight = fm.getHeight();
		fontAscent = fm.getAscent();

		if (fontHeight % 2 != 0) {
			fontHeight++;
		}

		// For each chromosome...
		for (GMarker[] markers : chromosomes) {
			// For each marker in that chromosome...
			for (int i = 0; i < markers.length; i++) {
				int width = fm.stringWidth(markers[i].name);
				markers[i].setStringBounds(this, width, fontHeight);

				if (width > fontWidth) {
					fontWidth = width;
				}
			}
		}
	}

	public Dimension getPreferredSize() {
		int num = isOverallShown ? chromosomes.size() : chromosomes.size() - 1;

		// Max-width-of-font + the chromsome + 2 fans + gaps (and cm score)
		minWidth = (fontWidth + (2 * cw) + (2 * fan) + 50) * num;
		minHeight = ((count * fontHeight) + (2 * cOffset)) * zoom;
		// System.out.println("total width: " + MIN_WIDTH);
		// System.out.println("width per chromosome: " + (FONT_WIDTH + (2*C_W) +
		// (2*FAN) + 50));
		return new Dimension(minWidth, minHeight);
	}

	public int print(Graphics graphics, PageFormat pf, int pageIndex) {
		Graphics2D g2 = (Graphics2D) graphics;

		double panelWidth = minWidth; // width in pixels
		double panelHeight = minHeight; // height in pixels

		double pageHeight = pf.getImageableHeight(); // height of printer page
		double pageWidth = pf.getImageableWidth(); // width of printer page

		// Make sure empty pages aren't printed
		int totalNumPages = (int) Math.ceil(panelHeight / pageHeight);
		if (pageIndex >= totalNumPages) {
			return NO_SUCH_PAGE;
		}

		// Shift Graphic to line up with beginning of print-imageable region
		g2.translate(pf.getImageableX(), pf.getImageableY());
		// Shift Graphic to line up with beginning of next page to print
		g2.translate(0f, -pageIndex * pageHeight);

		// Scale (width)
		if ((pageWidth / panelWidth) < 1) {
			g2.scale((pageWidth / panelWidth), 1);
		}

		w = minWidth;
		// Scale chromosome to fit page length (if it's smaller)
		if (minHeight < pageHeight) {
			ch = (int) pageHeight - (2 * cOffset);
		} else {
			ch = (int) minHeight - (2 * cOffset);
		}

		paintMap(g2);

		return PAGE_EXISTS;
	}

	public void paintComponent(Graphics graphics) {
		super.paintComponent(graphics);
		Graphics2D g = (Graphics2D) graphics;

		w = getSize().width;
		ch = getSize().height - (2 * cOffset);

		paintMap(g);
		// calculateMarkerRectangles(g);
	}

	private void paintMap(Graphics2D g) {
		if (isAntiAliased) {
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		}

		calculateMarkerRectangles(g);

		int num = isOverallShown ? chromosomes.size() : chromosomes.size() - 1;

		for (int i = 0; i < chromosomes.size(); i++) {
			if (isOverallShown == false && i == 0) {
				continue;
			}

			GMarker[] markers = chromosomes.get(i);
			int c = isOverallShown ? i : (i - 1);
			int csX = ((c + 1) * (w / (num + 1))) + fontWidth / 2 - 20;
			// System.out.println("csX: " + csX);
			String name = null;
			if (i == 0 || chromosomes.size() < 3) {
				name = "Overall";
			} else {
				name = "C" + i;
			}

			paintChromosome(g, csX, name);

			for (int m = 0; m < markers.length; m++) {
				paintMarker(g, markers[m], csX, m);
			}
		}
	}

	private void paintChromosome(Graphics2D g, int csX, String name) {
		g.setColor(Color.red);
		g.drawRoundRect(csX - cw, cOffset - 4, cw * 2, ch + 8, cw * 2, 6);

		g.setFont(ttlFont);
		g.setColor(Color.black);

		FontMetrics fm = g.getFontMetrics(ttlFont);
		int pos = csX - (fm.stringWidth(name) / 2);
		g.drawString(name, pos, 15);
	}

	private void paintMarker(Graphics2D g, GMarker mkr, int csX, int i) {
		int lSide = csX - cw;
		int rSide = csX + cw;

		Stroke s = g.getStroke();

		if (mkr.highlight) {
			g.setColor(Color.blue);
			g.setStroke(new BasicStroke(2));
		} else {
			g.setColor(Color.red);
		}
		g.drawLine(lSide - 2, mkr.pos, rSide + 2, mkr.pos);

		// Line linking marker name to cross-over line
		g.drawLine(lSide - fan, (mkr.rec.y + mkr.recHalf), lSide - 2, mkr.pos);
		// Line linking cross-over line to CM value
		g.drawLine(rSide + 2, mkr.pos, rSide + fan, (mkr.rec.y + mkr.recHalf));
		g.setStroke(s);

		if (mkr.highlight) {
			g.setColor(new Color(240, 240, 240));
			g.fillRect(lSide - fan - mkr.recW, mkr.rec.y, mkr.recW, mkr.recH);
		}
		g.setColor(Color.black);
		g.setFont(mkrFont);
		// System.out.print(" " + (lSide-FAN-mkr.recW));
		g.drawString(mkr.name, lSide - fan - mkr.recW, mkr.rec.y + fontAscent);

		g.setColor(Color.blue);
		if (topDown) {
			g.drawString("" + d.format(mkr.cm), rSide + fan + 2, mkr.rec.y + fontAscent);
		} else {
			// If we're drawing upside-down, then the distance is total-cM
			g.drawString("" + d.format(totalDistance - mkr.cm), rSide + fan + 2, mkr.rec.y + fontAscent);
		}

		if (mkr.highlight) {
			g.drawRect(lSide - fan - 2 - mkr.recW, mkr.rec.y, mkr.recW + 2, mkr.recH);
		}
	}

	private void calculateMarkerRectangles(Graphics2D g) {
		int num = isOverallShown ? chromosomes.size() : chromosomes.size() - 1;

		for (int i = 0; i < chromosomes.size(); i++) {
			int c = isOverallShown ? i : (i - 1);
			int csX = ((c + 1) * (w / (num + 1)));
			int lSide = csX - cw;

			GMarker[] markers = chromosomes.get(i);
			for (int m = 0; m < markers.length; m++) {
				markers[m].setInitialRectangle(cOffset, ch, lSide - fan - 2);
			}
		}

		for (int i = 0; i < chromosomes.size(); i++) {
			GMarker[] markers = chromosomes.get(i);
			for (int m = 0; m < markers.length; m++) {
				// BB g.clearRect(0, 0, getSize().width, getSize().height);
				// BB paintMap(g);
				markers[m].optimiseRectangle(-1, (m == markers.length - 1));
			}
		}
	}

	void toggleAntiAliasing() {
		isAntiAliased = !isAntiAliased;
		repaint();
	}

	void zoom(int amount) {
		zoom += amount;
		reset();
	}

	void toggleOverallShown() { 
		if (chromosomes.size() == 2) {
			return;
		}
		isOverallShown = !isOverallShown;
		reset();
	}

	private void reset() {
		initialise();
		setSize(getPreferredSize());
		repaint();
	}

	// Creates a BufferedImage and draws the map onto it
	BufferedImage getSavableImage() {
		int multiply_size = 3;
		int tmp_C_H = ch;
		int tmp_C_W = cw;
		int tmp_C_OFFSET = cOffset;
		int tmp_W = w;
		int tmp_FAN = fan;
		cOffset = cOffset * multiply_size;
		cw = cw * multiply_size;
		fan = fan * multiply_size;
		Font tmp_mkrFont = mkrFont;
		Font tmp_ttlFont = ttlFont;
		mkrFont = new Font("SansSerif", Font.PLAIN, 17);
		ttlFont = new Font("SansSerif", Font.BOLD, 18);
		initialise();
		// Font height
		zoom(1);
		BufferedImage image = new BufferedImage(multiply_size * minWidth, multiply_size * minHeight,
				BufferedImage.TYPE_INT_RGB);
		Graphics2D g = image.createGraphics();

		w = multiply_size * minWidth;
		ch = multiply_size * minHeight - (2 * cOffset);

		g.setColor(Color.white);
		g.fillRect(0, 0, multiply_size * minWidth, multiply_size * minHeight);
		paintMap(g);
		g.dispose();
		ch = tmp_C_H;
		cw = tmp_C_W;
		cOffset = tmp_C_OFFSET;
		fan = tmp_FAN;
		w = tmp_W;
		mkrFont = tmp_mkrFont;
		ttlFont = tmp_ttlFont;
		initialise();
		zoom(-1);
		return image;
	}

	// Flips drawing of the chromosomes (with animation) by first increating the
	// size of the offset (the gap at the top/bottom of the screen), then
	// flipping, then decreasing the offset back to its original value
	void flip(final AbstractButton button) {
		Runnable r = new Runnable() {
			public void run() {
				int initialOffset = cOffset;
				int midpoint = getSize().height / 2;
				int increment = (midpoint - initialOffset) / 10;

				// Shrink...
				button.setEnabled(false);
				for (int i = initialOffset; i < midpoint; i += increment) {
					cOffset = i;
					repaint();
					snooze();
				}

				// Flip...
				topDown = !topDown;
				initialise();

				// Expand...
				for (int i = midpoint; i >= initialOffset; i -= increment) {
					cOffset = i;
					repaint();
					snooze();
				}
				button.setEnabled(true);

				repaint();
			}
		};

		new Thread(r).start();
	}

	private void snooze() {
		try {
			Thread.sleep(50);
		} catch (InterruptedException e) {
			 System.out.println("MapPanel(); you interrupted my snooze..");
		}
	}
}
