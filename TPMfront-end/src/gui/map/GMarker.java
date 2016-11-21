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

import java.awt.Rectangle;
import data.CMarker;

class GMarker {
	// The marker's name
	String name;
	// Original Marker it represents
	CMarker marker;
	// The MapPanel object it is being drawn upon
	MapPanel panel;

	// Reference to the previous marker in the chromosome
	GMarker prev;

	// Its position in centiMorgans
	float cm;
	// Actual % position on the map (how far down the map this marker should be)
	float aPos;
	// Final position as decided graphically at display-time
	int pos;

	// Graphical position on the map
	Rectangle rec = new Rectangle();

	// Width of the string (rectangle) required to render this marker
	int recW;
	// Height of the string (rectangle) required to render this marker
	int recH;
	// Half the height of this rectangle
	int recHalf;

	// Are we drawing this marker topDown or bottomUp
	boolean topDown;

	boolean highlight = false;

	GMarker(CMarker marker, String name) {
		this.marker = marker;
		this.name = name;
	}

	public String toString() {
		return name;
	}

	// Clones (the non-graphical) fields of this object and returns a new object
	// containing them. Used so that the GMarkers on the "all" chromosome can be
	// easily copied into GMarkers on the individual chromosomes
	GMarker getClone() {
		GMarker gMkr = new GMarker(marker, name);

		gMkr.cm = cm;
		gMkr.aPos = aPos;

		return gMkr;
	}

	void setStringBounds(MapPanel panel, int width, int height) {
		recW = width;
		recH = height;
		recHalf = height / 2;

		this.panel = panel;
		topDown = panel.topDown;
	}

	// Given the offset of the chromosome from the top of the screen, and the
	// height of the chromosome: works out the "initial" position rectangle to
	// draw this marker's text at
	void setInitialRectangle(int cOffset, int ch, int rSide) {
		if (topDown) {
			pos = cOffset + (int) (aPos * ch);
		} else {
			pos = cOffset + ch - (int) (aPos * ch);
		}
		rec = new Rectangle(rSide - recW, pos - recHalf, recW, recH);
	}

	// Tests for a marker being above (or below) the top (or bottom) of the
	// chromosome, and resets its position to stop it
	boolean limitPosition() {
		// Ensure it's not above the top
		// if (rec.y <= panel.C_OFFSET-recHalf)
		if (rec.y <= panel.cOffset - recH) {
			// System.out.println("rec.y was above the top." + rec.y + " ;
			// adjusted to " + (panel.C_OFFSET-recHalf));
			// BB - rec.y = panel.C_OFFSET-recHalf;
			return true;
		}

		// Ensure it's not below the bottom
		int bottom = panel.cOffset + panel.ch + recHalf;
		// BB - if (rec.y+recH >= bottom)
		if (rec.y + recHalf >= bottom) {
			// System.out.println("rec.y+recH was below the bottom." + (rec.y +
			// recH) + " ; adjusted to " + (bottom - recH));
			// BB - rec.y = bottom - recH;
			return true;
		}

		return false;
	}

	// Optimises this marker's position rectangle by seeing if it overlaps with
	// any other marker's rectangle, and if so, asks that marker to adjust its
	// rectangle to make room
	int optimiseRectangle(int shift, boolean force) {
		if (shift != -1) {
			if (topDown) {
				// System.out.println("marker " + name + " shifted down by " +
				// shift);
				rec.y = rec.y - shift;
			} else {
				// System.out.println("marker " + name + " shifted UP by " +
				// shift);
				rec.y = rec.y + shift;
			}
		}

		boolean fixed = limitPosition();

		if (prev != null) {
			int overlap = overlap();
			// System.out.println("overlap: " + overlap);
			if (overlap > -1) {
				if (fixed || force) {
					prev.optimiseRectangle(overlap, true);
				} else {
					rec.y = prev.optimiseRectangle(overlap / 2, force);
					// if(topDown)
					// System.out.println(name + " moved to " + (rec.y +
					// rec.height) + " by " + prev.name);
					// else
					// System.out.println(name + " moved to " + (rec.y -
					// rec.height) + " by " + prev.name);
				}
			}
		}

		if (topDown) {
			// System.out.println(name + " moved to " + (rec.y + rec.height) + "
			// by " + prev.name);
			return rec.y + rec.height;
		} else {
			// System.out.println(name + " moved to " + (rec.y - rec.height) + "
			// by " + prev.name);
			return rec.y - rec.height;
		}
	}

	// Determines if this marker's position rectangle overlaps any of the other
	// marker's rectangles, returning -1 if not, or the amount of overlap (in
	// pixels) if it does
	private int overlap() {
		GMarker m1 = this;
		GMarker m2 = this.prev;

		while (m1 != null && m2 != null) {
			int m1Bot = m1.rec.y + m1.rec.height;
			int m2Bot = m2.rec.y + m2.rec.height;

			// Top of new rectangle is above bottom of prev
			if (topDown && m1.rec.y < m2Bot) {
				return (m2Bot - m1.rec.y);
			} else if (!topDown && m1Bot > m2.rec.y) {
				return (m1Bot - m2.rec.y);
			}

			m1 = m2;
			m2 = m1.prev;
		}

		return -1;
	}
}