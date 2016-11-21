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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Stroke;

import javax.swing.JComponent;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.plaf.basic.BasicSliderUI;

/**
 *
 * @see http://stackoverflow.com/a/12297384/714968
 */
public class CustomSlider2UI extends BasicSliderUI {

	private BasicStroke stroke = new BasicStroke(1f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 0f,
			new float[] { 1f, 2f }, 0f);

	public CustomSlider2UI(JSlider b) {
		super(b);
	}

	@Override
	public void paint(Graphics g, JComponent c) {
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		super.paint(g, c);
	}

	@Override
	public void paintTrack(Graphics g) {
		Graphics2D g2d = (Graphics2D) g;
		final Stroke old = g2d.getStroke();
		g2d.setStroke(stroke);
		// g2d.setPaint(Color.BLACK);
		g2d.setPaint(Color.RED);
		if (slider.getOrientation() == SwingConstants.HORIZONTAL) {
			g2d.drawLine(trackRect.x, trackRect.y + trackRect.height / 2, trackRect.x + trackRect.width,
					trackRect.y + trackRect.height / 2);
		} else {
			g2d.drawLine(trackRect.x + trackRect.width / 2, trackRect.y, trackRect.x + trackRect.width / 2,
					trackRect.y + trackRect.height);
		}
		g2d.setStroke(old);
	}

	@Override
	public void paintThumb(Graphics g) {
		Graphics2D g2d = (Graphics2D) g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
		Rectangle t = thumbRect;
		g2d.drawImage(Icons.RIGHT_ARROW2.getImage(), t.x, t.y, null);
	}

	@Override
	protected Dimension getThumbSize() {
		return new Dimension(Icons.RIGHT_ARROW2.getIconWidth(), Icons.RIGHT_ARROW2.getIconHeight());
	}
}