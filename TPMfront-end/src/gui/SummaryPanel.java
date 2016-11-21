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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import data.CMarker;
import data.LinkageGroup;
import data.Summary;
import doe.DoeLayout;
import doe.MsgBox;

public class SummaryPanel extends JPanel implements ActionListener {
	private static final long serialVersionUID = -6965905010678039817L;
	private Summary summary;
	private JButton button;

	/** SummaryPanel().
	 * 
	 */
	public SummaryPanel(Summary summary) {
		this.summary = summary;


		button = new JButton("Click here to reselect these markers");
		button.addActionListener(this);
		JPanel bPanel = new JPanel(new FlowLayout());
		bPanel.add(button);

		DoeLayout layout = new DoeLayout();
		JLabel timeLabel = new JLabel(summary.getTimeSummary(), JLabel.CENTER);
		layout.add(timeLabel, 0, 0, 1, 1, new Insets(5, 5, 5, 5));
		JLabel mkrsLabel = new JLabel(summary.getMarkersSummary(), JLabel.CENTER);
		layout.add(mkrsLabel, 0, 1, 1, 1, new Insets(5, 5, 5, 5));
		JLabel mkrsLabel2 = new JLabel(summary.getMarkersSummary2(), JLabel.CENTER);
		layout.add(mkrsLabel2, 0, 2, 1, 1, new Insets(5, 5, 5, 5));
		layout.add(bPanel, 0, 3, 1, 1, new Insets(5, 5, 5, 5));

		setLayout(new BorderLayout());
		add(layout.getPanel());
	}

	/** listens to the UI buttons.
	 * 
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == button) {
			LinkageGroup selGroup = summary.getSelectedGroup();
			LinkageGroup oriGroup = summary.getOriginalGroup();

			int count = oriGroup.getSelectedMarkerCount();
			MsgBox.msg(count + " marker" + (count == 1 ? " has " : "s have ")
					+ "are now selected in the original linkage group.", MsgBox.INF);
			for (CMarker cm : oriGroup.getMarkers()) {
				cm.checked = false;
			}

			for (CMarker cmO : oriGroup.getMarkers()) {
				for (CMarker cmS : selGroup.getMarkers()) {
					if (cmS.marker == cmO.marker) {
						cmO.checked = true;
						break;
					}
				}
			}

			count = oriGroup.getSelectedMarkerCount();
			MsgBox.msg(count + " marker" + (count == 1 ? " has " : "s have ")
					+ "been reselected in the original linkage group.", MsgBox.INF);
		}

	}
}