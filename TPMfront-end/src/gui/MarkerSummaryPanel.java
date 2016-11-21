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
import java.awt.Font;
import java.awt.Insets;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import data.Marker;
import data.SigLinkage;

class MarkerSummaryPanel extends JPanel {
	private static final long serialVersionUID = -1954370226619989663L;
	private GradientPanel gPanel;
	private JTabbedPane tabs;
	private MarkerInfoPanel infoPanel;
	private MarkerParentPanel p1Panel, p2Panel;

	MarkerSummaryPanel() {
		tabs = new JTabbedPane();

		infoPanel = new MarkerInfoPanel();
		tabs.addTab("Summary", null, infoPanel, null);

		p1Panel = new MarkerParentPanel(1);
		tabs.addTab("Parent1 Linkages", null, p1Panel, null);
		p2Panel = new MarkerParentPanel(2);
		tabs.addTab("Parent2 Linkages", null, p2Panel, null);

		setLayout(new BorderLayout());
		add(gPanel = new GradientPanel(""), BorderLayout.NORTH);
		add(tabs);
	}

	void setMarker(Marker marker) {
		AppFrameMenuBar.aMove.setEnabled(false);

		if (marker != null) {
			gPanel.setTitle(marker.getName());
			if (AppFrame.navPanel.getCurrentClusterHeadNode() != null) {
				AppFrameMenuBar.aMove.setEnabled(true);
			}
		} else {
			gPanel.setTitle("");
		}

		infoPanel.setMarker(marker);
		p1Panel.setMarker(marker);
		p2Panel.setMarker(marker);
	}

	private class MarkerInfoPanel extends JPanel {
		private static final long serialVersionUID = 4103773746670508265L;
		private JEditorPane text;

		MarkerInfoPanel() {
			text = new JEditorPane("text/html", "");
			text.setFont(new Font("Monospaced", Font.PLAIN, 11));
			text.setMargin(new Insets(2, 5, 2, 5));
			text.setEditable(false);
			JScrollPane sp = new JScrollPane(text);

			setLayout(new BorderLayout());
			add(sp);
		}

		void setMarker(Marker marker) {
			if (marker != null) {
				text.setText(marker.getSummaryInfo());
				text.setCaretPosition(0);
			}
		}
	}

	private class MarkerParentPanel extends JPanel {
		private static final long serialVersionUID = -7558413742664711936L;
		private JEditorPane text;
		private int parent;

		MarkerParentPanel(int parent) {
			this.parent = parent;

			text = new JEditorPane("text/html", "");
			text.setFont(new Font("Monospaced", Font.PLAIN, 11));
			text.setMargin(new Insets(2, 5, 2, 5));
			text.setEditable(false);
			JScrollPane sp = new JScrollPane(text);

			setLayout(new BorderLayout());
			add(sp);
		}

		void setMarker(Marker marker) {
			if (marker != null) {
				String str = "<html><pre><font size='3'>";
				str += "<b>SC Group    Marker Name         Chi^2   Sig         " + "Phase</b><br><br>";

				if (parent == 1) {
					str += "<b>Simplex-duplex linkages</b>";
					for (SigLinkage sig : marker.getSimMatchData().p1A) {
						str += showSig(sig);
					}

					str += ("<br><br><b>Simplex-3 to 1 linkages</b>");
					for (SigLinkage sig : marker.getSimMatchData().p1B) {
						str += showSig(sig);
					}

					str += ("<br><br><b>Other linkages to simplex markers</b>");
					for (SigLinkage sig : marker.getSimMatchData().p1C) {
						str += showSig(sig);
					}
				} else {
					str += ("<b>Simplex-duplex linkages</b>");
					for (SigLinkage sig : marker.getSimMatchData().p2A) {
						str += showSig(sig);
					}

					str += ("<br><br><b>Simplex-3 to 1 linkages</b>");
					for (SigLinkage sig : marker.getSimMatchData().p2B) {
						str += showSig(sig);
					}

					str += ("<br><br><b>Other linkages to simplex markers</b>");
					for (SigLinkage sig : marker.getSimMatchData().p2C) {
						str += showSig(sig);
					}
				}

				str += "</html>";
				text.setText(str);
				text.setCaretPosition(0);
			}
		}

		private String set(String value, int size) {
			if (value.length() < size) {
				for (int i = value.length(); i < size; i++) {
					value += " ";
				}
			}

			return value;
		}

		private String showSig(SigLinkage sig) {
			return "<br>" + set(sig.markerPrefix, 12) + set(sig.markerName, Prefs.markername_maxlen + 1)
					+ Prefs.d3.format(sig.chi) + "  " + Prefs.d8.format(sig.sig) + "  "
					+ (sig.phase == null ? "" : sig.phase);
		}
	}
}
