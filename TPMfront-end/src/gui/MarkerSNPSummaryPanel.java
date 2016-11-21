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

class MarkerSNPSummaryPanel extends JPanel {
	private static final long serialVersionUID = 2137152488074847143L;
	private GradientPanel gPanel;
	private JTabbedPane tabs;

	private MarkerSNPInfoPanel infoPanel;
	private MarkerSNPParentPanel p1Panel, p2Panel;

	MarkerSNPSummaryPanel() {
		tabs = new JTabbedPane();

		infoPanel = new MarkerSNPInfoPanel();
		tabs.addTab("Summary", null, infoPanel, null);

		p1Panel = new MarkerSNPParentPanel(1);
		tabs.addTab("Parent1 Linkages", null, p1Panel, null);
		p2Panel = new MarkerSNPParentPanel(2);
		tabs.addTab("Parent2 Linkages", null, p2Panel, null);

		setLayout(new BorderLayout());
		add(gPanel = new GradientPanel(""), BorderLayout.NORTH);
		add(tabs);
	}

	void setMarker(Marker marker) {
		if (AppFrame.tpmmode != AppFrame.TPMMODE_QTL) {
			AppFrameMenuBar.aMove.setEnabled(false);
		}

		if (marker != null) {
			gPanel.setTitle(marker.getName());
			if (AppFrame.navPanel.getCurrentClusterHeadNode() != null) {
				if (AppFrame.tpmmode != AppFrame.TPMMODE_QTL) {
					AppFrameMenuBar.aMove.setEnabled(true);
				}
			}
		} else {
			gPanel.setTitle("");
		}

		infoPanel.setMarker(marker);
		p1Panel.setMarker(marker);
		p2Panel.setMarker(marker);
	}

	private class MarkerSNPInfoPanel extends JPanel {
		private static final long serialVersionUID = -2720710533672731091L;
		private JEditorPane text;

		MarkerSNPInfoPanel() {
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
				text.setText(marker.getSNPSummaryInfo());
				text.setCaretPosition(0);
			}
		}
	}

	private class MarkerSNPParentPanel extends JPanel {
		private static final long serialVersionUID = -5996798731072919623L;
		private JEditorPane text;
		private int parent;

		MarkerSNPParentPanel(int parent) {
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
				str += "<b>SC Group    Marker Name                    Chi^2   Sig         " 
						+ "Phase</b><br><br>";

				if (parent == 1) {
					str += "<b>Simplex-duplex linkages</b>";
					for (SigLinkage sig : marker.getSimMatchData().p1A) {
						str += showSig(sig);
					}

					str += ("<br><br><b>Simplex-double simplex linkages</b>");
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

					str += ("<br><br><b>Simplex-double simplex linkages</b>");
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
