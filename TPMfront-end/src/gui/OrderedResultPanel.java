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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import analyses.order.mds.ShowThreeD;
import data.OrderedResult;
import doe.DoeLayout;
import gui.nav.NavPanel;

public class OrderedResultPanel extends JPanel implements ActionListener {
	private static final long serialVersionUID = 6422907889258950320L;
	private OrderedResult order;
	private TwopointPWDPanel pwdPanel;
	private TwopointPWDPanelnonsnp pwdPanelnonsnp;
	private TwopointQTLPanel qtlPanel;
	private JButton tdbutton;
	private JTabbedPane tabs = new JTabbedPane();
	protected Process proc = null;
	private ShowThreeD std;

	/** OrderedResultPanel().
	 * 
	 */
	public OrderedResultPanel(OrderedResult order, NavPanel navPanel) {
		this.order = order;
		if (order.is_nonsnp()) {
			pwdPanelnonsnp = new TwopointPWDPanelnonsnp(order);
			JPanel p1 = new JPanel(new BorderLayout(5, 5));
			p1.add(pwdPanelnonsnp);
			JScrollPane sp1 = new JScrollPane(p1);
			tabs.addTab("twopoint.pwd", sp1);
			JScrollPane sp2 = new JScrollPane(getTextArea(order.tp2.toString()));
			tabs.addTab("twopoint.out", sp2);
			setLayout(new BorderLayout());
			add(new GradientPanel("Ordered Results"), BorderLayout.NORTH);
			add(tabs);
		} else if (order.is_twopoint()) {
			pwdPanel = new TwopointPWDPanel(order);
			JPanel p1 = new JPanel(new BorderLayout(5, 5));
			p1.add(pwdPanel);
			JScrollPane sp1 = new JScrollPane(p1);
			tabs.addTab("twopoint.pwd", sp1);
			if (!order.tp2.toString().isEmpty()) {
				JScrollPane sp2 = new JScrollPane(getTextArea(order.tp2.toString()));
				tabs.addTab("twopoint.SNPout", sp2);
			}
			if (!order.tp2a.toString().isEmpty()) {
				JScrollPane sp2a = new JScrollPane(getTextArea(order.tp2a.toString()));
				tabs.addTab("twopoint.SNPfullout", sp2a);
			}
			String excludedDups = order.getExcludedDuplicateString();
			if (!"".equals(excludedDups)) {
				JScrollPane sp3 = new JScrollPane(getTextArea(excludedDups));
				tabs.addTab("Excluded duplicates", sp3);
			}
			setLayout(new BorderLayout());
			add(new GradientPanel("Twopoint results"), BorderLayout.NORTH);
			add(tabs);
		} else if (order.is_mds()) {
			pwdPanel = new TwopointPWDPanel(order);
			JPanel p1 = new JPanel(new BorderLayout(5, 5));
			p1.add(pwdPanel);
			tabs.addTab("MDS/twopoint result", p1);
			if (!order.tp1.toString().isEmpty()) {
				JScrollPane sp1 = new JScrollPane(getTextArea(order.tp1.toString()));
				tabs.addTab("twopoint.SNPpwd", sp1);
			}
			if (!order.tp2.toString().isEmpty()) {
				JScrollPane sp2 = new JScrollPane(getTextArea(order.tp2.toString()));
				tabs.addTab("twopoint.SNPout", sp2);
			}
			if (!order.tp3.toString().isEmpty()) {
				JScrollPane sp8 = new JScrollPane(getTextArea(order.loclist()));

				if (order.is3d) {
					DoeLayout layout = new DoeLayout();
					layout.add(getThreeDeeButton(), 0, 1, 1, 1, new Insets(5, 5, 5, 5));
					layout.add(getMDSImage(), 0, 2, 1, 1, new Insets(0, 5, 5, 5));
					tabs.addTab("MDS", new JScrollPane(layout.getPanel()));
				} else {
					tabs.addTab("MDS", new JScrollPane(getMDSImage()));
				}
				tabs.addTab("Locations", sp8);
				tabs.addTab("Mean(nnfit)", new JScrollPane(getTextArea(Prefs.d3.format(order.meannnfit))));
			}

			setLayout(new BorderLayout());
			add(new GradientPanel("Ordered twopoint results"), BorderLayout.NORTH);
			add(tabs);

		} else if (order.is_readqtl()) {
			qtlPanel = new TwopointQTLPanel(order);
			JPanel p1 = new JPanel(new BorderLayout(5, 5));
			p1.add(qtlPanel);
			JScrollPane sp1 = new JScrollPane(p1);
			setLayout(new BorderLayout());
			add(new GradientPanel("Ordered Results"), BorderLayout.NORTH);
			add(sp1);
		} else {
			System.out.println("My OrderedResult it not MDS or TWOPOINT or NONSNP");
		}
	}

	private JPanel getThreeDeeButton() {
		tdbutton = new JButton("Popup 3D");
		tdbutton.addActionListener(this);
		JPanel jp = new JPanel();
		jp.add(tdbutton);
		return jp;
	}

	private JTextArea getTextArea(String str) {
		JTextArea text = new JTextArea(str);

		text.setFont(new Font("Monospaced", Font.PLAIN, 11));
		;
		text.setMargin(new Insets(2, 5, 2, 5));

		return text;
	}

	private JLabel getMDSImage() {
		try {
			JLabel imgLabel = new JLabel(order.MDSimage);
			return imgLabel;
		} catch (Exception e) {
			System.out.println("ERROR while reading PNG file twopointpc.png : " + e);
			return null;
		}
	}

	/** listen to the 'ThreeD' button.. 
	 * 
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == tdbutton) {
			File mds3dSmacConf = new File(Prefs.tools_scratch, "smacofsym.conf");
			File mds3dLocikey = new File(Prefs.tools_scratch, "locikey");
			File mds3dPc = new File(Prefs.tools_scratch, "pc");
			File showthreeD = new File(Prefs.tools_Rscripts_path, "threeD.R");
			File showthreeDc = new File(Prefs.tools_scratch, "threeD.R");
			try {
				order.writeMDS3dResults(mds3dSmacConf, mds3dLocikey, mds3dPc);
				AppFrame.copyInputFile(showthreeD, showthreeDc);
			} catch (Exception ee) {
				System.out.println("ERROR while writing 3d data files : " + ee);
				return;
			}
			std = new ShowThreeD();
			std.start();
		}
	}
}