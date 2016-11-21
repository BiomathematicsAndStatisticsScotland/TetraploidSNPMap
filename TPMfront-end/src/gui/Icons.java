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

import javax.swing.ImageIcon;

public class Icons {
	public static ImageIcon AA, ANOVA, ANTIALIAS, BALLOON, BB2, CLUSTER_FOLDER;
	public static ImageIcon CLUSTER_OPEN, COLOR_DENDROGRAM, DELETE, DENDROGRAM;
	public static ImageIcon EXCEL, FLIP, FLIP2, FOLDER, FOLDER_OPEN;
	public static ImageIcon GRAPH_SCALE, GROUP_FOLDER, GROUP_OPEN, HIDE_OVERALL;
	public static ImageIcon IMPORT, LINKAGE_GROUP, LOG, MAP, NEW_PROJECT, OPEN;
	public static ImageIcon ORDER, PERM, PRINT, QTL, RADIAL, RESCAN, RIGHT_ARROW;
	public static ImageIcon RIGHT_ARROW2, RUN_ANOVA, RUN_CLUSTER, RUN_MAP, RUN_SIMANNEAL;
	public static ImageIcon RUN_TWOPOINT, SAVE, SAVE_IMAGE, SAVE_TEXT, SCALE;
	public static ImageIcon SELECT_MARKERS, SUMMARY, WARNING, ZOOM_IN, ZOOM_OUT;

	/** Icons.
	 * 
	 */
	public Icons() {
		@SuppressWarnings("rawtypes")
		Class c = getClass();

		try {
			AA = new ImageIcon(c.getResource("/aa.png"));
			ANOVA = new ImageIcon(c.getResource("/anova.png"));
			ANTIALIAS = new ImageIcon(c.getResource("/antialias.png"));
			// APP = new ImageIcon(c.getResource("/app.png"));
			BALLOON = new ImageIcon(c.getResource("/balloon.png"));
			BB2 = new ImageIcon(c.getResource("/bb2.png"));
			CLUSTER_FOLDER = new ImageIcon(c.getResource("/cluster_folder.png"));
			CLUSTER_OPEN = new ImageIcon(c.getResource("/cluster_open.png"));
			COLOR_DENDROGRAM = new ImageIcon(c.getResource("/color_dendrogram.png"));
			DELETE = new ImageIcon(c.getResource("/delete.png"));
			DENDROGRAM = new ImageIcon(c.getResource("/dendrogram.png"));
			// EXCEL = new ImageIcon(c.getResource("/excel.png"));
			FLIP = new ImageIcon(c.getResource("/flip.png"));
			FLIP2 = new ImageIcon(c.getResource("/flip2.png"));
			FOLDER = new ImageIcon(c.getResource("/folder.png"));
			FOLDER_OPEN = new ImageIcon(c.getResource("/folder_open.png"));
			GRAPH_SCALE = new ImageIcon(c.getResource("/graph_scale.png"));
			GROUP_FOLDER = new ImageIcon(c.getResource("/group_folder.png"));
			GROUP_OPEN = new ImageIcon(c.getResource("/group_open.png"));
			HIDE_OVERALL = new ImageIcon(c.getResource("/hide_overall.png"));
			IMPORT = new ImageIcon(c.getResource("/import.png"));
			LINKAGE_GROUP = new ImageIcon(c.getResource("/linkage_group.png"));
			LOG = new ImageIcon(c.getResource("/log.png"));
			MAP = new ImageIcon(c.getResource("/map.png"));
			NEW_PROJECT = new ImageIcon(c.getResource("/new_project.png"));
			OPEN = new ImageIcon(c.getResource("/open.png"));
			ORDER = new ImageIcon(c.getResource("/order.png"));
			PERM = new ImageIcon(c.getResource("/perm.png"));
			PRINT = new ImageIcon(c.getResource("/print.png"));
			QTL = new ImageIcon(c.getResource("/qtl.png"));
			RADIAL = new ImageIcon(c.getResource("/radial.png"));
			RESCAN = new ImageIcon(c.getResource("/rescan.png"));
			RIGHT_ARROW = new ImageIcon(c.getResource("/right.png"));
			RIGHT_ARROW2 = new ImageIcon(c.getResource("/right2.png"));
			RUN_ANOVA = new ImageIcon(c.getResource("/run_anova.png"));
			RUN_CLUSTER = new ImageIcon(c.getResource("/run_cluster.png"));
			RUN_MAP = new ImageIcon(c.getResource("/run_map.png"));
			RUN_SIMANNEAL = new ImageIcon(c.getResource("/run_simanneal.png"));
			RUN_TWOPOINT = new ImageIcon(c.getResource("/run_twopoint.png"));
			SAVE = new ImageIcon(c.getResource("/save.png"));
			SAVE_IMAGE = new ImageIcon(c.getResource("/save_image.png"));
			SAVE_TEXT = new ImageIcon(c.getResource("/save_text.png"));
			SCALE = new ImageIcon(c.getResource("/scale.png"));
			SELECT_MARKERS = new ImageIcon(c.getResource("/select_markers.png"));
			SUMMARY = new ImageIcon(c.getResource("/summary.png"));
			WARNING = new ImageIcon(c.getResource("/warning.png"));
			ZOOM_IN = new ImageIcon(c.getResource("/zoom_in.png"));
			ZOOM_OUT = new ImageIcon(c.getResource("/zoom_out.png"));
		} catch (NullPointerException e) {
			e.printStackTrace();
			System.out.println(e);
			System.exit(1);
		}
	}
}