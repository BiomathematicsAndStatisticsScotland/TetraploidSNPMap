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

package data;

import java.io.Serializable;
import java.text.DateFormat;
import java.util.LinkedList;
import gui.AppFrameMenuBar;

/** AnalysisLog. SNP & NONSNP.
 * 
 * 
 *
 */
public class AnalysisLog implements Serializable {
	static final long serialVersionUID = 3154353755671453381L;

	private static DateFormat df1 = DateFormat.getDateInstance(DateFormat.MEDIUM);
	private static DateFormat df2 = DateFormat.getTimeInstance(DateFormat.MEDIUM);

	private LinkedList<LogEntry> log = new LinkedList<LogEntry>();

	/** add(String logentry).
	 * 
	 * @param entry = the (String) log entry to add.
	 */
	public void add(String entry) {
		long now = System.currentTimeMillis();
		String time = df1.format(now) + " " + df2.format(now);

		log.add(new LogEntry(time, entry));

		AppFrameMenuBar.aFileSave.setEnabled(true);
	}

	/** Converts the log into a single String that can returned.
	 * 
	 */
	public String getLog() {
		StringBuffer buffer = new StringBuffer(1000);

		buffer.append("<font size=\"3\" face=\"Monospaced\">");
		for (LogEntry l : log) {
			buffer.append("<p><b>" + l.time + "</b><br>" + l.entry + "</p>");
		}
		buffer.append("</font>");

		return buffer.toString();
	}

	private class LogEntry implements Serializable {
		private static final long serialVersionUID = 83059570974892506L;
		String time;
		String entry;

		LogEntry(String time, String entry) {
			this.time = time;
			this.entry = entry;
		}

		public String toString() {
			return time + " - " + entry;
		}
	}
}