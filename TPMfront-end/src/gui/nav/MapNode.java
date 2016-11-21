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

package gui.nav;

import data.LinkageMapGraph;
import data.OrderedResult;
import gui.IPrintable;
import gui.map.LinkageMapPanel;

class MapNode implements IPrintable {
	LinkageMapGraph map;
	LinkageMapPanel panel;

	MapNode(OrderedResult order, LinkageMapGraph map) {
		this.map = map;
		panel = new LinkageMapPanel(order, map.getLinkageGroups());
	}

	public String toString() {
		return map.getName();
	}

	public boolean isPrintable() {
		return true;
	}

	public void print() {
		panel.print();
	}
}
