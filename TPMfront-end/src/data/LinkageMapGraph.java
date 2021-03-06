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

public class LinkageMapGraph implements Serializable {
	static final long serialVersionUID = 76713199780917726L;

	private String name;
	private LinkageGroup[] lGroups;

	public LinkageMapGraph(LinkageGroup[] lGroups) {
		this.lGroups = lGroups;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public LinkageGroup[] getLinkageGroups() {
		return lGroups;
	}
}
