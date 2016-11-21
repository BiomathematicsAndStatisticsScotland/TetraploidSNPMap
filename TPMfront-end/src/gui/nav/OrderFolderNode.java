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

import data.LinkageGroup;
import data.OrderedResult;

public class OrderFolderNode {
	public OrderedResult order;
	public LinkageGroup lGroup;

	OrderFolderNode(OrderedResult order, LinkageGroup lGroup) {
		this.order = order;
		this.lGroup = lGroup;
	}

	public OrderedResult getResult() {
		return this.order;
	}

	public String toString() {
		return order.toString();
	}
}
