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

/** AlleleState. NONSNP only.
 * 
 * 
 *
 */
public class AlleleState implements Serializable {
	static final long serialVersionUID = -1163321568714213767L;

	public static final  byte PRESENT = 1;
	public static final  byte ABSENT = 0;
	public static final  byte UNKNOWN = 9;

	private byte state;

	/** AlleleState(). NONSNP only.
	 * 
	 * @param state = byte, 0 (absent), 1 (present), 9 (unknown)
	 */
	public AlleleState(byte state) {
		this.state = state;
	}

	public void setState(byte state) {
		this.state = state;
	}

	public byte getState() {
		return state;
	}
}