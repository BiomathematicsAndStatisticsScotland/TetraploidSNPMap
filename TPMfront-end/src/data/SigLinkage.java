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

public class SigLinkage implements Serializable
{
	static final long serialVersionUID = 5919462199368603914L;
	
	//public final Marker marker;
	public final String markerName;
	public final String markerPrefix;
	
	public final float chi;
	public final float sig;
	public final String phase;
	
	public SigLinkage(Marker m, float c, float s, String ph)
	{
		//marker = m;
		markerName = m.getName();
		markerPrefix = m.getPrefix();
		chi = c;
		sig = s;
		phase = ph;
	}
	
	public SigLinkage(Marker m, float c, float s)
		{ this(m, c, s, null); }
}