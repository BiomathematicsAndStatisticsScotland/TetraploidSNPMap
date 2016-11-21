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

public class PhasePair implements Serializable
{
	static final long serialVersionUID = -9043725362181996778L;
	
	public CMarker cm1, cm2;
	public String phase1, phase2;
	
	// Recombination frequency and LOD score
	public float rfq, lod;
	
	PhasePair(CMarker m1, CMarker m2, String p1, String p2)
	{
		cm1 = m1;
		cm2 = m2;
		phase1  = p1;
		phase2  = p2;
	}
	public void showdata()
	{
		System.out.println("pair data for " + cm1.marker.getName() + " and " + cm2.marker.getName() + " = (" + rfq + ", " + lod + ")");
	}
	public int compare(CMarker m1, CMarker m2)
	{
		// 1st case
		if (cm1.marker == m1.marker && cm2.marker == m2.marker)
			return 1;
		
		// Alternate case
		if (cm1.marker == m2.marker && cm2.marker == m1.marker)
			return 2;
		
		return 0;
	}
	
	// Given the marker 'cm', returns the other marker in this pairing
	public CMarker getPartner(CMarker cm)
	{
		if (cm1.marker == cm.marker)
			return cm2;
		else if (cm2.marker == cm.marker)
			return cm1;
		else
			return null;
	}
	
	// Returns true if this PhasePair contains the two markers
	public boolean hasMarkers(CMarker m1, CMarker m2)
	{
		if (cm1.marker == m1.marker && cm2.marker == m2.marker ||
			cm1.marker == m2.marker && cm2.marker == m1.marker)
		{
			return true;
		}
		
		return false;
	}
}