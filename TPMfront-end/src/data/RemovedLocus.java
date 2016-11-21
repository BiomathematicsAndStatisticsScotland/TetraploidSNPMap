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

// Wrapper class for Marker that provides the ability for the same Marker within
// different LinkageGroups (possible from different analyses results) to have a
// different checked state
public class RemovedLocus implements Serializable
{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 5745405660868065014L;
	private String removedLocusName, similarLocusName, similarity;
		
	public RemovedLocus(String removedLocusName, String similarLocusName, String similarity)
	{
		this.removedLocusName = removedLocusName;
		this.similarLocusName = similarLocusName;
		this.similarity = similarity;
	}
	
	private String space(int n)
	{
		String r = "";
		while(n-- > 0) 
			r += " ";
		return r;
	}
	public String toString()
	{
		return removedLocusName + space(33-removedLocusName.length()) + similarLocusName + space(33-similarLocusName.length()) + similarity; 
	}
	public String getRemovedLocusName() { return removedLocusName; }
	public String getSimilarLocusName() { return similarLocusName; }
	public String getSimilarity() { return similarity; }
	
}