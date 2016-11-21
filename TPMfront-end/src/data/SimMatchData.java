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
import java.util.Vector;

public class SimMatchData implements Serializable
{
	static final long serialVersionUID = -4940031113702898385L;
	
	// TODO: At some point maybe - why can't I create typesafe vector of vectors
	// or an array of Vector<SigLinkage> objects
	
	public Vector<SigLinkage> p1A;
	public Vector<SigLinkage> p2A;
	
	public Vector<SigLinkage> p1B;	
	public Vector<SigLinkage> p2B;
	
	public Vector<SigLinkage> p1C;
	public Vector<SigLinkage> p2C;
	
	public SimMatchData()
	{
		p1A = new Vector<SigLinkage>();
		p2A = new Vector<SigLinkage>();
		
		p1B = new Vector<SigLinkage>();		
		p2B = new Vector<SigLinkage>();
		
		p1C = new Vector<SigLinkage>();
		p2C = new Vector<SigLinkage>();
	}
	
	public void add(Vector<SigLinkage> array, SigLinkage data)
	{
		for (int i = 0; i < array.size(); i++)
		{
			if (array.get(i).chi <= data.chi)
			{
				array.add(i, data);
				return;
			}
		}
		
		array.add(data);
	}
}