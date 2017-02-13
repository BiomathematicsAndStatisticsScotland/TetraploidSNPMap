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

import gui.Prefs;

public class TraitFile implements Serializable
{
	static final long serialVersionUID = 2675383049059248499L;
	
	private Vector<String> names = new Vector<String>();
	private Vector<float[]> rows = new Vector<float[]>();
	private Vector<Boolean> enabled;
	
	public TraitFile()
	{
	}
	
	public void addName(String name) throws CreationException {
		if(name.length() > Prefs.traitname_maxlen){
			throw new CreationException(CreationException.TRAITNAME_TOO_LONG);
		}
		names.add(name);
	}
	
	public void addRow(float[] row)
		{ rows.add(row); }
	
	public Vector<String> getNames()
		{ return names; }
	
	public Vector<float[]> getRows()
		{ return rows; }
	
	public Vector<Boolean> getEnabled()
	{ 
		if (enabled == null)
		{
			enabled = new Vector<Boolean>();
			for (int i = 0; i < names.size(); i++)
				enabled.add(true);
		}
	
		return enabled;
	}
	
	public int getSelectedCount()
	{
		int count = 0;
		for (Boolean b: getEnabled())
			if (b) count++;
		
		//System.out.println("Count is " + count);
		return count;
	}
}
