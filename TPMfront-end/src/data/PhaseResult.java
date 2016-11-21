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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.Serializable;

public class PhaseResult implements Serializable
{
	static final long serialVersionUID = 6575547173738096739L;
	public StringBuffer pr1 = new StringBuffer();
	private String name;
	
	public void setName(String name)
	{ 
		this.name = name;
	}

	public String getName()
	{ 
		return name;
	}
	
	public String toString()
	{ return name; }
	
	public PhaseResult()
	{
		
	}
	public void setPhaseResults(File f1)
			throws Exception
		{
			readFile(pr1, f1);
		}
	private void readFile(StringBuffer str, File file)
			throws Exception
		{
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			String line = in.readLine();
			while (line != null)
			{
				str.append(line + "\n");
				line = in.readLine();
			}
				
			in.close();
		}
}