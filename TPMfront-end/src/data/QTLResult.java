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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.Serializable;
import java.util.Vector;

public class QTLResult implements Serializable
{
	static final long serialVersionUID = -5088939235379013495L;
	
	private TraitFile traitFile;
	
	private Vector<Trait> traits = new Vector<Trait>();
	private String name;
	
	private String backupFile;
	public StringBuffer qmm = new StringBuffer();
	
	public QTLResult()
	{
	}
	
	public Vector<Trait> getTraits()
		{ return traits; }
	
	public String getName()
		{ return name; }
	
	public void setName(String name)
		{ this.name = name; }
	
	public String getBackupFile()
		{ return backupFile; }
	
	public void setBackupFile(String backupFile)
		{ this.backupFile = backupFile; }
	public void StoreQMM(File file)
	{
		try {
			readFile(qmm, file);
		} catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	public void WriteQMM(File file)
	{
		try {
			writeFile(qmm, file);
		}catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	private void readFile(StringBuffer str, File file)
			throws Exception
	{
			//System.out.println("qtlresult.readfile() - strbuf: "  + str);
			//System.out.println("qtlresult.readfile() - file: "  + file);
			BufferedReader in = new BufferedReader(new FileReader(file));
			//System.out.println("qtlresult.readfile() - in: "  + in);
			String line = in.readLine();
			while (line != null)
			{
				str.append(line + "\n");
				line = in.readLine();
			}
				
			in.close();
	}
	private void writeFile(StringBuffer str, File file)
			throws Exception
	{
			BufferedWriter out = new BufferedWriter(new FileWriter(file));
			String txt = str.toString();
			out.write(txt);
			out.close();
	}	
	public TraitFile getTraitFile()
		{ return traitFile; }
	public void setTraitFile(TraitFile traitFile)
		{ this.traitFile = traitFile; }
}