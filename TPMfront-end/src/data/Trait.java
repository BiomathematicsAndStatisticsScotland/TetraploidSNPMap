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

public class Trait implements Serializable
{
	static final long serialVersionUID = 9029610617375265016L;
	
	private String name;
	
	private Vector<Float> positions = new Vector<Float>();
	private Vector<Float> lods = new Vector<Float>();
	private Vector<Float> lods2;
	private Vector<String> simplemodels = new Vector<String>();
	private Vector<String> simplemodelCoefficients = new Vector<String>();
	private Vector<String> bestSimpleModels = new Vector<String>();

	private PermResult permResult;
	
	public float qtlPosition, qtlPosition2;
	public float varExplained, varExplained2;
	public float errMS, errMS2;
	public float maxLOD, maxLOD2;
/*	public float mean, mean_se;
	public float ch_e[] = new float[3];
	public float ch_se[] = new float[3];
*/	

	public String[] qtlEffects = new String[7];
	public String[] seEffects = new String[7];
	public String[] qtlEffects2;
	public float[][] modelScores = new float[10][6];
	public float[][] modelScoresExtra = new float[10][1];
	public StringBuffer simple_model = new StringBuffer();

	public String getName()
		{ return name; }
	
	public String toString()
		{ return name; }
	
	public Vector<Float> getPositions()
		{ return positions; }
	
	public Vector<Float> getLODs()
		{ return lods; }
	
	public Vector<Float> getLODs2()
		{ return lods2; }
	
	public void createLODs2()
	{ 
		lods2 = new Vector<Float>();
		qtlEffects2 = new String[2];
	}
	public void setsimple(String model_ID, String model_info)
	{
		if(simplemodels.contains(model_ID)) return;
		simplemodels.add(model_ID);
		simplemodelCoefficients.add(model_info);
	}
	public void addBestSimpleModel(String s)
	{
		//System.out.println("Trait adding simple model: " + s);
		bestSimpleModels.add(s);
	}
	public Vector<String> getBestSimpleModels()
	{
		return bestSimpleModels;
	}
	public String get_modelCoefficient(String model_ID)
	{
		//System.out.println("simple models size: " + simplemodels.size());
		//System.out.println("modelID requested: " + model_ID);
		int i = simplemodels.indexOf(model_ID);
		if(i == -1) return "";
		return simplemodelCoefficients.get(i);
	}
	public void setName(String line)
	{
		name = line;
	}
	
	public PermResult getPermResult()
		{ return permResult; }
	
	public void setPermResult(PermResult permResult)
		{ this.permResult = permResult; }
}