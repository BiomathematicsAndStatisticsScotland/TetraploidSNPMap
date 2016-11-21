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
import java.util.LinkedList;
import pal.statistics.ChiSquareDistribution;

public class MarkerProbability implements Serializable
{
	static final long serialVersionUID = 1340095781563457778L;
	
	// Parental genotypes
	String p1, p2;
	// Probability
	float pb;
	// Degrees of freedom
	float df;
	// Chi square
	float chi;
	// Chi square significance
	double sig;
	
	public MarkerProbability(String p1, String p2, float pb, float df, float chi)
	{
		this.p1  = p1;
		this.p2  = p2;
		this.pb  = pb;
		this.df  = df;
		this.chi = chi;
		
		sig = 1 - (ChiSquareDistribution.cdf(chi, df));
	}
	
	// Insertion sort - adds elements to the list so that MarkerProbabilities
	// with the highest significance level are at the start of the list
	static void insert(LinkedList<MarkerProbability> list, MarkerProbability mp)
	{
		int i = 0;
		for (MarkerProbability eMP: list)
		{
			boolean add = false;
			
			// How to score higher?
			//  Order on significance. If significances are equal, then order on
			//  DF, if DF equal, then order on chi^2
			if (mp.pb > eMP.pb)
				add = true;
			else if (mp.pb == eMP.pb)
			{
				if (mp.sig < eMP.sig)
					add = true;
			}
			
			if (add)
			{
				list.add(i, mp);
				return;
			}
			else
				i++;
		}
		
		// If it didn't score better than the rest, it gets added at the end
		list.add(mp);
	}
}
