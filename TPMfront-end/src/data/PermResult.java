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
import java.util.Arrays;
import gui.Prefs;

public class PermResult implements Serializable
{
	static final long serialVersionUID = 6575547173738096739L;
	
	public float[] lodScores;	
	public int numperms;
	private float sig90, sig95;
	
	public PermResult()
	{
		lodScores = new float[Prefs.maxperms];
	}
	public PermResult(int numperms)
	{
		this.numperms = numperms;
		lodScores = new float[numperms];
	}
	public void calcSigScores(){
		float[] sortedscores = Arrays.copyOf(lodScores, numperms);
		Arrays.sort(sortedscores);
		double p = 90.0 * (numperms+1)/100.0;
		int j = (int) p;
		double remainder = p - j;
		this.sig90 = (float)((1.0 - remainder) * sortedscores[j-1] + remainder * sortedscores[j]);
		p = 95.0 * (numperms+1)/100.0;
		j = (int) p;
		remainder = p - j;
		this.sig95 = (float)((1.0 - remainder) * sortedscores[j-1] + remainder * sortedscores[j]);
		
/*		ii=85+5*i
c          pos = ii * FLOAT(ntrt+1) / 100.
c          j = int(pos)
c          remainder = pos - j
c          percentile(i) = (1.0d0 - remainder)*bestlod_sort(j) + 
c     &    remainder*bestlod_sort(j+1)
				*/
	}
	public void setSigScores(float sig90, float sig95)
	{
		this.sig90 = sig90;
		this.sig95 = sig95;
	}
	
	public float getSig90()
		{ return sig90; }
	
	public float getSig95()
		{ return sig95; }
}