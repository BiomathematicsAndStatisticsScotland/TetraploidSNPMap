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
import java.text.DecimalFormat;

// Class that stores summary information on an analysis run
public class Summary implements Serializable
{
	static final long serialVersionUID = 5663087561843697420L;
	
	// How long did the analysis take to run
	private long time;
	private boolean excludedup;
	private int numexcluded = -1;
	private int nummarkers = -1;
	
	// What was the original LinkageGroup that it run on
	private LinkageGroup oGroup;
	
	// Stores a list of markers that were selected at the time of the run
	private LinkageGroup sGroup;
		
	public Summary(LinkageGroup oGroup, long time)
	{
		this.oGroup = oGroup;
		//System.out.println("summary - oGroup = " + oGroup);
		this.time = time;
		
		this.sGroup = oGroup.getClonedLinkageGroup(true, true);
		//System.out.println("summary - sGroup = " + sGroup);
	}
	public Summary(LinkageGroup oGroup, long time, boolean excludedup, int numexcluded)
	{
		this.oGroup = oGroup;
		this.time = time;
		this.excludedup = excludedup;
		this.numexcluded = numexcluded;
		this.sGroup = oGroup.getClonedLinkageGroup(true, false);
	}
	public Summary(LinkageGroup oGroup, long time, boolean excludedup, int numexcluded, int nummarkers)
	{
		this.oGroup = oGroup;
		this.time = time;
		this.excludedup = excludedup;
		this.numexcluded = numexcluded;
		this.sGroup = oGroup.getClonedLinkageGroup(true, false);
		this.nummarkers = nummarkers;
	}
	public LinkageGroup getOriginalGroup() { return oGroup; }
	
	public LinkageGroup getSelectedGroup() { return sGroup; }
	
	public String getTimeSummary()
	{
		if (time < 1000)
			return "Analysis ran in " + time + " milliseconds";
			
		else if (time < 60000)
		{
			long secs = (long) (time / 1000f);
			return "Analysis ran in " + secs + " second"
				+ (secs == 1 ? "" : "s");
		}
		
		else
		{
			float mins = ((float)time / 1000f / 60f);
			return "Analysis ran in " + new DecimalFormat("0.0").format(mins)
				+ " minute" + (mins == 1 ? "" : "s");
		}
		
	}
	
	public String getMarkersSummary()
	{
		String s =  sGroup.getMarkerCount() + " of " + oGroup.getMarkerCount()
			+ " markers were selected at the time of the analysis. ";
		if(numexcluded != -1){
			if(excludedup) {
				s += " Exclude duplicates: Y. ";
			}
			else s += "Exclude duplicates: N. ";
		}
		return s;
	}
	public String getMarkersSummary2()
	{
		String s =  "";
		if(numexcluded != -1){
			if(excludedup) {
				s += " (" + numexcluded + " loci were excluded as duplicates).\n";
			}
		}
		if(nummarkers != -1) {
			s += " Number of markers: " + nummarkers;
		}
		return s;
	}

}