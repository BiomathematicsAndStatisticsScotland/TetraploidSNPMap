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
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.Vector;

import doe.MsgBox;
import gui.AppFrame;

/** Linkagegroup.
 * 
 * <p>LinkageGroup is a complex class which is used many times throughout TetraploidMap.
 * Its basic function is to provide the necessary data structures (collections) which 
 * will be used to store:
 * 	- the marker data imported from an external text file (dataset)	-> Vector<CMarker>
 * 	- the analysis results calced from those imported marker data	-> LinkedList<Object>
 *  - the TraitFile data for this group (only top-level group)		-> 3x Vector<> see TraitFile
 *  
 */
public class LinkageGroup implements Serializable {
	static final long serialVersionUID = -7093173103799457188L;

	// Stores the actual markers
	private Vector<CMarker> markers = new Vector<CMarker>();

	// Stores a set of results that have been run on this linkage group
	private LinkedList<Object> results = new LinkedList<Object>();

	// Stores TraitFile data for this group (only applies to the top-level
	// group)
	private TraitFile tFile;

	private String name;
	private int clusterCount;
	private int orderedCount;
	private int twopointCount;
	private int mdsCount;
	private int anovaCount;
	private boolean freezeSafeNames = false;

	public LinkageGroup(String name) {
		this.name = name;
	}

	/** markersNotIn(LinkageGroup othergroup).
	 * 
	 * <p>this is used only by the MDS analysis to determine which
	 * markers should be excluded. (in omitvec).
	 * 
	 * @param othergroup = the other linkagegroup.
	 * @return = the set of markers in ME that are not in other.
	 */
	public Set<String> markersNotIn(LinkageGroup othergroup) {
		Set<String> s1 = markerNames();
		Set<String> s2 = othergroup.markerNames();
		s1.removeAll(s2);
		return s1;
	}

	
	private Set<String> markerNames() {
 		Set<String> s = new HashSet<String>();

		for (CMarker cm : markers) {
			if (cm.checked) {
				s.add("" + Integer.parseInt(cm.safeName.substring(3)));
			}
		}
		return s;
	}

	public void freezeSafeNames() {
		freezeSafeNames = true;
	}
	public boolean tooFewBridging(int t) {
		int bridging = 0;
		int p1 = 0;
		int p2 = 0;
		boolean p1nn, p2nn;
		for(CMarker cm : markers) {
			if(cm.checked) {
				p1nn = !cm.marker.getParentDosage(1).contentEquals("0");
				p2nn = !cm.marker.getParentDosage(2).contentEquals("0");
				if(p1nn) p1++;
				if(p2nn) p2++;
				if(p1nn && p2nn) bridging++;
			}
		}
		if(p1 == 0 || p2 == 0) return false;
		if(bridging < t) return true;
		return false;
	}
	public boolean tooFewBridging(int t, LinkageGroup othergroup) {
		int bridging = 0;
		int p1 = 0;
		int p2 = 0;
		boolean p1nn, p2nn;
		othergroup.freezeSafeNames = true;
		for(CMarker cm : markers) {
			if(cm.checked) {
				CMarker otherM = othergroup.getMarkerBySafeName(cm.safeName);
				
				if(otherM != null && othergroup.getMarkerBySafeName(cm.safeName).checked) {
					p1nn = !cm.marker.getParentDosage(1).contentEquals("0");
					p2nn = !cm.marker.getParentDosage(2).contentEquals("0");
					if(p1nn) p1++;
					if(p2nn) p2++;
					if(p1nn && p2nn) bridging++;
				}
			}
		}
		if(p1 == 0 || p2 == 0) return false;
		if(bridging < t) return true;
		return false;
	}

	public void setTraitFile(TraitFile tFile) {
		this.tFile = tFile;
	}

	public TraitFile getTraitFile() {
		return tFile;
	}

	public String getName() {
		return name;
	}
	
	public String toString() {
		return name;
	}

	public int getMarkerCount() {
		return markers.size();
	}

	/** Adds a new marker to this linkage group.
	 * 
	 */
	public CMarker addMarker(Marker marker) {
		// Note that this CMarker's SAFE NAME will be indexed using the current
		// size of the marker list. This means we can always find a marker by
		// its safe name simply by going to that index.
		CMarker cm = new CMarker(marker, markers.size() + 1);
		markers.add(cm);

		return cm;
	}

	// Adds a Marker to the group, ensuring its "safe name" is the same as the
	// the safe name the CMarker passed in has
	public void addMarker(CMarker cm) {
		markers.add(cm.getClone());
	}

	public void sortMarkers() {
		Collections.sort(markers);
	}

	/** Returns the Marker with the given name. 
	 * If it doesn't exist, a new Marker is created to represent it.
	 */
	public Marker getOrAddMarker(String name, int alleleCount) throws CreationException {
		// 1) Does it already exist? If so, no need to do anything else
		// System.out.println("getoraddmarker1");

		for (CMarker cm : markers) {
			if (cm.marker.getName().equals(name)) {
				throw new CreationException(CreationException.NOT_UNIQUE, name);
			}
		}
		// System.out.println("getoraddmarker2");
		// 2) Otherwise, add it
		Marker m = new Marker(name, alleleCount);
		return addMarker(m).marker;
	}

	/** getMarkerByName(String name).
	 * go through all the markers and return if you find the one with given name.
	 * throw exception is the marker with given name is not found.
	 */
	public CMarker getMarkerByName(String name) throws CreationException {
		for (CMarker cm : markers) {
			if (cm.marker.getName().equals(name)) {
				return cm;
			}
		}
		throw new CreationException(CreationException.NOT_FOUND, name);
	}

	/** Returns the Marker with the given (safe) name.
	 * 
	 */
	public CMarker getMarkerBySafeName(String name) {
		if (freezeSafeNames) {
			for (CMarker cm : markers) {
				if (cm.safeName.equals(name)) {
					return cm;
				}
			}
			return null;
		} else {
			int index = Integer.parseInt(name.substring(3));
			return markers.get(index - 1);
		}

	}

	/** Returns the Marker with the given safeNameNr (int).
	 * 
	 */
	public CMarker getMarkerBySafeNameNr(int namenr) {
		if (freezeSafeNames) {
			for (CMarker cm : markers) {
				if (Integer.parseInt(cm.safeName.substring(3)) == namenr) {
					return cm;
				}
			}
			return null;
		} else {
			return markers.get(namenr - 1);
		}

	}

	public int getMarkerIndexBySafeName(String safeName) {
		int cmn = Integer.parseInt(safeName.substring(3));
		int i = 0;
		if (freezeSafeNames) {
			for (CMarker cm : markers) {
				if (Integer.parseInt(cm.safeName.substring(3)) == cmn) {
					return i;
				}
				i++;
			}
		} else {
			return cmn - 1;
		}
		System.out.println("can't find CM with safename " + safeName);
		return -1;
	}

	// Removes a marker from this group (also reindexes the group)
	public void removeMarker(CMarker toRemove) {
		markers.remove(toRemove);
		if (!freezeSafeNames) {
			int i = 1;
			for (CMarker cm : markers) {
				cm.setSafeName(i++);
			}
		}
	}

	// Returns the actual marker name for the marker with the given safe name
	public String getMarkerName(String safeName) {
		if (safeName.equals("")) return "";

		return getMarkerBySafeName(safeName).marker.getName();
	}

	// Returns the marker at the given index
	public CMarker getMarker(int index) {
		return markers.get(index);
	}

	public Vector<CMarker> getMarkers() {
		return markers;
	}

	public LinkedList<Object> getResults() {
		return results;
	}

	public void addAnovaResults(AnovaResult newAnova) {
		newAnova.setName("Anova " + (++anovaCount));
		results.add(newAnova);
	}

	public void addCluster(Cluster newCluster) {
		newCluster.setName("Cluster Analysis " + (++clusterCount));
		results.add(newCluster);
	}

	public void addOrderedResult(OrderedResult order) {
		order.setName("Ordered Analysis " + (++orderedCount));
		results.add(order);
	}

	public void addTwopointResult(OrderedResult order) {
		order.setName("Twopoint Analysis " + (++twopointCount));
		results.add(order);
	}

	public void addMDSResult(OrderedResult order) {
		order.setName("MDS Analysis " + (++mdsCount));
		results.add(order);
	}

	// Returns a count of the number of alleles in this dataset (which will be
	// more than the number of markers if SSR data is involved)
	public int getAlleleCount() {
		int count = 0;
		for (CMarker cm : markers) {
			count += cm.marker.getAlleleCount();
		}
		return count;
	}

	// Uses the first Marker in the set to determine how many individuals there
	// are
	public int getIndividualCount() {
		if (markers.size() > 0) {
			if (AppFrame.tpmmode == AppFrame.TPMMODE_NONSNP) {
				return markers.get(0).marker.getAllele(0).getStateCount();
			} else {
				return markers.get(0).marker.getAllele(0).getDosageCount();
			}
		} else {
			return 0;
		}
	}

	// Return a friendly-form String saying how many markers are in this group
	public String getMarkerCountString() {
		return markers.size() + " marker" + (markers.size() == 1 ? "" : "s");
	}

	// Returns the number of markers that are currently selected
	public int getSelectedMarkerCount() {
		int count = 0;
		for (CMarker cm : markers) {
			if (cm.checked) {
				count++;
			}
		}

		return count;
	}

	public int getFixedMarkerCount() {
		int count = 0;
		for (CMarker cm : markers) {
			if (cm.marker.fix_changed) {
				count++;
			}
		}

		return count;
	}

	// Returns a new LinkageGroup that contains the same markers as this group
	// (with only the selected markers if needbe)
	public LinkageGroup getClonedLinkageGroup(boolean selectedOnly, boolean maintainNames) {
		LinkageGroup lGroup = new LinkageGroup(name);

		for (CMarker cm : markers) {
			if (selectedOnly && cm.checked || !selectedOnly) {
				if (maintainNames) {
					lGroup.addMarker(cm);
				} else {
					lGroup.addMarker(cm.marker);
				}
			}
		}

		return lGroup;
	}

	public void verify() throws CreationException {
		if (markers.size() > 8000) {
			throw new CreationException(CreationException.TOO_MANY_LOCI);
		}

		if (getIndividualCount() > 300) {
			throw new CreationException(CreationException.TOO_MANY_INDV);
		}

		for (CMarker cm : markers) {
			cm.marker.verifyAlleles();
		}
	}

	public int[] count_npdr() {
		int[] r = { 0, 0 };
		for (CMarker cm : markers) {
			String st = cm.marker.getStatus();
			if (st.equals("NP")) {
				r[0]++;
			} else if (st.equals("DR")) {
				r[1]++;
			}
		}
		return r;
	}

	public void recalc_chisig() {
		MsgBox.msg("RECALC_CHISIG!", MsgBox.WAR);
	}

	public void fix_npdr(boolean fix_np, boolean fix_dr) {
		for (CMarker cm : markers) {
			cm.marker.fix_npdr(fix_np, fix_dr);
		}
	}

	public void fix_selection() {
		for (CMarker cm : markers) {
			if (cm.marker.fix_changed) {
				if (!cm.checked && cm.marker.canEnable(true)) {
					cm.checked = true;
				} else if (cm.checked && !cm.marker.canEnable(true)) {
					cm.checked = false;
				}
			}
		}

	}
}