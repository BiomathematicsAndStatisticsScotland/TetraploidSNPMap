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

/** Allele. SNP and NONSNP.
 * 
 * <p>Allele class is used to store all allele information for a Marker.
 * Untill now there was only one type of information presence/absence
 * of the allele, which was stored in a Vector&lt;AlleleState&gt;
 * 
 * <p>Now for the new SNP dosage Marker data, an additional information has
 * been added which is the AllelleDosage stored in a Vector&lt;AlleleDosage&gt;.
 * This Vector stores a totaly different type of Allelle information and 
 * should not be confused with the AlleleState type of information.
 * 
 */
public class Allele implements Serializable {
	static final long serialVersionUID = -5925065724311527021L;

	private Vector<AlleleState> states = new Vector<AlleleState>();
	private Vector<AlleleDosage> dosages = new Vector<AlleleDosage>();

	public void addAlleleState(AlleleState state) {
		states.add(state);
	}

	public void addAlleleDosage(AlleleDosage dosage) {
		dosages.add(dosage);
	}

	public Vector<AlleleState> getStates() {
		return states;
	}

	public Vector<AlleleDosage> getDosages() {
		return dosages;
	}

	public int getStateCount() {
		return states.size();
	}

	public int getDosageCount() {
		return dosages.size();
	}

}