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

/** AlleleDosage. SNP only.
 * 
 * <p>AlleleDosage adds an extra information regarding the Allele information
 * existing in the imported Marker data. More specifically this class is used
 * to represent information regarding the dosage of alleles rather than presence/absence.
 * 
 * <p>This class is used only for SNP Marker datasets to replace the AlleleState
 * class which only supports presence or absence of alleles.
 * 
 */
public class AlleleDosage implements Serializable {
	private static final long serialVersionUID = -4286790728351771603L;

	public static final byte AAAA = 0;
	public static final byte AAAB = 1;
	public static final byte AABB = 2;
	public static final byte ABBB = 3;
	public static final byte BBBB = 4;
	public static final byte UNKNOWN = 9;

	private byte dosage;

	/** AlleleDosage. SNP only.
	 * 
	 * @param dosage = byte, 0, 1, 2, 3, 4, 9
	 */
	public AlleleDosage(byte dosage) {
		this.dosage = dosage;
	}

	public void setDosage(byte dosage) {
		this.dosage = dosage;
	}

	public byte getDosage() {
		return dosage;
	}

}
