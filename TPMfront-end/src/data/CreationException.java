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

import gui.Prefs;

/** CreationException. 
 * 
 * 
 *
 */
public class CreationException extends Exception {
	private static final long serialVersionUID = -3075473798817502475L;
	public static int NAME_TOO_LONG = 1;
	public static int TOO_MANY_LOCI = 2;
	public static int TOO_MANY_INDV = 3;
	public static int UNKNOWN_FILE = 4;
	public static int NO_PAL_TREE = 5;
	public static int NOT_UNIQUE = 6;
	public static int NO_ALLELES = 7;
	public static int TOO_MANY_ALLELES = 8;
	public static int NOT_FOUND = 9;
	public static int TRAITNAME_TOO_LONG = 10;

	private int exception;
	private String additional;

	public CreationException(int exception) {
		super();
		this.exception = exception;
	}

	public CreationException(int exception, String additional) {
		this(exception);
		this.additional = additional;
	}

	public int getException() {
		return exception;
	}

	/** toString().
	 * translates from exception (int) to String description of exception.
	 * 
	 */
	public String toString() {
		if (exception == NAME_TOO_LONG) {
			return "At least one or more of the markers within the dataset had " + "a name longer than "
					+ Prefs.markername_maxlen + " characters.";

		} else if (exception == TRAITNAME_TOO_LONG) {
			return "At least one or more of the traits within the trait file had " + "a name longer than "
					+ Prefs.traitname_maxlen + " characters.";

		} else if (exception == TOO_MANY_LOCI) {
			return "The dataset contains more than 800 loci which is more than "
					+ "can be handled by TetraploidMap at present.";

		} else if (exception == TOO_MANY_INDV) {
			return "The dataset contains loci with details on more than 300 "
					+ "offspring which is more than can be handled by " + "TetraploidMap at present.";

		} else if (exception == UNKNOWN_FILE) {
			return "The file is of a type unreadable by TetraploidMap.";

		} else if (exception == NOT_UNIQUE) {
			return "The file contains one or more loci with the same name (" + additional
					+ " was found at least twice).";

		} else if (exception == NO_ALLELES) {
			return "The file contains one or more loci with no allele data.";

		} else if (exception == TOO_MANY_ALLELES) {
			return "The file contains one or more loci with more than 8 alleles.";

		} else if (exception == NO_PAL_TREE) {
			return "A required dendrogram could not be created.";

		} else if (exception == NOT_FOUND) {
			return "The requested CMarker " + this.additional + " could not be found in the LinkageGroup.";

		} else {
			return "CreationException: code=" + exception;
		}
	}
}