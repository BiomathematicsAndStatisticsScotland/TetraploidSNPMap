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

package analyses.order.custom;

import java.util.Vector;
import Jama.Matrix;
import data.CMarker;
import data.PhasePair;

// Creates the R and K matrices

// R is a n by 1 matrix where each element Rij is set to the value of Haldane's
// mapping function using the value of recombination fraction for ij

// K is a n-1 by n-1 matrix where n is the number of markers
// Each row's element is initially set to 0 or 1 based on whether Kij at that
// point forms part of a distance calculation, eg for 4 markers:
//  1,2: 1 0 0
//  1,3: 1 1 0
//  etc
//  3,4: 0 0 1
//
// Each element (0 or 1) is then multiplied by the sqrt of the lod score between
// markers i and j

class MatrixHandler {
	static boolean print = false;

	private Matrix R;
	private Matrix K;
	private Matrix KO;

	MatrixHandler(Vector<CMarker> order, PhasePair[][] ppData) {
		createMatrices(order, ppData);
	}

	Matrix getR() {
		return R;
	}

	Matrix getK() {
		return K;
	}

	private void createMatrices(Vector<CMarker> order, PhasePair[][] ppData) {
		// Number of markers
		int num = order.size();

		// Number of rows: (n * n-1) / 2
		// eg: for 4 markers: 6 rows
		int rows = (num * (num - 1)) / 2;

		// Create the matrices
		R = new Matrix(rows, 1);
		K = new Matrix(rows, order.size() - 1);
		// But create references to their actual values
		double[][] rArray = R.getArray();
		double[][] kArray = K.getArray();
		KO = new Matrix(rows, order.size() - 1);
		double[][] kOrig = KO.getArray();

		if (print) {
			System.out.println("\n     R   \tLod  \th (Haldane's) = -0.5 * log(1 - 2*R)\n");
		}

		for (int i = 0, r = 0; i < num - 1; i++) {
			for (int j = (i + 1); j <= num - 1; j++, r++) {
				// 1) Set each element for the current row in R

				// For each Rij we want to get the actual marker indices so that
				// the correct R and L values can be found in the array
				int iP = order.get(i).getSafeNameSuffix() - 1;
				int jP = order.get(j).getSafeNameSuffix() - 1;

				float Rij = ppData[iP][jP].rfq;
				float Lij = ppData[iP][jP].lod;

				if (Rij >= 0.5) {
					Rij = 0.49999f;
				}

				// Haldane's mapping function
				double h = -0.5 * (Math.log(1 - (2 * Rij)));

				// Set element i,0 in the R matrix to be h * sqrt(Lij)
				rArray[r][0] = h * Math.sqrt(Lij);

				if (print) {
					System.out.println("R" + i + "," + j + " " + Rij + "\t" + Lij + "\t" + h);
				}

				// 2) Set each of the elements in the current row in K
				for (int c = 0; c < num - 1; c++) {
					if (c >= i && c < j) {
						kArray[r][c] = Math.sqrt(Lij);
						kOrig[r][c] = 1;
					}
					// else = 0
				}
			}
		}

		if (print) {
			print();
		}
	}

	private void print() {
		System.out.println("\nR: = h * sqrt(Lod)");
		R.print(2, 2);
		System.out.println("K original:");
		KO.print(5, 2);
		System.out.println("K adjusted: = sqrt(Lod) (if K original=1)");
		K.print(5, 2);
	}
}