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
import data.LinkageGroup;
import data.OrderedResult;
import data.PhasePair;

public class CustomAnalysis extends Thread {
	// The list of markers to work with
	private LinkageGroup lGroup;
	// Object storing the phase information and final ordered list of markers
	private OrderedResult result;
	// Number of markers we are dealing with
	private int num;

	// A reworking of the PhasePair list to allow quick access to any mkr/mkr
	// combination, using their safeName indexes to determine position...ie
	// MKR005 is at element [5].
	private PhasePair[][] ppData;

	boolean isRunning = true;
	int mkrCount = 0;

	/** CustomAnalysis(). NONSNP only. 
	 * 
	 * @param result = the OrderedResult
	 * @param lGroup  = the LinkageGroup
	 */
	public CustomAnalysis(OrderedResult result, LinkageGroup lGroup) {
		this.result = result;
		this.lGroup = lGroup;

		num = lGroup.getMarkerCount();
		// createPhasePairArray();
		ppData = result.getPhasePairArray();
	}

	/** Does the custom analysis.
	 * 
	 */
	public void run() {
		int bestOrder = 0;
		float bestRSS = -1;
		Matrix bestC = null;

		CreateOrder createOrder = new CreateOrder(result, lGroup, ppData);

		for (int i = 0; i < num && isRunning; i++, mkrCount++) {
			Vector<CMarker> order = createOrder.getOrder(i);
			Matrix[] result = computeMatrices(order);

			float rSS = (float) result[1].get(0, 0);

			if (bestRSS == -1 || rSS < bestRSS) {
				bestOrder = i;
				bestRSS = rSS;
				bestC = result[0];
			}

		}

		// Takes the final ordering and sets it to the OrderedResult object
		if (isRunning) {
			// Marker order
			for (CMarker cm : createOrder.getOrder(bestOrder)) {
				result.addMarker(cm);
			}

			// Intermarker distances
			double[][] c = bestC.getArray();
			for (int r = 0; r < c.length; r++) {
				float f = 100 * (float) c[r][0];
				result.addDistance(f);
			}
		}

		isRunning = false;
	}

	// Computes the matrices C^ and rSS for the given marker order
	private Matrix[] computeMatrices(Vector<CMarker> order) {
		MatrixHandler m = new MatrixHandler(order, ppData);
		Matrix R = m.getR();
		Matrix K = m.getK();

		// K-transposed
		Matrix Kt = K.transpose();
		// R-transposed
		Matrix Rt = R.transpose();

		// TV
		Matrix KtK_i;

		// TV: exception haldling for the case: java.lang.RuntimeException:
		// Matrix is singular
		try {
			// Inverse of (K-transposed * K)
			KtK_i = Kt.times(K).inverse();
		} catch (Exception e) {
			System.out.println("CustomAnalysis.computeMatrices() [Line 115] : " + e);
			// MsgBox.msg(" Ordering Failed! \n Tried to inverse a singular
			// matrix.", MsgBox.ERR);
			KtK_i = Kt.times(K);
		}

		// K-transposed * R
		Matrix KtR = Kt.times(R);

		Matrix C = KtK_i.times(KtR);

		Matrix rSS = Rt.times(R).minus(Rt.times(K).times(KtK_i).times(KtR));

		if (MatrixHandler.print) {
			System.out.println("C:");
			C.print(5, 2);
		}

		Matrix[] result = { C, rSS };
		return result;
	}

	// Constructs a 2D array of PP values based on the fact that we know the
	// ordering of the PP list is 0:1, 0:2, 0:3, 1:2, 1:3, 2:3, etc
	// TODO: this may stop working if the passed-in PP list is not the same as
	// the marker list - that is, markers have been removed SINCE the TwoPoint
	// analysis.
	// If this is the case, then a new way of constructing this array is needed
	// - and this array IS needed, as it's too slow to search the list each time
	// a PP must be found
	/*
	 * private void createPhasePairArray() { ppData = new PhasePair[num][num];
	 * 
	 * ListIterator<PhasePair> itor = result.getPhasePairs().listIterator(0);
	 * for (int i = 0; i < num; i++) { for (int j = (i+1); j < num; j++) {
	 * ppData[i][j] = itor.next(); ppData[j][i] = ppData[i][j]; } } }
	 */

	void exit() {
		isRunning = false;
	}
}