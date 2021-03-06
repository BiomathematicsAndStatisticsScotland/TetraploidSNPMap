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

package analyses;

public abstract class FortranRunner extends Thread {
	/** Template class for all fortran analyses.
	 *  <p>
	 *  create a RunXxx class with a public void run() method
	 *  </p>
	 */
	protected Process proc = null;
	
	// Is the executable still running?
	public boolean isRunning = true;
	// Have any errors occurred?
	public boolean error = false;
	
	/** 
	 * exit() the runner. destroy the proc if it exists.
	 */
	public void exit() {
		if (proc != null) {
			proc.destroy();
		}
	}
}