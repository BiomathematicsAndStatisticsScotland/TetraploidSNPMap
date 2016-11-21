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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;

/**
 * StreamCatcher is used to process output from the Runner.
 * it attaches to proc.getInputStream() or proc.getErrorStream()
 */
public abstract class StreamCatcher extends Thread {
	private BufferedReader reader = null;
	
	public StreamCatcher(InputStream in) {
		reader = new BufferedReader(new InputStreamReader(in));
		start();
	}
	
	/** run() calls processLine(line) for any line read.
	 */
	public void run() {
		try {
			String line = reader.readLine();
			while (line != null) {
				processLine(line);
				line = reader.readLine();					
			}
		} catch (Exception e) {
			System.out.println(e);
		}
		
		try { 
			reader.close();
		} catch (IOException e) {
			System.out.println(e);
		}
	}
	
	protected abstract void processLine(String line);
}