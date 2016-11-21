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

package analyses.altqtl;

import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.Trait;
import doe.MsgBox;

class ProcessAltQtlResults extends Thread {
	private Trait trait;
	private File file;
	
	ProcessAltQtlResults(Trait trait, File file) {
		this.trait = trait;
		this.file = file;
	}
	
	public boolean process() {
		try {
			trait.createLODs2();
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			String str = in.readLine();
			// Read (and ignore) header information
			for (int i = 0; i < 4; i++) {
				str = in.readLine();	
			}
			
			while (str.startsWith(" Best position with iteration") == false) {
				StringTokenizer st = new StringTokenizer(str);			
				st.nextToken();
				st.nextToken();
				float lod = Float.parseFloat(st.nextToken());
				
				trait.getLODs2().add(lod);
				
				str = in.readLine();
			}
			
			str = in.readLine();
			
			// Position, variance, error mean square
			StringTokenizer st = new StringTokenizer(str);
			trait.qtlPosition2 = Float.parseFloat(st.nextToken());
			trait.varExplained2 = Float.parseFloat(st.nextToken());
			trait.errMS2 = Float.parseFloat(st.nextToken());
			trait.maxLOD2 = Float.parseFloat(st.nextToken());
			
			// Best position with iteration data (2 lines)
			for (int i = 0; i < 2; i++) {
				trait.qtlEffects2[i] = in.readLine();
			}
						
			in.close();
			
			return true;
		} catch (Exception e) {
			MsgBox.msg("AltQTLModel was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
			
			e.printStackTrace(System.out);
			
			return false;
		}
	}
}
