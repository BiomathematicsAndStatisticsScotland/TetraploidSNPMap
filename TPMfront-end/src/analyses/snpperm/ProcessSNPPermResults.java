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

package analyses.snpperm;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.StringTokenizer;
import data.PermResult;
import doe.MsgBox;

class ProcessSNPPermResults extends Thread {
	private PermResult permResult;
	private File file;
	
	ProcessSNPPermResults(PermResult permResult, File file) {
		this.permResult = permResult;
		this.file = file;
	}
	
	public boolean process() {
		try {
			BufferedReader in = new BufferedReader(new FileReader(file));
			
			String str = in.readLine();
			// Skip first line
			str = in.readLine();
			
			// Read the LOD scores
			int i = 0;
			while (str != null && str.startsWith("  ****") == false) {
				permResult.lodScores[i++] = Float.parseFloat(str);
				str = in.readLine();
			}
			
			// Read the 90% score
			StringTokenizer st = new StringTokenizer(in.readLine());
			st.nextToken();
			float sig90 = Float.parseFloat(st.nextToken());
			
			// Read the 95% score
			st = new StringTokenizer(in.readLine());
			st.nextToken();
			float sig95 = Float.parseFloat(st.nextToken());
			
			permResult.setSigScores(sig90, sig95);
						
			in.close();
			
			return true;
		} catch (Exception e) {
			MsgBox.msg("Perm was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
			
			e.printStackTrace(System.out);
			
			return false;
		}
	}
}
