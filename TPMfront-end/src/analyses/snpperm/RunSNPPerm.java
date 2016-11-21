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

import java.io.File;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.OutputStreamWriter;
import java.io.BufferedReader;
import java.util.Vector;
import java.util.Random;
import java.util.StringTokenizer;
import java.lang.Process;
import analyses.FortranRunner;
import analyses.StreamCatcher;
import gui.Prefs;
import doe.MsgBox;

class RunSNPPerm extends FortranRunner {
	Vector<Vector<Float>> results = new Vector<Vector<Float>>();
	Vector<Integer> positions = new Vector<Integer>();
	int activeprocs = 0;
	int stage = 1;
	int numperms, numpermseach;
	private Vector<Integer> seeds = new Vector<Integer>();
	int cores;
	private Vector<Process> procs = new Vector<Process>();
	
	public  RunSNPPerm(int numperms, int seed) {
		int i;
		Random g = new Random(seed);
		this.cores = Runtime.getRuntime().availableProcessors();
		for (i = 0; i < this.cores; i++) {
			this.seeds.add(g.nextInt());
			results.add(new Vector<Float>());
			positions.add(0);
 		} 
		this.numpermseach = numperms / cores;
		this.numperms = cores * this.numpermseach;
	}

	public void run() {
		int i;
		try {
			for (i = 0; i < this.cores; i++) {
				proc = Runtime.getRuntime().exec(Prefs.tools_snpqtlperm_path, null,
						new File(Prefs.tools_scratch));
				activeprocs++;
				procs.add(proc);
				new PermCatcher(proc.getInputStream(), i);
				new PermCatcher(proc.getErrorStream(), i);
				PrintWriter writer = new PrintWriter(new OutputStreamWriter(
						proc.getOutputStream()));
				writer.println("perm");
				writer.println("perm");
				writer.println("1");
				writer.println(numpermseach);
				writer.println(this.seeds.elementAt(i));
				writer.close();
			}
			for (i = 0; i < this.cores;i++) procs.elementAt(i).waitFor();
		} catch (Exception e) {
			error = true;
			MsgBox.msg("Perm was unable to run due to the following error:"
				+ "\n" + e, MsgBox.ERR);
		}
		
		isRunning = false;
	}

	public int totalposition() {
		int i, tp = 0;
		for (i = 0; i < this.cores; i++) {
			tp += positions.elementAt(i);
		}
		return tp;
	}
	
	class PermCatcher extends StreamCatcher {
		BufferedReader reader = null;
		int myindex, mp;

		PermCatcher(InputStream in, int i) {
			super(in);
			myindex = i;
		}
		
		protected void processLine(String line) {
			if (line.startsWith("forrtl: severe")) {
				error = true;
				MsgBox.msg("Critical error:\n" + line, MsgBox.ERR);						
			} else if (line.startsWith(" permutation")) {
				stage  = 1;
				StringTokenizer st = new StringTokenizer(line);
				st.nextToken();
				mp = Integer.parseInt(st.nextToken());
				positions.set(myindex, mp);
				if (mp == numpermseach) activeprocs--;
			} else if (line.startsWith(" lod")) {
				stage = 2;
			} else if (stage == 2) {
				results.elementAt(myindex).add(Float.parseFloat(line));
			}
		}
	}
}