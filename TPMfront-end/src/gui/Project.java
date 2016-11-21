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

package gui;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InvalidClassException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.LinkedList;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import data.AnalysisLog;
import data.LinkageGroup;
import doe.MsgBox;

public class Project implements Serializable {
	static final long serialVersionUID = 4346495433165001405L;

	// A list of all the top-level LinkageGroup objects in this project. More
	// than one group can be open at a time, hence the need to keep a list.
	private LinkedList<LinkageGroup> groups = new LinkedList<LinkageGroup>();

	// This project's name
	private String name;

	// A log of everything that's ever happened with this project
	private AnalysisLog log = new AnalysisLog();
	// A static reference to the log for ease-of-use
	public static AnalysisLog logger = null;

	// Temporary object used to track the (most recent) file this project was
	// opened from
	public static File filename;

	Project(String name, File filename) {
		this.name = name;
		Project.filename = filename;

		// Associate the static logger with this project
		logger = log;
		logger.add("Project created.");

		Project.save(this, false);
	}

	public String getName() {
		return name;
	}

	void addLinkageGroup(LinkageGroup lGroup) {
		groups.add(lGroup);
	}

	public LinkedList<LinkageGroup> getLinkageGroups() {
		return groups;
	}

	// Loads the given project from disk
	private static Project open(File filename) {
		try {
			ObjectInputStream in = new ObjectInputStream(new GZIPInputStream(new FileInputStream(filename)));
			Project proj = (Project) in.readObject();
			Project.filename = filename;

			in.close();

			logger = proj.log;
			return proj;
		} catch (InvalidClassException e1) {
			String msg = filename + " is in an old project format that " + "is no longer supported.";
			MsgBox.msg(msg, MsgBox.ERR);
		} catch (Exception e2) {
			String msg = "There was an unexpected error while loading " + filename + "\n" + e2;
			MsgBox.msg(msg, MsgBox.ERR);
		}

		return null;
	}

	/** Calls load() to load the given project from disk.
	 *  
	 * @param filename = if it is null, we'll open a FileDialog to choose a file.
	 */
	public static Project open(String filename) {
		if (filename != null) {
			return open(new File(filename));
		}

		// Create the dialog
		JFileChooser fc = new JFileChooser();
		fc.setCurrentDirectory(new File(Prefs.gui_dir));
		fc.setDialogTitle("Open Project");

		// Loop until either valid files are picked, or cancel is selected
		while (fc.showOpenDialog(MsgBox.frm) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			Prefs.gui_dir = "" + fc.getCurrentDirectory();

			if (file == null) {
				String msg = fc.getSelectedFile() + "\nFile not found.\nPlease "
						+ "verify the correct file name was given.";
				MsgBox.msg(msg, MsgBox.ERR);
			} else {
				return open(file);
			}
		}

		return null;
	}

	/** TV method that returns object size.
	 * 
	 */
	public static byte[] sizeOf(Object obj) throws java.io.IOException {
		ByteArrayOutputStream byteObject = new ByteArrayOutputStream();
		ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteObject);
		objectOutputStream.writeObject(obj);
		objectOutputStream.flush();
		objectOutputStream.close();
		byteObject.close();
		return byteObject.toByteArray();
	}

	static boolean save(Project proj, boolean saveAs) throws StackOverflowError {
		if (saveAs && proj.saveAs() == false) {
			System.out.println("failed to save as..");
			return false;
		}
		try {
			FileOutputStream fileObject = new FileOutputStream(Project.filename);
			GZIPOutputStream gzipObject = new GZIPOutputStream(fileObject);
			ObjectOutputStream out = new ObjectOutputStream(gzipObject);
			byte[] bb = sizeOf(proj);
			for (int i = 1; i < bb.length; i++) {
				bb[0] = (byte) (bb[0] + bb[i]);
			}
			out.writeObject(proj);

			out.flush();
			out.close();
			gzipObject.close();
			fileObject.close();

			return true;
		} catch (Exception e) {
			System.out.println("There was an unexpected error while saving");
			System.out.println(e);
			System.out.println(Project.filename.getName());

			String msg = "There was an unexpected error while saving " + Project.filename + "\n" + e;
			MsgBox.msg(msg, MsgBox.ERR);
		}
		System.out.println("returning false");
		return false;
	}

	private boolean saveAs() {
		JFileChooser fc = new JFileChooser();
		// fc.addChoosableFileFilter(Filters.getFileFilter(3));
		fc.setDialogTitle("Save As");
		fc.setSelectedFile(filename);

		while (fc.showSaveDialog(MsgBox.frm) == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();

			// Make sure it has an appropriate extension
			if (!file.exists()) {
				if (file.getName().indexOf(".") == -1) {
					file = new File(file.getPath() + ".proj");
				}
			}

			// Confirm overwrite
			if (file.exists()) {
				int response = MsgBox.yesnocan(file + " already exists.\nDo " + "you want to replace it?", 1);

				if (response == JOptionPane.NO_OPTION) {
					continue;
				} else if (response == JOptionPane.CANCEL_OPTION || response == JOptionPane.CLOSED_OPTION) {
					return false;
				}
			}

			// Otherwise it's ok to save...
			Prefs.gui_dir = "" + fc.getCurrentDirectory();
			filename = file;

			return true;
		}

		return false;
	}
}
