/*
 * Copyright 2016 Jan Van Haaren (KU Leuven)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.ucla.cs.starai.forclift.learning.structure.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.channels.FileChannel;

import edu.ucla.cs.starai.forclift.learning.structure.StructureLearner;

/**
 * This class contains the utility methods.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Friday 5 July 2013
 */
public class Utils {

	public static void reportMessage(String message) {
		System.out.println(message);
		try {
			FileBuilder.writeMessageToFile(message, true);
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static String getCodeLocation() throws UnsupportedEncodingException {
		String environmentVariable = System.getenv("WFOMC_JAR");
		if (environmentVariable != null) {
			return environmentVariable;
		}

		return URLDecoder.decode(StructureLearner.class.getProtectionDomain().getCodeSource().getLocation().getPath(), "UTF-8");
	}

	public static String getNewLine() {
		return System.getProperty("line.separator");
	}

	public static File getTemporaryDirectory() {
		return Settings.getOutputDirectory();
	}

	public static void copyFile(File sourceFile, File destinationFile) throws IOException {
		if (!destinationFile.exists()) {
			destinationFile.createNewFile();
		}

		FileChannel source = null;
		FileChannel destination = null;

		try {
			source = new FileInputStream(sourceFile).getChannel();
			destination = new FileOutputStream(destinationFile).getChannel();
			destination.transferFrom(source, 0, source.size());
		}
		finally {
			if (source != null) {
				source.close();
			}
			if (destination != null) {
				destination.close();
			}
		}
	}

}
