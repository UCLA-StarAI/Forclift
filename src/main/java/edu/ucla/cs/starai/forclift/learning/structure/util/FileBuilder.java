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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Set;

import edu.ucla.cs.starai.forclift.learning.structure.model.Clause;
import edu.ucla.cs.starai.forclift.learning.structure.model.Model;

/**
 * This class can be used to write objects to file.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Wednesday 3 July 2013
 */
public class FileBuilder {

	public static File writeModelToFile(final Model model, final List<File> databases) throws IOException {

		// create model file
		File modelFile = FileBuilder.createModelFile(model, databases);

		// create writer
		BufferedWriter writer = new BufferedWriter(new FileWriter(modelFile));

		// typings
		Set<String> typings = model.getTypings();
		for (String typing : typings) {
			writer.write(typing);
			writer.write(Utils.getNewLine());
		}

		// blank line
		writer.write(Utils.getNewLine());

		// clauses
		Set<Clause> clauses = model.getClauses();
		for (Clause clause : clauses) {
			writer.write(clause.toString());
			writer.write(Utils.getNewLine());
		}

		// close writer
		writer.close();

		return modelFile;
	}

	public static void writeMessageToFile(final String message, boolean append) throws IOException {

		// create message file
		File messageFile = FileBuilder.createMessageFile();

		// create writer
		BufferedWriter writer = new BufferedWriter(new FileWriter(messageFile, append));

		// write message
		writer.write(message + "\n");

		// close writer
		writer.close();
	}

	public static void writeInputStreamToFile(final InputStream inputStream, final File file) throws IOException {

		// write input stream to file
		BufferedReader inputStreamReader = new BufferedReader(new InputStreamReader(inputStream));
		BufferedWriter inputStreamWriter = new BufferedWriter(new FileWriter(file));

		String line;

		while ((line = inputStreamReader.readLine()) != null) {
			inputStreamWriter.write(line);
			inputStreamWriter.write(Utils.getNewLine());
		}

		// close reader and writer
		inputStreamReader.close();
		inputStreamWriter.close();
	}

	public static boolean doesModelLogFileExist(final Model model, final List<File> databases) {
		return createModelLogFile(model, databases).exists();
	}

	public static boolean doesLearnedModelFileExist(final Model model, final List<File> databases) {
		return createLearnedModelFile(model, databases).exists();
	}

	public static File createModelLogFile(final Model model, final List<File> databases) {
		return new File(Utils.getTemporaryDirectory(), "structure-" + model.hashCode() + "-" + databases.hashCode() + ".log");
	}

	public static File createModelErrorFile(final Model model, final List<File> databases) {
		return new File(Utils.getTemporaryDirectory(), "structure-" + model.hashCode() + "-" + databases.hashCode() + ".err");
	}

	private static File createModelFile(final Model model, final List<File> databases) {
		return new File(Utils.getTemporaryDirectory(), "structure-" + model.hashCode() + "-" + databases.hashCode() + ".mln");
	}

	public static File createLearnedModelFile(final Model model, final List<File> databases) {
		return new File(Utils.getTemporaryDirectory(), "structure-" + model.hashCode() + "-" + databases.hashCode() + "_learned.mln");
	}

	public static File createMessageFile() {
		return new File(Utils.getTemporaryDirectory(), "output.txt");
	}

	public static File createStoredModelFile(String suffix) {
		return new File(Utils.getTemporaryDirectory(), "model-" + suffix + ".mln");
	}

	public static File createStoredRankingFile(String suffix) {
		return new File(Utils.getTemporaryDirectory(), "ranking-" + suffix + ".txt");
	}

	public static File storeModel(final Model model, final List<File> databases, String suffix) {
		File sourceFile = createLearnedModelFile(model, databases);
		File destinationFile = createStoredModelFile(suffix);

		try {
			Utils.copyFile(sourceFile, destinationFile);
		}
		catch (IOException e) {
			e.printStackTrace();
		}

		return destinationFile;
	}

	public static File storeRanking(final List<Model> models, String suffix) {
		File file = createStoredRankingFile(suffix);

		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			int position = 0;
			for (Model model : models) {
				writer.write(position + "\t" + model.getScoreBig() + "\t" + model.getClauses() + "\t" + model.hashCode() + "\n");
				position++;
			}
			writer.close();
		}
		catch (IOException e) {
			e.printStackTrace();
		}

		return file;
	}

}
