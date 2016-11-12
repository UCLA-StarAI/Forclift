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

package edu.ucla.cs.starai.forclift.learning.structure.task;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Callable;

import edu.ucla.cs.starai.forclift.learning.structure.model.Model;
import edu.ucla.cs.starai.forclift.learning.structure.util.FileBuilder;
import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;
import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

/**
 * This class represents a data likelihood calculation task.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Wednesday 3 July 2013
 */
public class ScoreTask extends Task implements Callable<Score> {

	private final Model model;

	public ScoreTask(Model model, List<File> databases) {
		super(databases, TaskCollection.SCORE);
		this.model = model;
	}

	@Override
	public Score call() throws Exception {
		this.runScoreTask();
		return this.readScoreFromFile();
	}

	private Score readScoreFromFile() throws Exception {

		// create score
		Score score = new Score(this.getModel());

		// create output files
		File modelLogFile = FileBuilder.createModelLogFile(this.getModel(), this.getDatabases());

		// read the process output
		BufferedReader reader = new BufferedReader(new FileReader(modelLogFile));

		String line = null;
		boolean lineBelongsToModel = false;

		while ((line = reader.readLine()) != null) {

			// extract learned models
			if (line.trim().equals(Settings.MODEL_END_INDICATOR)) {
				lineBelongsToModel = false;
			}

			if (lineBelongsToModel) {
				score.addLineToLearnedModel(line);
			}

			if (line.trim().equals(Settings.MODEL_START_INDICATOR)) {
				lineBelongsToModel = true;
			}

			// extract statistics
			if (line.startsWith(Settings.PARTITION_CIRCUIT_ORDER)) {
				int partitionCircuitOrder = Integer.valueOf(line.replaceAll("\\D+", "").trim());
				score.setPartitionCircuitOrder(partitionCircuitOrder);
			}

			if (line.startsWith(Settings.PARTITION_CIRCUIT_SIZE)) {
				int partitionCircuitSize = Integer.valueOf(line.replaceAll("\\D+", "").trim());
				score.setPartitionCircuitSize(partitionCircuitSize);
			}

			if (line.startsWith(Settings.QUERY_CIRCUIT_ORDER)) {
				int queryCircuitOrder = Integer.valueOf(line.replaceAll("\\D+", "").trim());
				score.addQueryCircuitOrder(queryCircuitOrder);
			}

			if (line.startsWith(Settings.QUERY_CIRCUIT_SIZE)) {
				int queryCircuitSize = Integer.valueOf(line.replaceAll("\\D+", "").trim());
				score.addQueryCircuitSize(queryCircuitSize);
			}

			if (line.startsWith(Settings.OBJECTIVE_FUNCTION)) {
				String[] fields = line.split(" ");
				double logLikelihood = -Double.valueOf(fields[2]);
				score.setLogLikelihood(logLikelihood);
				score.setCompilable(true);
			}

		}

		// flush and close the reader and writer
		reader.close();

		return score;
	}

	private void runScoreTask() throws Exception {

		// create output files
		File modelFile = FileBuilder.writeModelToFile(this.getModel(), this.getDatabases());
		File modelLogFile = FileBuilder.createModelLogFile(this.getModel(), this.getDatabases());
		File modelErrorFile = FileBuilder.createModelErrorFile(this.getModel(), this.getDatabases());

		// execute the command
		Runtime runtime = Runtime.getRuntime();
		Process process = runtime.exec(this.generateCommand(modelFile));

		// schedule task killer
		Timer timer = new Timer();
		TimerTask timerTask = new TaskMonitor(process);
		timer.schedule(timerTask, 1000 * Settings.getTimeoutInSeconds());

		// read input streams
		InputStream inputStream = process.getInputStream();
		InputStream errorStream = process.getErrorStream();

		// write input streams to file
		FileBuilder.writeInputStreamToFile(inputStream, modelLogFile);
		FileBuilder.writeInputStreamToFile(errorStream, modelErrorFile);

		// close the streams
		inputStream.close();
		errorStream.close();
		process.getOutputStream().close();

		// retrieve exit value
		process.waitFor();

		// cancel task killer
		timerTask.cancel();
	}

	@Override
	protected String generateCommand(File modelFile) throws IOException {

		StringBuilder commandString = new StringBuilder();
		commandString.append("java -jar");
		commandString.append(" " + Utils.getCodeLocation());
		commandString.append(" " + this.getTask().getCommand());
		commandString.append(" -v");

		if (Settings.getNormalizeLikelihood()) {
			commandString.append(" --normalizell");
		}

		for (File file : this.getDatabases()) {
			commandString.append(" --train");
			commandString.append(" " + file.getAbsolutePath());
		}

		commandString.append(" " + modelFile.getAbsolutePath());

		return commandString.toString();
	}

	public Model getModel() {
		return this.model;
	}

}
