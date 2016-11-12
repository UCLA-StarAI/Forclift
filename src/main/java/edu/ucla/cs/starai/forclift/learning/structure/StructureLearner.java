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

package edu.ucla.cs.starai.forclift.learning.structure;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.ucla.cs.starai.forclift.learning.structure.model.Clause;
import edu.ucla.cs.starai.forclift.learning.structure.model.ClauseBuilder;
import edu.ucla.cs.starai.forclift.learning.structure.model.Model;
import edu.ucla.cs.starai.forclift.learning.structure.model.ModelBuilder;
import edu.ucla.cs.starai.forclift.learning.structure.task.Score;
import edu.ucla.cs.starai.forclift.learning.structure.task.ScoreTask;
import edu.ucla.cs.starai.forclift.learning.structure.task.TaskExecutor;
import edu.ucla.cs.starai.forclift.learning.structure.util.FileBuilder;
import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;
import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

/**
 * This class contains the lifted structure learning algorithm.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Tuesday 2 July 2013
 */
public class StructureLearner {

	private final List<File> databases;

	private final String header;

	private final String formulas;

	public StructureLearner(final List<File> databases, final String header, final String formulas, final boolean normalizeLikelihood, final boolean normalizeObjectiveFunction, final int searchStrategy, final double complexityPenalty, final int maximumNumberOfClauses, final double stepSize, final int timeout,
			final int numberOfAdditionalThreads, final File outputDirectory) {
		this.databases = databases;
		this.header = header;
		this.formulas = formulas;
		Settings.setNormalizeLikelihood(normalizeLikelihood);
		Settings.setNormalizeObjectiveFunction(normalizeObjectiveFunction);
		Settings.setSearchStrategy(searchStrategy);
		Settings.setComplexityPenalty(complexityPenalty);
		Settings.setMaximumNumberOfClauses(maximumNumberOfClauses);
		Settings.setStepSize(stepSize);
		Settings.setTimeoutInSeconds(timeout);
		Settings.setNumberOfAdditionalThreads(numberOfAdditionalThreads);
		Settings.setOutputDirectory(outputDirectory);
	}

	private Set<Model> constructModels(Model initialModel, Set<Clause> clauses) {
		Set<Model> result = new HashSet<Model>();

		for (Clause clause : clauses) {
			Model model = new Model(initialModel);
			model.addClause(clause);
			result.add(model);
		}

		return result;
	}

	public void run() {

		// report debugging information
		Utils.reportMessage("Search strategy: " + Settings.getSearchStrategy());
		Utils.reportMessage("Normalize likelihood: " + Settings.getNormalizeLikelihood());
		Utils.reportMessage("Normalize objective function: " + Settings.getNormalizeObjectiveFunction());
		Utils.reportMessage("Complexity penalty: " + Settings.getComplexityPenalty());
		Utils.reportMessage("Maximum number of clauses: " + Settings.getMaximumNumberOfClauses());
		Utils.reportMessage("Step size: " + Settings.getStepSize());
		Utils.reportMessage("Timeout: " + Settings.getTimeoutInSeconds());
		Utils.reportMessage("Number of additional threads: " + Settings.getNumberOfAdditionalThreads());
		Utils.reportMessage("Output directory: " + Settings.getOutputDirectory());

		long startTime = System.currentTimeMillis();

		// read clauses from theory and construct empty model
		Set<Clause> clauses = ClauseBuilder.buildClausesFromTheory(this.getFormulas());
		Model model = ModelBuilder.buildEmptyModelFromTheory(this.getHeader());

		this.runScoreTask(model);

		int numberOfClauses = 0;
		int maximumNumberOfClauses = Math.min(clauses.size(), Settings.getMaximumNumberOfClauses());

		int iteration = 0;

		while (!clauses.isEmpty() && numberOfClauses < maximumNumberOfClauses) {

			// construct models
			Set<Model> models = this.constructModels(model, clauses);

			// compute scores
			this.runScoreTasks(models);

			// compute minimum and maximum circuit size and log likelihood
			int minimumCircuitSize = Integer.MAX_VALUE;
			int maximumCircuitSize = 0;
			double minimumLogLikelihood = Double.MAX_VALUE;
			double maximumLogLikelihood = -Double.MAX_VALUE;

			for (Model m : models) {
				if (!m.getScores().isEmpty()) {
					Score s = m.getScores().get(0);
					minimumCircuitSize = Math.min(minimumCircuitSize, s.getMinimumCircuitSize());
					maximumCircuitSize = Math.max(maximumCircuitSize, s.getMaximumCircuitSize());
					minimumLogLikelihood = Math.min(minimumLogLikelihood, s.getLogLikelihood());
					maximumLogLikelihood = Math.max(maximumLogLikelihood, s.getLogLikelihood());
				}
			}

			Model.setMinimumCircuitSize(minimumCircuitSize);
			Model.setMaximumCircuitSize(maximumCircuitSize);
			Model.setMinimumLogLikelihood(minimumLogLikelihood);
			Model.setMaximumLogLikelihood(maximumLogLikelihood);

			// sort models
			List<Model> modelsList = sortModels(models, this.getNumberOfDatabases());

			// retrieve best score, model, and clause
			Model bestModel = null;
			if (!modelsList.isEmpty()) {
				bestModel = modelsList.get(0);

				this.runScoreTask(bestModel);

				File iterationRanking = FileBuilder.storeRanking(modelsList, "iteration" + iteration);
				Utils.reportMessage("Iteration " + iteration + " ranking: " + iterationRanking.getAbsolutePath());
			}

			if (bestModel == null || bestModel == model) {
				break;
			}

			if (bestModel.getFinalLogLikelihood() > model.getFinalLogLikelihood()) {
				// remove best clause from set with candidate clauses
				clauses.remove(bestModel.getLastClause());

				// replace model with current best model
				model = bestModel;
			}
			else {
				break;
			}

			// write model to file
			File iterationModel = FileBuilder.storeModel(model, this.getDatabases(), "iteration" + iteration);
			Utils.reportMessage("Iteration " + iteration + " model: " + iterationModel.getAbsolutePath());

			// increase number of clauses
			numberOfClauses++;

			// increase iteration
			iteration++;
		}

		// output final model
		if (model != null) {
			Utils.reportMessage("----------");
			Utils.reportMessage(model.getFinalScoreObject().getLearnedModelAsString());
			Utils.reportMessage("----------");
			Utils.reportMessage("Final model hashcode: " + model.hashCode());
			Utils.reportMessage("Final model: " + FileBuilder.storeModel(model, this.getDatabases(), "final"));
		}

		// report runtime
		long endTime = System.currentTimeMillis();
		long runTime = endTime - startTime;
		Utils.reportMessage("Total runtime: " + runTime + " ms");

		System.exit(0);
	}

	private static List<Model> sortModels(Set<Model> models, int numberOfDatabases) {
		List<Model> modelsList = new ArrayList<Model>();
		for (Model model : models) {
			if (model.getNumberOfScores() == numberOfDatabases) {
				modelsList.add(model);
			}
		}
		Collections.sort(modelsList);
		return modelsList;
	}

	// score on ALL-BUT-ONE database and repeat
	private void runScoreTasks(Set<Model> models) {

		// create score tasks
		Set<ScoreTask> scoreTasks = new HashSet<ScoreTask>();
		for (Model model : models) {
			for (int i = 0; i < this.getNumberOfDatabases(); i++) {
				List<File> databases = new ArrayList<File>(this.getDatabases());
				databases.remove(i);
				scoreTasks.add(new ScoreTask(model, databases));
			}
		}

		// execute score tasks
		TaskExecutor.getInstance().executeScoreTasks(scoreTasks, false);
	}

	// score on ALL databases
	private void runScoreTask(Model model) {

		// create score task
		Set<ScoreTask> scoreTasks = new HashSet<ScoreTask>();
		scoreTasks.add(new ScoreTask(model, this.getDatabases()));

		// execute score task
		TaskExecutor.getInstance().executeScoreTasks(scoreTasks, true);
	}

	private List<File> getDatabases() {
		return this.databases;
	}

	private int getNumberOfDatabases() {
		return this.getDatabases().size();
	}

	private String getHeader() {
		return this.header;
	}

	private String getFormulas() {
		return this.formulas;
	}

}
