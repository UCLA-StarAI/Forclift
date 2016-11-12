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

/**
 * This class contains some settings.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Friday 5 July 2013
 */
public class Settings {

	/** Algorithm **/

	private static int MAXIMUM_NUMBER_OF_CLAUSES = 15;

	public static int getMaximumNumberOfClauses() {
		return Settings.MAXIMUM_NUMBER_OF_CLAUSES;
	}

	public static void setMaximumNumberOfClauses(int maximumNumberOfClauses) {
		Settings.MAXIMUM_NUMBER_OF_CLAUSES = maximumNumberOfClauses;
	}

	private static int SEARCH_STRATEGY = 0;

	public static int getSearchStrategy() {
		return Settings.SEARCH_STRATEGY;
	}

	public static void setSearchStrategy(int searchStrategy) {
		Settings.SEARCH_STRATEGY = searchStrategy;
	}

	private static double STEP_SIZE = 0.99;

	public static double getStepSize() {
		return Settings.STEP_SIZE;
	}

	public static void setStepSize(double stepSize) {
		Settings.STEP_SIZE = stepSize;
	}

	/** MLN syntax **/

	public static final String DISJUNCTION = " v ";

	public static final String START_ARGUMENTS = "(";

	public static final String END_ARGUMENTS = ")";

	public static final String ARGUMENT_SEPARATOR = ",";

	public static final String NEGATION = "!";

	public static final String VARIABLE_NAME = "v";

	/** Multi-threading **/

	private static int NUMBER_OF_ADDITIONAL_THREADS = 2;

	public static int getNumberOfAdditionalThreads() {
		return Settings.NUMBER_OF_ADDITIONAL_THREADS;
	}

	public static void setNumberOfAdditionalThreads(int numberOfAdditionalThreads) {
		Settings.NUMBER_OF_ADDITIONAL_THREADS = numberOfAdditionalThreads;
	}

	private static int TIMEOUT_IN_SECONDS = 910;

	public static int getTimeoutInSeconds() {
		return Settings.TIMEOUT_IN_SECONDS;
	}

	public static void setTimeoutInSeconds(int timeoutInSeconds) {
		Settings.TIMEOUT_IN_SECONDS = timeoutInSeconds;
	}

	/** Weight learning output **/

	public static final String MODEL_START_INDICATOR = "Learned model:";

	public static final String MODEL_END_INDICATOR = "End learned model";

	/** Likelihood calculation output **/

	public static final String OBJECTIVE_FUNCTION = "Final loglikelihood: ";

	public static final String PARTITION_CIRCUIT_ORDER = "Partition function has order";

	public static final String QUERY_CIRCUIT_ORDER = "Query circuit has order";

	public static final String PARTITION_CIRCUIT_SIZE = "Partition function has size";

	public static final String QUERY_CIRCUIT_SIZE = "Query circuit has size";

	/** Normalization **/

	private static boolean NORMALIZE_LIKELIHOOD = false;

	public static boolean getNormalizeLikelihood() {
		return Settings.NORMALIZE_LIKELIHOOD;
	}

	public static void setNormalizeLikelihood(boolean normalizeLikelihood) {
		Settings.NORMALIZE_LIKELIHOOD = normalizeLikelihood;
	}

	private static boolean NORMALIZE_OBJECTIVE_FUNCTION = false;

	public static boolean getNormalizeObjectiveFunction() {
		return Settings.NORMALIZE_OBJECTIVE_FUNCTION;
	}

	public static void setNormalizeObjectiveFunction(boolean normalizeObjectiveFunction) {
		Settings.NORMALIZE_OBJECTIVE_FUNCTION = normalizeObjectiveFunction;
	}

	/** Complexity penalty **/

	private static double COMPLEXITY_PENALTY = 0.01;

	public static double getComplexityPenalty() {
		return Settings.COMPLEXITY_PENALTY;
	}

	public static void setComplexityPenalty(double complexityPenalty) {
		Settings.COMPLEXITY_PENALTY = complexityPenalty;
	}

	/** Output directory **/

	private static File OUTPUT_DIRECTORY = new File(System.getProperty("java.io.tmpdir"));

	public static File getOutputDirectory() {
		return Settings.OUTPUT_DIRECTORY;
	}

	public static void setOutputDirectory(File outputDirectory) {
		if (outputDirectory == null || !outputDirectory.isDirectory()) {
			throw new IllegalArgumentException("The given output directory is invalid.");
		}
		Settings.OUTPUT_DIRECTORY = outputDirectory;
	}

}
