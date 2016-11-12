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

package edu.ucla.cs.starai.forclift.learning.structure.model;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.ucla.cs.starai.forclift.learning.structure.task.Score;
import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;
import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

/**
 * This class represents a model.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Tuesday 2 July 2013
 */
public class Model implements Comparable<Model> {

	private final Set<String> typings;

	private final Set<Clause> clauses;

	private Clause lastClause;

	private final List<Score> scores;

	private Score finalScore;

	private static int MINIMUM_CIRCUIT_SIZE = 0;

	private static int MAXIMUM_CIRCUIT_SIZE = Integer.MAX_VALUE;

	private static double MINIMUM_LOGLIKELIHOOD = -Double.MAX_VALUE;

	private static double MAXIMUM_LOGLIKELIHOOD = Double.MAX_VALUE;

	public Model() {
		this.typings = new HashSet<String>();
		this.clauses = new HashSet<Clause>();
		this.lastClause = null;
		this.scores = new ArrayList<Score>();
		this.finalScore = null;
	}

	public Model(final Model model) {
		this(model.getTypings(), model.getClauses());
	}

	private Model(final Set<String> typings, final Set<Clause> clauses) {
		this.typings = new HashSet<String>(typings);
		this.clauses = new HashSet<Clause>(clauses);
		this.lastClause = null;
		this.scores = new ArrayList<Score>();
	}

	public void addTyping(final String typing) {
		if (typing == null) {
			throw new IllegalArgumentException("The given typing is null.");
		}
		this.typings.add(typing);
	}

	public void addClause(final Clause clause) {
		if (clause == null) {
			throw new IllegalArgumentException("The given clause is null.");
		}

		this.clauses.add(clause);
		this.lastClause = clause;
	}

	public Set<String> getTypings() {
		return this.typings;
	}

	public Set<Clause> getClauses() {
		return Collections.unmodifiableSet(this.clauses);
	}

	public Clause getLastClause() {
		return this.lastClause;
	}

	public double getLogLikelihood() {
		if (this.getScores().isEmpty()) {
			return -Double.MAX_VALUE;
		}

		double result = 0.00;
		for (Score score : this.getScores()) {
			result += score.getLogLikelihood();
		}
		return result;
	}

	public double getFinalLogLikelihood() {
		if (this.finalScore == null) {
			return -Double.MAX_VALUE;
		}
		return this.finalScore.getLogLikelihood();
	}

	public BigDecimal getScoreBig() {
		if (this.getScores().isEmpty()) {
			return new BigDecimal(-Double.MAX_VALUE);
		}

		BigDecimal result = new BigDecimal(0.00);
		for (Score score : this.getScores()) {
			BigDecimal value = new BigDecimal(0.00);
			BigDecimal likelihood = new BigDecimal(score.getLogLikelihood());
			BigDecimal circuitSize = new BigDecimal(score.getMaximumCircuitSize());
			if (Settings.getNormalizeObjectiveFunction()) {
				BigDecimal normalizedLogLikelihood = normalize(new BigDecimal(MINIMUM_LOGLIKELIHOOD), new BigDecimal(MAXIMUM_LOGLIKELIHOOD), likelihood);
				BigDecimal normalizedCircuitSize = normalize(new BigDecimal(MINIMUM_CIRCUIT_SIZE), new BigDecimal(MAXIMUM_CIRCUIT_SIZE), circuitSize);
				value = normalizedLogLikelihood.subtract(new BigDecimal(Settings.getComplexityPenalty()).multiply(normalizedCircuitSize));
			}
			else {
				value = likelihood.subtract(new BigDecimal(Settings.getComplexityPenalty()).multiply(circuitSize));
			}
			result = result.add(value);
		}
		return result.divide(new BigDecimal(this.getScores().size()));
	}

	public double getScore() {
		if (this.getScores().isEmpty()) {
			return -Double.MAX_VALUE;
		}

		double result = 0.00;
		for (Score score : this.getScores()) {
			double value = 0;
			if (Settings.getNormalizeObjectiveFunction()) {
				double normalizedLogLikelihood = normalize(MINIMUM_LOGLIKELIHOOD, MAXIMUM_LOGLIKELIHOOD, score.getLogLikelihood());
				double normalizedCircuitSize = normalize(MINIMUM_CIRCUIT_SIZE, MAXIMUM_CIRCUIT_SIZE, score.getMaximumCircuitSize());
				value = normalizedLogLikelihood - Settings.getComplexityPenalty() * normalizedCircuitSize;
			}
			else {
				value = score.getLogLikelihood() - Settings.getComplexityPenalty() * score.getMaximumCircuitSize();
			}
			result += value;
		}
		return result / this.getScores().size();
	}

	public Score getFinalScoreObject() {
		return this.finalScore;
	}

	public List<Score> getScores() {
		return this.scores;
	}

	public int getNumberOfScores() {
		return this.getScores().size();
	}

	public void addScore(Score score) {
		this.getScores().add(score);
	}

	public void addFinalScore(Score score) {
		this.finalScore = score;
	}

	@Override
	public boolean equals(Object object) {
		if (!Model.class.isAssignableFrom(object.getClass())) {
			return false;
		}

		Model otherModel = (Model) object;

		if (this.hashCode() == otherModel.hashCode()) {
			return true;
		}
		else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		int result = 1;

		for (Clause clause : this.getClauses()) {
			result *= 31;
			result += clause.hashCode();
		}

		return result;
	}

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();

		// typings
		for (String typing : this.getTypings()) {
			stringBuilder.append(typing);
			stringBuilder.append(Utils.getNewLine());
		}

		// blank line
		stringBuilder.append(Utils.getNewLine());

		// clauses
		for (Clause clause : this.getClauses()) {
			stringBuilder.append(clause.toString());
			stringBuilder.append(Utils.getNewLine());
		}

		return stringBuilder.toString();
	}

	@Override
	public int compareTo(Model otherModel) {
		if (this.getScore() > otherModel.getScore()) {
			return -1;
		}
		else if (this.getScore() < otherModel.getScore()) {
			return 1;
		}
		else {
			return 0;
		}
	}

	private static double normalize(double minimum, double maximum, double value) {
		return (value - minimum) / (maximum - minimum);
	}

	private static BigDecimal normalize(BigDecimal minimum, BigDecimal maximum, BigDecimal value) {
		BigDecimal numerator = value.subtract(minimum);
		BigDecimal denominator = maximum.subtract(minimum);
		return numerator.divide(denominator, 5, RoundingMode.HALF_UP);
	}

	public static int getMinimumCircuitSize() {
		return MINIMUM_CIRCUIT_SIZE;
	}

	public static void setMinimumCircuitSize(int minimumCircuitSize) {
		MINIMUM_CIRCUIT_SIZE = minimumCircuitSize;
	}

	public static int getMaximumCircuitSize() {
		return MAXIMUM_CIRCUIT_SIZE;
	}

	public static void setMaximumCircuitSize(int maximumCircuitSize) {
		MAXIMUM_CIRCUIT_SIZE = maximumCircuitSize;
	}

	public static double getMinimumLogLikelihood() {
		return MINIMUM_LOGLIKELIHOOD;
	}

	public static void setMinimumLogLikelihood(double minimumLogLikelihood) {
		MINIMUM_LOGLIKELIHOOD = minimumLogLikelihood;
	}

	public static double getMaximumLogLikelihood() {
		return MAXIMUM_LOGLIKELIHOOD;
	}

	public static void setMaximumLogLikelihood(double maximumLogLikelihood) {
		MAXIMUM_LOGLIKELIHOOD = maximumLogLikelihood;
	}

}
