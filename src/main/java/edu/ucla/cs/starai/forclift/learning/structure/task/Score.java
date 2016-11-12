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

import java.util.ArrayList;
import java.util.List;

import edu.ucla.cs.starai.forclift.learning.structure.model.Model;
import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

public class Score implements Comparable<Score> {

	private final Model model;

	private final List<String> learnedModel;

	private double logLikelihood;

	private int partitionCircuitOrder;

	private final List<Integer> queryCircuitOrders;

	private int partitionCircuitSize;

	private final List<Integer> queryCircuitSizes;

	private boolean compilable;

	public Score(Model model) {
		this.model = model;
		this.learnedModel = new ArrayList<String>();
		this.logLikelihood = 0;
		this.partitionCircuitOrder = Integer.MAX_VALUE;
		this.queryCircuitOrders = new ArrayList<Integer>();
		this.partitionCircuitSize = Integer.MAX_VALUE;
		this.queryCircuitSizes = new ArrayList<Integer>();
		this.compilable = false;
	}

	public Model getModel() {
		return this.model;
	}

	public void addLineToLearnedModel(String output) {
		this.getLearnedModel().add(output);
	}

	public double getLogLikelihood() {
		return this.logLikelihood;
	}

	public void setLogLikelihood(final double score) {
		this.logLikelihood = score;
	}

	public int getPartitionCircuitOrder() {
		return this.partitionCircuitOrder;
	}

	public void setPartitionCircuitOrder(int partitionCircuitOrder) {
		this.partitionCircuitOrder = partitionCircuitOrder;
	}

	public int getPartitionCircuitSize() {
		return this.partitionCircuitSize;
	}

	public void setPartitionCircuitSize(int partitionCircuitSize) {
		this.partitionCircuitSize = partitionCircuitSize;
	}

	public List<Integer> getQueryCircuitOrders() {
		return this.queryCircuitOrders;
	}

	public void addQueryCircuitOrder(int queryCircuitOrder) {
		this.getQueryCircuitOrders().add(queryCircuitOrder);
	}

	public List<Integer> getQueryCircuitSizes() {
		return this.queryCircuitSizes;
	}

	public void addQueryCircuitSize(int queryCircuitSize) {
		this.getQueryCircuitSizes().add(queryCircuitSize);
	}

	public int getMaximumCircuitOrder() {
		int maximumOrder = this.getPartitionCircuitOrder();
		for (int order : this.getQueryCircuitOrders()) {
			maximumOrder = Math.max(maximumOrder, order);
		}
		return maximumOrder;
	}

	public int getMaximumCircuitSize() {
		int maximumSize = this.getPartitionCircuitSize();
		for (int size : this.getQueryCircuitSizes()) {
			maximumSize = Math.max(maximumSize, size);
		}
		return maximumSize;
	}

	public int getMinimumCircuitSize() {
		int minimumSize = this.getPartitionCircuitSize();
		for (int size : this.getQueryCircuitSizes()) {
			minimumSize = Math.min(minimumSize, size);
		}
		return minimumSize;
	}

	public void setCompilable(boolean value) {
		this.compilable = value;
	}

	public boolean isCompilable() {
		return this.compilable;
	}

	public String getLearnedModelAsString() {
		StringBuilder result = new StringBuilder();

		for (String line : this.getLearnedModel()) {
			result.append(line);
			result.append(Utils.getNewLine());
		}

		return result.toString();
	}

	@Override
	public int compareTo(Score otherScore) {
		if (this.getLogLikelihood() < otherScore.getLogLikelihood()) {
			return 1;
		}
		else if (this.getLogLikelihood() > otherScore.getLogLikelihood()) {
			return -1;
		}
		else {
			return 0;
		}
	}

	private List<String> getLearnedModel() {
		return this.learnedModel;
	}

}
