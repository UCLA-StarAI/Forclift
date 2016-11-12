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
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;

import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;

/**
 * This class controls the execution of WFOMC tasks in multi-threaded mode.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Friday 5 July 2013
 */
public class TaskExecutor {

	private static TaskExecutor INSTANCE;

	private final ScheduledExecutorService taskExecutorService;

	private TaskExecutor() {
		this.taskExecutorService = Executors.newScheduledThreadPool(Settings.getNumberOfAdditionalThreads());
	}

	public static TaskExecutor getInstance() {
		if (TaskExecutor.INSTANCE == null) {
			TaskExecutor.INSTANCE = new TaskExecutor();
		}

		return TaskExecutor.INSTANCE;
	}

	private ScheduledExecutorService getTaskExecutorService() {
		return this.taskExecutorService;
	}

	public void executeScoreTasks(Set<ScoreTask> tasks, boolean isFinal) {

		List<Future<Score>> futures = new ArrayList<Future<Score>>();

		for (final ScoreTask task : tasks) {
			final Future<Score> future = this.getTaskExecutorService().submit(task);
			futures.add(future);
		}

		// wait until all tasks have finished
		boolean keepWaiting = true;

		while (keepWaiting) {
			boolean allTasksHaveFinished = true;
			for (Future<Score> future : futures) {
				allTasksHaveFinished &= future.isDone();
			}
			keepWaiting = !allTasksHaveFinished;
		}

		setScoresFromFutures(futures, isFinal);
	}

	private static void setScoresFromFutures(List<Future<Score>> futures, final boolean isFinal) {
		for (Future<Score> scoreTaskFuture : futures) {
			try {
				Score score = scoreTaskFuture.get();
				if (score.isCompilable()) {
					if (isFinal) {
						score.getModel().addFinalScore(score);
					}
					else {
						score.getModel().addScore(score);
					}
				}
			}
			catch (Exception e) {
				// do nothing - was cancelled or failed
			}
		}
	}

	public void shutdownNow() {
		this.getTaskExecutorService().shutdownNow();
	}

}
