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

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * This class represents a task.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Wednesday 3 July 2013
 */
public abstract class Task {

	private final List<File> databases;

	private final TaskCollection task;

	public Task(final List<File> databases, TaskCollection task) {
		this.databases = databases;
		this.task = task;
	}

	protected List<File> getDatabases() {
		return this.databases;
	}

	protected TaskCollection getTask() {
		return this.task;
	}

	protected abstract String generateCommand(File modelFile)
			throws IOException;

}
