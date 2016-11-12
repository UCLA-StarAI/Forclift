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

import java.util.Collections;
import java.util.List;

import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;

/**
 * This class represents a literal.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Tuesday 2 July 2013
 */
public class Literal {

	private final String name;

	private final boolean truthValue;

	private final List<String> arguments;

	public Literal(final String name, final boolean truthValue, final List<String> arguments) {
		this.name = name;
		this.truthValue = truthValue;
		this.arguments = arguments;
	}

	public String getName() {
		return this.name;
	}

	public boolean getTruthValue() {
		return this.truthValue;
	}

	public List<String> getArguments() {
		return Collections.unmodifiableList(this.arguments);
	}

	public int getNumberOfArguments() {
		return this.getArguments().size();
	}

	@Override
	public int hashCode() {
		int result = 1;

		result *= 31;
		result += this.getName().hashCode();
		result *= 37;
		result += this.getTruthValue() ? 1 : 0;

		for (String argument : this.getArguments()) {
			result *= 41;
			result += argument.hashCode();
		}

		return result;
	}

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();

		// truth value
		if (!this.getTruthValue()) {
			stringBuilder.append(Settings.NEGATION);
		}

		// name
		stringBuilder.append(this.getName());

		// arguments
		stringBuilder.append(Settings.START_ARGUMENTS);
		String infix = "";
		for (String argument : this.getArguments()) {
			stringBuilder.append(infix);
			stringBuilder.append(argument);
			infix = Settings.ARGUMENT_SEPARATOR;
		}
		stringBuilder.append(Settings.END_ARGUMENTS);

		return stringBuilder.toString();
	}

}
