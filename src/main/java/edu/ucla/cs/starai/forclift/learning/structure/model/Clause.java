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
 * This class represents a clause.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Tuesday 2 July 2013
 */
public class Clause {

	private final List<Literal> literals;

	public Clause(final List<Literal> literals) {
		this.literals = literals;
	}

	public List<Literal> getLiterals() {
		return Collections.unmodifiableList(this.literals);
	}

	@Override
	public boolean equals(Object object) {
		if (!Clause.class.isAssignableFrom(object.getClass())) {
			return false;
		}

		Clause otherClause = (Clause) object;

		if (this.hashCode() == otherClause.hashCode()) {
			return true;
		}
		else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		int result = 1;

		for (Literal literal : this.getLiterals()) {
			result *= 31;
			result += literal.hashCode();
		}

		return result;
	}

	@Override
	public String toString() {
		StringBuilder stringBuilder = new StringBuilder();

		String infix = "";
		for (Literal literal : this.getLiterals()) {
			stringBuilder.append(infix);
			stringBuilder.append(literal.toString());
			infix = Settings.DISJUNCTION;
		}

		return stringBuilder.toString();
	}

}
