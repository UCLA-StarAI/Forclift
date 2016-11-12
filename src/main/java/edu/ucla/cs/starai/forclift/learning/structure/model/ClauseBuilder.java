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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.ucla.cs.starai.forclift.learning.structure.util.Settings;
import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

/**
 * This class can be used to build Clause objects from strings that represent
 * first-order formulas in either the BUSL or MSL format.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Tuesday 2 July 2013
 */
public class ClauseBuilder {

	/**
	 * This method transforms the string representation of a theory into a set
	 * of Clause objects.
	 */
	public static Set<Clause> buildClausesFromTheory(final String formulas) {

		Set<Clause> clauses = new HashSet<Clause>();

		for (String formula : formulas.split(Utils.getNewLine())) {
			if (!formula.isEmpty()) {
				Clause clause = buildClauseFromString(formula);
				clauses.add(clause);
			}
		}

		return clauses;
	}

	/**
	 * This method transforms the string representation of a theory into a map
	 * of Clause objects indexed by their length.
	 */
	public static Map<Integer, Set<Clause>> buildClausesPerLengthFromTheory(final String formulas) {

		Map<Integer, Set<Clause>> clauses = new HashMap<Integer, Set<Clause>>();

		for (String formula : formulas.split(Utils.getNewLine())) {
			if (!formula.isEmpty()) {
				Clause clause = buildClauseFromString(formula);
				int length = clause.getLiterals().size();
				if (!clauses.containsKey(length)) {
					clauses.put(length, new HashSet<Clause>());
				}
				clauses.get(length).add(clause);
			}
		}

		return clauses;
	}

	/**
	 * This method transforms the string representation of a first-order formula
	 * in the BUSL or MSL format into a Clause object.
	 * 
	 * Example BUSL: workedUnder(-1,-2) v !workedUnder(-3,-2) v !actor(-1)
	 * Example MSL: !workedUnder(v0,v1) v !movie(v2,v0) v !movie(v2,v1)
	 */
	public static Clause buildClauseFromString(final String clause) {

		// 1. split into literals
		Set<String> literals = splitIntoLiterals(clause);

		// 2. sort literals according to their natural order
		List<String> literalsList = new ArrayList<String>(literals);
		Collections.sort(literalsList);

		// 3. construct Literal objects
		List<Literal> literalObjects = new ArrayList<Literal>();
		List<String> argumentsCache = new ArrayList<String>();

		for (String literal : literalsList) {

			boolean truthValue = isPositiveLiteral(literal);
			String name = extractName(literal);
			List<String> arguments = extractArguments(literal);

			List<String> argumentsList = new ArrayList<String>();

			for (String argument : arguments) {
				// the argument numbers follow the order in which they appear in
				// the clause
				if (!argumentsCache.contains(argument)) {
					argumentsCache.add(argument);
				}
				argumentsList.add(Settings.VARIABLE_NAME + argumentsCache.indexOf(argument));
			}

			literalObjects.add(new Literal(name, truthValue, argumentsList));
		}

		return new Clause(literalObjects);
	}

	/**
	 * This method converts the string representation of a clause into a set of
	 * string representations of literals.
	 */
	private static Set<String> splitIntoLiterals(final String clause) {
		Set<String> result = new HashSet<String>();

		String[] literals = clause.split(Settings.DISJUNCTION);
		for (String literal : literals) {
			result.add(literal);
		}

		return result;
	}

	/**
	 * This method indicates whether or not the given literal is positive.
	 */
	private static boolean isPositiveLiteral(final String literal) {
		return !literal.contains(Settings.NEGATION);
	}

	/**
	 * This method extracts the name from the given literal.
	 */
	private static String extractName(final String literal) {
		String atom = literal.replaceAll("!", "");

		return atom.substring(0, atom.indexOf(Settings.START_ARGUMENTS));
	}

	/**
	 * This method extracts the arguments from the given literal.
	 */
	private static List<String> extractArguments(final String literal) {
		String[] arguments = literal.substring(literal.indexOf(Settings.START_ARGUMENTS) + 1, literal.indexOf(Settings.END_ARGUMENTS)).split(Settings.ARGUMENT_SEPARATOR);

		return new ArrayList<String>(Arrays.asList(arguments));
	}

}
