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

import edu.ucla.cs.starai.forclift.learning.structure.util.Utils;

/**
 * This class can be used to build Model objects from strings that represent a
 * first-order theory.
 * 
 * @author Jan Van Haaren <jan.vanhaaren@cs.kuleuven.be>
 * @date Wednesday 3 July 2013
 */
public class ModelBuilder {

	public static Model buildEmptyModelFromTheory(String theory) {
		Model model = new Model();
		for (String formula : theory.split(Utils.getNewLine())) {
			if (!formula.isEmpty()) {
				model.addTyping(formula);
			}
		}
		return model;
	}

}
