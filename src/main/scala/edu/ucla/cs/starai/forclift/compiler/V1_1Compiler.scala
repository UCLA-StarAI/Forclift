/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
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

package edu.ucla.cs.starai.forclift.compiler

import collection._
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._

object V1_1Compiler {

  val builder: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new V1_1Compiler(sizeHint) with LiftedCompiler

  val builderWithGrounding: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new V1_1Compiler(sizeHint) with GroundingCompiler

}

abstract class V1_1Compiler(sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_)) extends NIPS11Compiler(sizeHint) {

  def tryTautologyClauseElimination(cnf: CNF) = {
    val newCnf = cnf.removeTautologies
    if (newCnf eq cnf) None
    else Some(compile(newCnf))
  }
  override def inferenceRules: List[InferenceRule] = List(
    tryCache,
    tryTautology,
    tryContradictionClause,
    tryPositiveUnitClause,
    tryNegativeUnitClause,
    tryPositiveUnitPropagation,
    tryNegativeUnitPropagation,
    tryTautologyClauseElimination, // added wrt NIPS11
    tryIndependentSubtheories,
    tryIndependentSubtheoriesAfterShattering,
    tryGroundDecomposition,
    tryInclusionExclusion,
    tryShatter,
    tryIndependentPartialGrounding, // O(log(n))
    tryCounting, // O(n)
    tryDomainRecursion // is O(log(n)) now! But assumes no unary predicates
    )
}
