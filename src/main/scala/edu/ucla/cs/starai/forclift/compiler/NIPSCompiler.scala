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
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift._

object NIPS11Compiler {

  val builder: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new NIPS11Compiler(sizeHint) with LiftedCompiler

  val builderWithGrounding: Compiler.Builder = (sizeHint: Compiler.SizeHints) => new NIPS11Compiler(sizeHint) with GroundingCompiler

}

abstract class NIPS11Compiler(sizeHint: Compiler.SizeHints = Compiler.SizeHints.unknown(_)) extends IJCAI11Compiler(sizeHint) {

  def tryDomainRecursion(cnf: CNF) = {
    assume(cnf.clauses.forall { _.singletonLiterals.isEmpty })
    assume(cnf.clauses.forall { _.groundLiterals.isEmpty })
    if (cnf.clauses.forall { clause =>
      val clauseVars = clause.literalVariables
      clause.atoms.forall { _.variables == clauseVars }
    }) {
      assume(cnf.clauses.forall {
        _.literalVariables.size == cnf.clauses.head.literalVariables.size
      }, "All clauses have the same number of logical variables")
      // I think this code otherwise ignores the case when there are two root binding classes of size > 1!!)
      assume(2 == cnf.clauses.head.literalVariables.size, "All clauses have 2 logical variables")
      val domain = cnf.clauses.head.constrs.domainFor(cnf.clauses.head.literalVariables.head)
      assume(cnf.clauses.forall { clause =>
        clause.literalVariables.forall { clause.constrs.domainFor(_) == domain }
      }, "There is only one domain for all logical variables, if IPG failed.")
      val ineqs = cnf.clauses.head.constrs.ineqConstrs(cnf.clauses.head.literalVariables.head).collect { case c: Constant => c }
      val constant = groundingConstantFor(cnf, domain)
      val mixedClauses = cnf.clauses.flatMap { clause =>
        clause.literalVariables.subsets.flatMap { equalVariables =>
          if (equalVariables.isEmpty || equalVariables.size == clause.literalVariables.size) List()
          else {
            val substitutedClause = clause.substitute((variable: Var) =>
              if (equalVariables.contains(variable)) constant else variable)
            val ineqVars = clause.literalVariables -- equalVariables
            List(ineqVars.foldLeft(substitutedClause) { _.addInequality(_, constant) })
          }
        }
      }
      val mixedCNF = new CNF(mixedClauses)
      val headVar1 = cnf.clauses.head.literalVariables.head
      val headVar2 = (cnf.clauses.head.literalVariables - headVar1).head
      val groundClauses = if (cnf.clauses.head.constrs.ineqConstrs(headVar1).contains(headVar2)) {
        assume(cnf.clauses.forall { clause =>
          val headVar1 = clause.literalVariables.head
          val headVar2 = (clause.literalVariables - headVar1).head
          clause.constrs.ineqConstrs(headVar1).contains(headVar2)
        }, "All clauses have the same inequality constraints")
        // the ground clauses are empty!
        Nil
      } else {
        cnf.clauses.map { _.substitute { v => constant } }
      }
      val groundCNF = new CNF(groundClauses)
      val msg = "Domain recursion on $" + domain + "$"
      val mixedNnf = tryIndependentPartialGrounding(mixedCNF)
      assume(mixedNnf.nonEmpty) // property of DR?
      Some(new DomainRecursionNode(cnf, mixedNnf.get, compile(groundCNF), constant, ineqs, domain, msg))
    } else None
  }

  override def inferenceRules: List[InferenceRule] = {
    super.inferenceRules ::: List[InferenceRule](tryDomainRecursion)
  }

}
