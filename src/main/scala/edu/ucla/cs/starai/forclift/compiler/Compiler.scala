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

object Compiler {

  object Builder {
    val default: Builder = V1_1Compiler.builder
    val defaultWithGrounding: Builder = V1_1Compiler.builderWithGrounding
  }

  type Builder = ((Domain => Int) => Compiler)

  def default: Compiler = new V1_1Compiler() with LiftedCompiler

  object SizeHints {
    def unknown(d: Domain) = 1
  }

  type SizeHints = (Domain => Int)

}

trait Compiler {

  def compile(cnf: CNF): NNFNode

  def compileSmooth(cnf: CNF): NNFNode = {
    compileSmooth(cnf, cnf.predicates, Set.empty)
  }

  def compileSmooth(cnf: CNF, predicates: Set[Predicate], excluded: Set[PositiveUnitClause] = Set.empty): NNFNode = {
    val nnf = compile(cnf)
    nnf.smoothWithPredicates(predicates, excluded)
  }

}

trait GroundingCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    new GroundingNode(cnf, "No rule fires.")
  }

}

trait LiftedCompiler extends AbstractCompiler {

  override def cannotCompile(cnf: CNF): NNFNode = {
    throw new IllegalArgumentException("Cannot compile " + cnf + " without grounding.")
  }

}

abstract class AbstractCompiler extends Compiler {

  val nnfCache = new mutable.HashMap[CNF, NNFNode]

  def updateCache(cnf: CNF, nnf: NNFNode) {
    assume(nnf != null)
    if (!nnfCache.contains(cnf)) nnfCache(cnf) = nnf
  }

  def tryCache(cnf: CNF) = {
    nnfCache.get(cnf).map { new Ref(cnf, _, "Cache hit.") }
  }

  type InferenceRule = CNF => Option[NNFNode]

  def inferenceRules: List[InferenceRule]

  var nbCompilationSteps = 0;
  
  def checkCnfInput(cnf: CNF) {
    require(!cnf.domains.contains(Universe), s"Cannot compile CNFs containing the universe domain: $cnf")
    require(!cnf.domains.contains(EmptyDomain), s"Cannot compile CNFs containing the empty domain: $cnf")
  }

  def compile(cnf: CNF): NNFNode = {
    //    nbCompilationSteps += 1
    //    println("COMPILING CNF:\n"+cnf)
    //    println("COMPILATION STEP"+nbCompilationSteps)
    //    println("CNF SIZE: "+cnf.size)
    //    if(nbCompilationSteps>2000){
    //        println("DEBUG")
    //    }
    // avoid stack overflow by reducing recursion in compile function
    checkCnfInput(cnf)
    var rules = inferenceRules
    var nnf: NNFNode = null
    while (nnf == null && rules.nonEmpty) {
      val tryRule = rules.head(cnf)
      if (tryRule.nonEmpty) nnf = tryRule.get
      else rules = rules.tail
    }
    if (nnf == null) {
      nnf = cannotCompile(cnf)
    }
    updateCache(cnf, nnf)
    nnf
  }

  def cannotCompile(cnf: CNF): NNFNode

}
