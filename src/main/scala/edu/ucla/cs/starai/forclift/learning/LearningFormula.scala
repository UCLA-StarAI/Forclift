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

package edu.ucla.cs.starai.forclift.learning

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.util._
import edu.ucla.cs.starai.forclift.util.SignLogDouble._
import edu.ucla.cs.starai.forclift.inference.ExchangeableGroundingsCircuit


/**
 * A formula whose weight can be learned.
 *
 * @todo In each learning step also the test db likelihoods are evaluated.
 *       Is this useful?
 */
sealed abstract class LearningFormula(
  nameSpace: WFNameSpace,
  dbs: Databases,
  mu: Double,
  sigma: Double,
  val verbose: Boolean) {

  def mlnPredicates: Set[Predicate]
  def res: Predicate

  var logWeight: Double = 0.0
  def weights: Weights = {
    val weight = new SignLogDouble(true, new LogDouble(logWeight))
    new WeightsFromLog(weight, one)
  }


	def priorDensity: SignLogDouble = {
	  import math._
	  val logDensity = ( - (log(2.0 * Pi) / 2.0) - log(sigma) 
	  			   - (logWeight - mu) * (logWeight - mu) / (2.0 * sigma * sigma))
	  new SignLogDouble(true,new LogDouble(logDensity))
	}
	
	def gradientLogPriorDensity = {
	  // http://www.wolframalpha.com/input/?i=log%28e%5E%28-%28x-mu%29%5E2%2F%282+sigma%5E2%29%29%2F%28sqrt%282+pi%29+sigma%29%29
	  (mu-logWeight) / (sigma * sigma)
	}
    
  
  
  def circuitsForDatabases: IndexedSeq[CircuitsForDatabase]

  /**
   * Create ForDatabase instances using the database augmented with the
   * given evidence atom.
   */
  def customCircuitsForDatabase(db: Database, evidenceAtom: Option[(Atom, Boolean)]): CircuitsForDatabase

  def initializeCircuits(compiler: Compiler, queryClasses: IndexedSeq[PositiveUnitClause],
    zCircuits: IndexedSeq[(Database, PrecompiledCNFCircuit)], vocabularyPredicates: Set[Predicate]) {
    val queryCircuits = queryClasses.map { queryClass =>
      val query = queryClass.getGrounding(dbs.minimalDomainSize)
      val cnf = zCircuits.head._2.cnf ++ CNF(Clause(List(query), Nil))
      val startcompilingt = System.currentTimeMillis()
      if (verbose) println("Compiling circuit for " + queryClass)
      val circuit = compiler.compile(cnf).smoothWithPredicates(vocabularyPredicates)
      if (verbose) {
        if(verbose) println("Compiling took " + (System.currentTimeMillis() - startcompilingt) + " ms")
        if(verbose) println("Query circuit has size " + circuit.size)
        if(verbose) println("Query circuit has order " + circuit.evalOrder)
        if (circuit.evalOrder > 2) {
          println("Evaluating the circuits will be hard, unless caching changes the complexity.")
          //                	println(circuit.cnf)
          //                    circuit.showPDF(null, null, true, 30, "bug.nnf")
        }
      }
      (queryClass, circuit)
    }
    for ((circuitsForDatabase, (db,zCircuit)) <- circuitsForDatabases zip zCircuits) {
      require(db == circuitsForDatabase.db)
      circuitsForDatabase.setZCircuit(zCircuit)
      circuitsForDatabase.initializeCircuits(queryCircuits)
    }
  }

  def reevaluateQueryCircuits(predicateWeights: PredicateWeights) {
    for (circuitsForDatabase <- circuitsForDatabases) {
      circuitsForDatabase.reevaluateQueryCircuits(predicateWeights)
    }
  }

  def learnedFormula: WeightedFormula
  
  override def toString = learnedFormula.toString
  
}

/**
 * A learning formula for complex MLN formulas
 */
case class LearningMLNFormula(
  learningProblem: LiftedLearning,
  wf: WeightedFormula,
  nameSpace: WFNameSpace,
  dbs: Databases,
  mu: Double,
  sigma: Double,
  _verbose: Boolean) extends LearningFormula(nameSpace, dbs, mu, sigma, _verbose) {

  // @todo Removed to allow for unit clauses where different terms are
  //       the same variable. Correct in all cases?
  //require(wf.formula.atoms.size > 1, "Do not add unit clauses to structure:\n"+wf)
  if (wf.formula.atoms.size < 2) println("Warning: Creating LearningMLNFormula for unit clause:\n" + wf)

  logWeight = wf.weight

  override val mlnPredicates: Set[Predicate] = wf.predicates.toSet

  val wmc = {
    val wmc = wf.toWeightedCNF(nameSpace)
    if (verbose) {
      println("Weighted formula: " + wf)
      println(" - has equivalent WFOMC:")
      println(wmc)
      println
    }
    wmc
  }

  val cnf = wmc.cnf

  /**
   * The residual predicate representing the formula and associated with
   * the formula weight.
   */
  val res: Predicate = {
    val resSet = wmc.vocabularyPredicates -- mlnPredicates
    assume(resSet.size == 1, "No weighted predicate found")
    resSet.head
  }

  override val circuitsForDatabases = {
    dbs.dbs.map { db =>
      customCircuitsForDatabase(db)
    }
  }

  override def customCircuitsForDatabase(db: Database, evidenceAtom: Option[(Atom, Boolean)] = None) = {
    new CircuitsForDatabase(db, this) {
      val counter = new FormulaCounter(db, res, cnf, verbose, evidenceAtom)
      val nbGroundings = counter.nbGroundings
      val nbTrueGroundings: GInt = (counter.nbTrueGroundings) //+1.0)/(nbGroundings+2.0)*(nbGroundings)
      //assert(nbTrueGroundings>0)
      if (verbose) {
        println("- MLN formula " + res + " in " + db + (evidenceAtom match { case Some((a, b)) => " with " + a + "=" + b; case None => "" }))
        println("    * has true count " + nbTrueGroundings)
        println("    * has false count " + (nbGroundings - nbTrueGroundings))
      }
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // TODO wannes: Why is this no 0 count supported exactly necessary?
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //require(nbTrueGroundings>0, "We do not support 0 counts ("+res+").")
      //require(nbTrueGroundings<nbGroundings, "We do not support 0 counts ("+res+").")
      //if (nbTrueGroundings > 0 || nbTrueGroundings < nbGroundings)
      //println("Warning: We do not support 0 counts ("+res+"). Only works for pseudo-likelihood.")
    }
  }

  def learnedFormula: WeightedFormula = wf.copy(weight = logWeight)
}

/**
 * A learning formula for unit clauses
 */
case class LearningUnitClause(
  learningProblem: LiftedLearning,
  res: Predicate,
  nameSpace: WFNameSpace,
  dbs: Databases,
  mu: Double,
  sigma: Double,
  _verbose: Boolean) extends LearningFormula(nameSpace, dbs, mu, sigma, _verbose) {

  logWeight = 0.0

  def mlnPredicates = Set(res)

  if (verbose) {
    println("Weighted unit clause: " + learnedFormula)
  }

  override val circuitsForDatabases = {
    dbs.dbs.map { db =>
      customCircuitsForDatabase(db)
    }
  }

  override def customCircuitsForDatabase(db: Database, evidenceAtom: Option[(Atom, Boolean)] = None) = {
    new CircuitsForDatabase(db, this) {
      // number of groundings
      val subDb = db.subDb(Set(res))
      val subDbExt = evidenceAtom match {
        case Some((atom, pos)) if atom.predicate == res => {
          if (pos) subDb + atom
          else subDb - atom
        }
        case _ => subDb
      }

      val nbGroundings = res.toAtom.toPositiveUnitClause.nbGroundings(db.domainSizes)
      val nbTrueGroundings: GInt = subDbExt.size //+1.0)/(nbGroundings+2.0)*(nbGroundings)
      if (verbose) {
        println("- Unit clause " + res + " in " + db + (evidenceAtom match { case Some((a, b)) => " with " + a + "=" + b; case None => "" }))
        println("    * has true count " + nbTrueGroundings)
        println("    * has false count " + nbFalseGroundings)
      }
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      // TODO wannes: Why is this no 0 count supported exactly necessary?
      // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      //require(nbTrueGroundings>0, "We do not support 0 counts ("+res+").")
      //require(nbTrueGroundings<nbGroundings, "We do not support 0 counts ("+res+").")
      //if (nbTrueGroundings > 0 || nbTrueGroundings < nbGroundings)
      //println("Warning: We do not support 0 counts ("+res+"). Only works for pseudo-likelihood.")
    }
  }

  def learnedFormula = WeightedFormula(LiteralFormula(res.toAtom.atom), logWeight, false)

}
