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

import edu.ucla.cs.starai.forclift.inference.PrecompiledCNFCircuit
import edu.ucla.cs.starai.forclift.nnf.NNFNode
import edu.ucla.cs.starai.forclift.GInt
import edu.ucla.cs.starai.forclift.inference.ExchangeableGroundingsCircuit
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.inference.PredicateWeights
import edu.ucla.cs.starai.forclift.util.SignLogDouble

case class DatabaseLikelihood(
  db: Database,
  formulaCircuits: Seq[CircuitsForDatabase],
  z: PrecompiledCNFCircuit) {

  // likelihoods in exp-space

  def likelihood() = {
    val numerator = formulaCircuits.map(_.numeratorLikelihood).reduce(_ * _)
    val lh = numerator / z.cachedWmc
    require(lh >= 0, s"$lh is not a valid likelihood")
    require(lh <= 1, s"$lh is not a valid likelihood")
    lh
  }

  def perVariableLikelihood() = {
    likelihood().root(db.vocabularySize)
  }

  // gradients in log-space

  def gradientLogLikelihood(weightId: Int): Double = {
    formulaCircuits(weightId).gradientLogLikelihood
  }

  def gradientPerVariableLogLikelihood(weightId: Int): Double = {
    gradientLogLikelihood(weightId) / db.vocabularySize
  }

}

abstract class CircuitsForDatabase(val db: Database, lf: LearningFormula) {

  def nbGroundings: GInt
  def nbTrueGroundings: GInt
  def nbFalseGroundings = nbGroundings - nbTrueGroundings

  /**
   * Z Circuit for this db's domain size
   */
  private[this] var zCircuit: PrecompiledCNFCircuit = null

  def setZCircuit(zCircuit: PrecompiledCNFCircuit) {
    assume(this.zCircuit == null)
    this.zCircuit = zCircuit
  }

  def getZCircuit = zCircuit

  /**
   * Query circuits for all equivalence classes and this db's domain size
   */
  private[this] var queryCircuits: IndexedSeq[ExchangeableGroundingsCircuit] = null

  def initializeCircuits(generalCircuits: IndexedSeq[(PositiveUnitClause, NNFNode)]) {
    assume(queryCircuits == null)
    queryCircuits = generalCircuits.flatMap {
      case (query, circuit) =>
        val exchCircuit = new ExchangeableGroundingsCircuit(query, circuit, getZCircuit, db.domainSizes)
        // remove circuits with 0 ground queries for the domain
        if (exchCircuit.nbMarginals > 0) {
          List(exchCircuit);
        } else {
          if(lf.verbose) println("Removing query circuit for " + query + " in db " + db + " since it has no groundings.")
          List();
        }
    }
    assume(queryCircuits.map { _.nbMarginals }.sum == nbGroundings, (
      "Counting " + lf.res + " in " + db + ": " + queryCircuits.map { _.nbMarginals }.mkString("+") + "="
      + queryCircuits.map { _.nbMarginals }.sum + " marginals but " + nbGroundings + " groundings"))
  }

  def reevaluateQueryCircuits(predicateWeights: PredicateWeights) {
    for (circuit <- queryCircuits) {
      circuit.clearCache
      circuit.cacheWmc(predicateWeights)
      //                circuit.smoothNNF.showPDF(db.domainSizes, predicateWeights)
    }
  }

  def expectedNumGroundings(): Double = {
    val expectedNumGroundings = queryCircuits.map { circuit =>
      val expectedNumGroundings = circuit.marginal.toDouble * circuit.nbMarginals
      if(lf.verbose) println(s"      ${circuit.queryClass} on $db | "
        + s"marginal: ${circuit.marginal.toFloat}, "
        + s"nbGroundings: ${circuit.nbMarginals}, "
        + s"expectedNumGroundings: $expectedNumGroundings")
      expectedNumGroundings
    }.sum
    if(lf.verbose) println(s"    ${lf.res} on $db | expectedNumGroundings: $expectedNumGroundings, dbNumGroundings: $nbTrueGroundings")
    expectedNumGroundings
  }

  /**
   * Returns e^{w * n} for this formula and database
   */
  def numeratorLikelihood: SignLogDouble = {
    val numLh = lf.weights.posW.pow(nbTrueGroundings) * lf.weights.negW.pow(nbFalseGroundings)
    if(lf.verbose) println(s"    ${lf.res} on $db | "
      + s"logWeight: ${lf.logWeight}, "
      + s"nb(t/f)groundings: $nbTrueGroundings / $nbFalseGroundings, "
      + s"numeratorLikelihood: $numLh")
    numLh
  }

  /**
   * Returns dlog(e^{w * n})/dw for this formula and database
   */
  def gradientLogLikelihood: Double = {
    val expectedNbGroundings = expectedNumGroundings()
    (nbTrueGroundings - expectedNbGroundings)
  }

}
