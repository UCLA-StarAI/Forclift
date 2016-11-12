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

package edu.ucla.cs.starai.forclift.rcr

import scala.collection._
import edu.ucla.cs.starai.forclift.compiler._

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._
import edu.ucla.cs.starai.forclift.inference._
import scala.util.Random._

import util.KLD._


class NoTruthRCR(
  weightedCNF: WeightedCNF,
  rcrCompilerBuilder: Compiler.Builder = Compiler.Builder.default,
  verbose: Boolean = false) extends RelaxCompensateRecover(weightedCNF, rcrCompilerBuilder, verbose) {

  override def onCompensateRecoverStart(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {
    super.onCompensateRecoverStart(initialWeightFunction, compensations, marginalCircuitsSet)
    val relaxedCNF: CNF = marginalCircuitsSet.independentZs.map { _.cnf }.foldLeft(new CNF(Nil)) { _ ++ _ }
    println("Relaxed initial structure:")
    println(relaxedCNF)
    println
  }

  override def onStartCompensation(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {
    super.onStartCompensation(initialWeightFunction, compensations, marginalCircuitsSet)
    val relaxedCNF: CNF = marginalCircuitsSet.independentZs.map { _.cnf }.foldLeft(new CNF(Nil)) { _ ++ _ }
    println("Recovered structure:")
    println(relaxedCNF)
    println
  }

  override def onEndCompensation(
    weights: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {
    super.onEndCompensation(weights, compensations, marginalCircuitsSet)
    println("Compensation converged to weights:")
    for (compensation <- compensations) {
      println(" - " + compensation.eq.thetaCopy + "=" + weights(compensation.eq.thetaCopy))
      println(" - " + compensation.eq.thetaOrig + "=" + weights(compensation.eq.thetaOrig))
    }
    for (marginalCircuit <- marginalCircuitsSet.origMarginals) {
      println("Probability for class of queries " + marginalCircuit.queryClass + " is " + marginalCircuit.marginal)
    }
    println
  }

  override def onCompensationIteration(
    i: Int,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet,
    weights: PredicateWeights) {
    super.onCompensationIteration(i, compensations, marginalCircuitsSet, weights)
    //    	println("Compensation iteration " + i)
  }

}
