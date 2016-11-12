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

class GroundTruthRCR(
  weightedCNF: WeightedCNF,
  rcrCompilerBuilder: Compiler.Builder = Compiler.Builder.default,
  exactCompilerBuilder: Compiler.Builder = Compiler.Builder.defaultWithGrounding,
  verbose: Boolean = false) extends RelaxCompensateRecover(weightedCNF, rcrCompilerBuilder, verbose) {

  import equip._

  val exactCompiler = exactCompilerBuilder(domainSizes.toInts)

  val exactMarginalCircuits = {

    val exactZ = {
      val exactZ = new CNFCircuit(exactCompiler, weightedCNF.cnf)
      exactZ.cacheWmc(domainSizes, weightedCNF.predicateWeights)
      exactZ
    }

    val exactMarginalCircuits = coshatteredFgCAtoms.map { catom =>
      val marginalCircuit = new MarginalCircuits(exactCompiler, exactZ, catom, domainSizes)
      marginalCircuit.cacheQueryWmc(weightedCNF.predicateWeights)
      (catom -> marginalCircuit)
    }.toMap

    if (verbose) {
      println("Exact CNF")
      println(weightedCNF.cnf)
      println
      println("Exact Log Partition Function = " + exactZ.cachedWmc)
      println
      println("Exact Marginals")
      println(exactMarginalCircuits.values.map { c => "P(" + c.queryClass + ") = " + c.marginal.toDouble }.mkString("\n"))
      println
      println("z circuit size = " + exactZ.smoothNNF.size)
      try {
        println("z circuit size evaluation complexity = O(n^" + exactZ.smoothNNF.evalOrder + ")")
      } catch {
        case e: UnsupportedOperationException => println("z circuit size evaluation complexity = unknown")
      }
      for (marginal <- exactMarginalCircuits.values) {
        println(marginal.queryClass + " circuit size = " + marginal.queryCNFCircuit.smoothNNF.size)
        try {
          println(marginal.queryClass + " circuit size evaluation complexity = O(n^" + marginal.queryCNFCircuit.smoothNNF.evalOrder + ")")
        } catch {
          case e: UnsupportedOperationException => println(marginal.queryClass + " circuit size evaluation complexity = unknown")
        }
      }
    }
    exactMarginalCircuits
  }

  override def onCompensateRecoverStart(
    initialWeightFunction: PredicateWeights,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet) {
    super.onCompensateRecoverStart(initialWeightFunction, compensations, marginalCircuitsSet)
    val recoveredWeightFunction = initialWeightFunction
    val relaxedCNF: CNF = marginalCircuitsSet.independentZs.map { _.cnf }.foldLeft(new CNF(Nil)) { _ ++ _ }
    val recoveredZ = new CNFCircuit(exactCompiler, compensations.map { _.eq }.foldLeft(relaxedCNF) { (cnf, equiv) =>
      equiv.substituteCopy(cnf)
    })
    println("Recovered WMC")
    println((new WeightedCNF(recoveredZ.cnf, domainSizes, recoveredWeightFunction)))
    println
    recoveredZ.cacheWmc(domainSizes, recoveredWeightFunction)
    println("Fully recovered log partition function = " + recoveredZ.cachedWmc)
    println
    val recoveredMarginalCircuits = coshatteredFgCAtoms.map { catom =>
      val marginalCircuit = new MarginalCircuits(exactCompiler, recoveredZ, catom, domainSizes)
      marginalCircuit.cacheQueryWmc(recoveredWeightFunction)
      marginalCircuit
    }
    println("Fully recovered Marginals")
    println(recoveredMarginalCircuits.map { c => "P(" + c.queryClass + ") = " + c.marginal.toDouble }.mkString("\n"))
    println
  }

  def symmetricKLDFromTruth(mc: MarginalCircuits): Double = {
    val queryClass = mc.queryClass
    val nbMarginals = queryClass.nbGroundings(domainSizes)
    val exactMC = exactMarginalCircuits(queryClass)
    val groundingKLD = symmetricKld(exactMC.marginal, mc.marginal).toDouble 
    					+ symmetricKld(exactMC.negMarginal, mc.negMarginal).toDouble
    //    // changed to absolut errpr
    //    def logdiff(a: Double,b:Double) = if(a>b) logminusexp(a,b) else logminusexp(b,a)
    //    import math.exp
    //    val groundingKLD = exp(logdiff(exactMC.logMarginal,mc.logMarginal))
    nbMarginals * groundingKLD
  }

  def symmetricKLDFromTruth(marginalCircuits: List[MarginalCircuits]): Double = {
    marginalCircuits.map { symmetricKLDFromTruth(_) }.sum
  }

  override def onCompensationIteration(
    i: Int,
    compensations: List[Compensation],
    marginalCircuitsSet: MarginalCircuitsSet,
    weights: PredicateWeights) {
    super.onCompensationIteration(i, compensations, marginalCircuitsSet, weights)
    if (verbose) {
      val approxKLDs = marginalCircuitsSet.origMarginals.map { symmetricKLDFromTruth(_) }
      println("Approximation KLD = " + approxKLDs.map { _.toFloat }.mkString(" + "))
      println("                  = " + approxKLDs.sum)
    }
  }

}
