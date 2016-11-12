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
import edu.ucla.cs.starai.forclift.util.SignLogDouble._
import scala.util.Random._
import util.KLD._

class MarginalCircuitsSet(
  val independentZs: List[CNFCircuit],
  val origMarginals: List[MarginalCircuits],
  val copyMarginals: List[MarginalCircuits],
  compiler: Compiler,
  domainSizes: DomainSizes) {

  def recover(comp: Compensation) = {
    val z1 = comp.copyMarginal.Z
    val z2 = comp.origMarginal.Z
    val newZ = {
      val z1AndZ2 = if (z1 eq z2) z1.cnf else z1.cnf ++ z2.cnf
      val z1AndZ2WithEq = comp.eq.substituteCopy(z1AndZ2)
      val z1AndZ2WithEqWithoutThetas = new CNF(z1AndZ2WithEq.clauses.filter { clause =>
        !clause.predicates.contains(comp.eq.thetaOrig) && !clause.predicates.contains(comp.eq.thetaCopy)
      })
      //        if (verbose) {
      //          println("Merged")
      //          println(z1.cnf)
      //          println("--and")
      //          println(z2.cnf)
      //          println("--into")
      //          println(z1AndZ2WithEqWithoutThetas)
      //          println
      //        }
      z1AndZ2WithEqWithoutThetas
    }
    val newZCircuit = new CNFCircuit(compiler, newZ)
    val newIndependentZs = newZCircuit :: (independentZs filterNot (x => x == z1 || x == z2))
    def mapZs(marginal: MarginalCircuits, from1: CNFCircuit, from2: CNFCircuit, to: CNFCircuit) = {
      if (marginal.Z == from1 || marginal.Z == from2) new MarginalCircuits(compiler, to, marginal.queryClass, domainSizes)
      else marginal
    }
    val mappedOrigMarginals = origMarginals.map { before => (before -> mapZs(before, z1, z2, newZCircuit)) }.toMap
    val mappedCopyMarginals = (copyMarginals filterNot (_ == comp.copyMarginal)).map { before => (before -> mapZs(before, z1, z2, newZCircuit)) }.toMap
    val mappedThis = new MarginalCircuitsSet(newIndependentZs, mappedOrigMarginals.values.toList, mappedCopyMarginals.values.toList, compiler, domainSizes)
    (mappedThis, mappedOrigMarginals, mappedCopyMarginals)
  }

  def cachedZ(weights: PredicateWeights) = {
    independentZs.foldLeft(one) { _ * _.cachedWmc }
  }

}
