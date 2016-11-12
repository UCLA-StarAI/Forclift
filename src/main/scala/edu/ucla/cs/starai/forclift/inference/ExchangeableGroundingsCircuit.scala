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

package edu.ucla.cs.starai.forclift.inference

import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.nnf._
import edu.ucla.cs.starai.forclift.util.SignLogDouble._


/**
 * A cache for a specific WMC when we already have the circuits
 */
class ExchangeableGroundingsCircuit(
    val queryClass: PositiveUnitClause, queryNNF: NNFNode,
    val zNNF: PrecompiledCNFCircuit, val domainSizes: DomainSizes)
  extends PrecompiledCNFCircuit(queryNNF) {

  val nbMarginals = {
    queryClass.nbGroundings(domainSizes)
  }

  def marginal = {
    var marginal = cachedWmc / zNNF.cachedWmc
    if (marginal > one) {
      if (cachedWmc > zNNF.cachedWmc*1.01) {
        println("bad numerator:")
        //                        println(learningProblem.getPredicateWeights)
        println(domainSizes)
        println("bad numerator:")
        println(queryNNF.cnf)
        println("bad denominator:")
        println(zNNF.cnf)
        println("debug")
        println
        throw new IllegalStateException("marginal above zero:" + marginal)
      }
      marginal = one
    }
    marginal
  }

  def cacheWmc(predicateWeights: PredicateWeights) = super.cacheWmc(domainSizes, predicateWeights)

}
