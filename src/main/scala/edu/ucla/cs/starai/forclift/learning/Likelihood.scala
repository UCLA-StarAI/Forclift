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

import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.learning._
import edu.ucla.cs.starai.forclift.languages.mln._
import collection._
import edu.ucla.cs.starai.forclift.util.SignLogDouble

object Likelihood {

  /** Calculate the likelihood for a set of databases given a theory. */
  def mlnLikelihood(
    mln: MLN,
    dbMlns: Seq[MLN],
    verbose: Boolean = false,
    compiledBuilder: Compiler.Builder = Compiler.Builder.default,
    integralformulas: Boolean = false,
    normalizell: Boolean = false): Seq[SignLogDouble] = {

    val learner = new LiftedLearning(
      mln,
      dbMlns,
      verbose = verbose,
      normalizeLH = normalizell)

    learner.reevaluateZ
    
    for(dbLh <- learner.trainDatabaseLikelihoods) yield {
      println(s"db: $dbLh")
      println(s"likelihood = ${dbLh.likelihood.toDouble}")
      println(s"loglikelihood = ${dbLh.likelihood.logToDouble}")
      if(normalizell) dbLh.perVariableLikelihood
      else dbLh.likelihood
      
    }
  }

  /** Calculate the pseudo-likelihood */
  def mlnPseudoLikelihood(
    mln: MLN,
    dbMlns: Seq[MLN],
    verbose: Boolean = false,
    compiledBuilder: Compiler.Builder = Compiler.Builder.default,
    integralformulas: Boolean = false,
    normalizepll: Boolean = false): Seq[SignLogDouble] = {

    val dbpllbuffer = new mutable.ListBuffer[Double]()
    val dblogpllbuffer = new mutable.ListBuffer[Double]()

    // Set up learning for likelihood
    val learner = new LiftedLearning(mln,
      dbMlns,
      verbose = verbose,
      normalizepll = normalizepll)
    val lplls = learner.logPseudoLikelihood

    lplls.map { lpll =>
      val pll = math.exp(lpll)
      println("db:")
      println("pseduolikelihood = " + pll)
      println("logpseudolikelihood = " + lpll)
      SignLogDouble.fromLog(lpll)
    }
  }
}
