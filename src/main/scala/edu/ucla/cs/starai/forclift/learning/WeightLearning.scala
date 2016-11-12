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
import edu.ucla.cs.starai.forclift.languages.mln._

import edu.ucla.cs.starai.forclift.inference._

import java.io.File

object WeightLearning {

  def parseAndlearnWeights(
    structure: String,
    traindbs: Seq[String],
    verbose: Boolean = false,
    normalizeLH: Boolean = false,
    doLL: Boolean = false,
    doPLL: Boolean = false,
    testdbs: Seq[String] = Seq(),
    skolemize: Boolean = true): MLN = {
    
    val theoryParser = new MLNParser
    theoryParser.setLearnModus(true)
    
    val startParsing = System.currentTimeMillis

    val mln_structure = theoryParser.parseMLN(structure)
    if (verbose) {
      println("Parsing MLN structure:")
      println(mln_structure)
      println("with domains:")
      println(mln_structure.domainSizes)
      println
      println("I will learn weights for:")
      if (skolemize) {
        println(mln_structure.toMLNSkolemized.toMLNasAlchemyCNF)
      } else {
        println(mln_structure.toMLNasAlchemyCNF)
      }
      println
    }
    if (verbose) println("Parsing " + traindbs.size + " databases.")

    val traindbMlns = traindbs.map { theoryParser.parseDB(_) }
    if (verbose) {
      for (dbMln <- traindbMlns) {
        println("Parsed training db with domains:")
        println(dbMln.domainSizes)
        println
      }
    }

    val testdbMlns = testdbs.map { theoryParser.parseDB(_) }
    if (verbose) {
      for (dbMln <- testdbMlns) {
        println("Parsed test db with domains:")
        println(dbMln.domainSizes)
        println
      }
    }
    println("Parsing took " + (System.currentTimeMillis
 - startParsing) + " ms")
    
    learnWeights(
      mln_structure,
      traindbMlns,
      verbose,
      normalizeLH,
      doLL,
      doPLL,
      testdbMlns,
      skolemize)
    
  }
    
  /**
   * Learn the weights for a structure given a set of databases.
   */
  def learnWeights(
    mln_structure: MLN,
    traindbMlns: Seq[MLN],
    verbose: Boolean = false,
    normalizeLH: Boolean = false,
    doLL: Boolean = false,
    doPLL: Boolean = false,
    testdbMlns: Seq[MLN] = Seq(),
    skolemize: Boolean = true): MLN = {
    

    val startLearning = System.currentTimeMillis

    if (verbose) println("Set up learning")
    val learner = new LiftedLearning(mln_structure,
      traindbMlns,
      verbose = verbose,
      normalizeLH = normalizeLH,
      testdbMLNs = testdbMlns,
      skolemize = skolemize)
    if (verbose) println("Start learning")
    val learnedMLN = learner.learnParameters()

    val endLearning = System.currentTimeMillis
    println("Learning took " + (endLearning - startLearning) + " ms")

    println
    println("Learned model:")
    // Print the full MLN (domain declarations + weighted formulas)
    println(learnedMLN._1.toStringFull)
    println("End learned model")

    if (doLL) {
      if (verbose) println("Start calculating likelihood")
      //learner.reevaluateZ
      //learner.reevaluateQueryCircuits
	    for(dbLh <- learner.testDatabaseLikelihoods) yield {
	      println(s"test db: $dbLh")
	      println(s"likelihood = ${dbLh.likelihood.toDouble}")
	      println(s"loglikelihood = ${dbLh.likelihood.logToDouble}")
	    }
    }

    if (doPLL) {
      if (verbose) println("Start calculating pseudo-likelihood")
      val tlplls = learner.testLogPseudoLikelihood
      tlplls.foreach { lpll =>
        val pll = math.exp(lpll)
        println("test db:")
        println("logpseudolikelihood = " + lpll)
        println("pseudolikelihood = " + pll)
      }
    }

    learnedMLN._1
  }

}
