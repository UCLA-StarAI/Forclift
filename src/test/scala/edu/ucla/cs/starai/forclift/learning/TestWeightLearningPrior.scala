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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import java.io._

import scala.io._

import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TestWeightLearningPrior extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  describe("uniform data with strong prior") {

    val parser = new MLNParser
    parser.isLearnModus = true

    val structureStr =
      """
Q(person)
R(person)
0 R(x)
0 Q(x)
      """


    val trainingDBStr =
      """
R(1)
Q(3)
"""

    val structure = parser.parseMLN(structureStr)
    val db = parser.parseDB(trainingDBStr)
    it(" should learn weights towards that prior"){
        val learner = new LiftedLearning(structure, Seq(db), mu = 5, sigma = 2, verbose=false)
        val learnedMLN = learner.learnParameters()
        for( wFormula <- learnedMLN._1.wformulas){
          assert(wFormula.weight > 1)
        }
    }
  }
}
