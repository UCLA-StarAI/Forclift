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

import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestConvergence extends FunSpec with Matchers {

  describe("Convergence bug") {

    val theoryStr = Resource.fromFile("/uwcse/convergencetest.mln").mkString
    val dbs = Seq(
        "/uwcse/uwcse_fold1.db",
        "/uwcse/uwcse_fold3.db",
        "/uwcse/uwcse_fold4.db",
        "/uwcse/uwcse_fold5.db")
        .map(f => Resource.fromFile(f).mkString)

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(theoryStr)
    val db = dbs.map(parser.parseDB(_))
      
    var learnedMLN: MLN = null;

    it("should learn an MLN with nonzero weights") {
      learnedMLN = WeightLearning.learnWeights(
        mln,
        db,
        false,
        false)
      println(learnedMLN)
      learnedMLN.wformulas.filter(_.weight != 0) should be('nonEmpty);
    }
  }
}
