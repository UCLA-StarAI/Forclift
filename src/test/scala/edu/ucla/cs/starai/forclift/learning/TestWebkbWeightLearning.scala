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
import edu.ucla.cs.starai.forclift.learning._
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.inference._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestWebkbWeightLearning extends FunSpec with Matchers {

  println("Running from directory:")
  println(System.getProperty("user.dir"))

  describe("Webkb partition loop") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnFile = "/webkb/webkb_bug1.mln"
    val mlnString = Resource.fromFile(mlnFile).mkString
    val dbFile = "/webkb/corn20_bug1.db"
    val dbString = Resource.fromFile(dbFile).mkString

    var mln  = MLN()
    var db   = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("DB is parsable") {
      db = parser.parseDB(dbString)
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()

      println(learnedMLN)
    }
  }
}
