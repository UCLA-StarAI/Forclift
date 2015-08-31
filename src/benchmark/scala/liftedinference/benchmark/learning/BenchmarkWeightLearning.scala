/*
 * Copyright 2015 Guy Van den Broeck and Wannes Meert
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

package liftedinference.benchmark.learning

import liftedinference._
import liftedinference.learning._
import liftedinference.languages.mln._
import liftedinference.inference._
import liftedinference.util.LogOps._

import cc.factorie.optimize._

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import scala.io._

@RunWith(classOf[JUnitRunner])
class BenchmarkWeightLearning extends FunSpec with Matchers {

    println("Running from directory:")
    println(System.getProperty("user.dir"))

    describe("IMDB bug2") {

        val parser = new MLNParser
        val parsere = new MLNParser
        parser.setLearnModus(true)
        parsere.setLearnModus(true)
        // Smoking MLN
        val mlnFile = new File("./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.bug2.mln")
        val mlnString = Source.fromFile(mlnFile).mkString
        // Database file for training
        val dbFile = new File("./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.bug2.db")
        val dbString = Source.fromFile(dbFile).mkString

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

// vim: set ts=4 sw=4 et:

