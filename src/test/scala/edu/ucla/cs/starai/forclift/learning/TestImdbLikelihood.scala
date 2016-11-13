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

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import edu.ucla.cs.starai.forclift.languages.mln.MLN

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser
import scala.io.Source
import java.io.File
import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestImdbLikelihood extends FunSpec with Matchers with ResourceParseHelper {
  
  //--------------------------------------------------------------------------
  println("Running from directory:")
  println(System.getProperty("user.dir"))
  var mln  = MLN()
  var db   = MLN()

  //--------------------------------------------------------------------------

    describe("IMDB bug JanVH") {

        val parser = new MLNParser
        val parsere = new MLNParser
        parser.setLearnModus(true)
        parsere.setLearnModus(true)
        // Smoking MLN
        val mlnFile = Resource.fromFile("/imdb/imdb.janvh1.mln")
        val mlnStr = mlnFile.mkString
        // Database file for training
        val dbFile = Resource.fromFile("/imdb/imdb.db")
        val dbStr = dbFile.mkString

        var mln  = MLN()
        var db   = MLN()

        it("MLN is parsable") {
            //println("Testing MLN:\n"+mlnStr)
            mln = parser.parseMLN(mlnStr)
            //println(mln)
        }

        it("DB is parsable") {
            db = parser.parseDB(dbStr)
        }

        it("Should have correct pseudo-likelihood") {
            val pllstat = Likelihood.mlnPseudoLikelihood(mln, Seq(db), normalizepll=true)
            pllstat.head.logToDouble should be (-6.93147 +- 0.01)
        }

        it("Should have correct likelihood") {
          val llstat = Likelihood.mlnLikelihood(mln, Seq(db))
          println("ll = %f" format llstat.head.logToDouble)
          llstat.head.logToDouble should be (-5597.856630202117 +- 0.01)
        }
    }

}
