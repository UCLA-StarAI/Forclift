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

import scala.io._

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import liftedinference._
import liftedinference.learning._
import liftedinference.util.LogOps._
import liftedinference.languages.mln._
import liftedinference.inference._

@RunWith(classOf[JUnitRunner])
class BenchmarkImdbWeightLearning extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  println("Running from directory:")
  println(System.getProperty("user.dir"))
  var mln  = MLN()
  var db   = MLN()

  def parse(mlnfile: String, dbfile: String): (MLN,MLN) = {
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnstring = Source.fromFile(mlnfile).mkString
    val dbstring = Source.fromFile(dbfile).mkString

    val mln = parser.parseMLN(mlnstring)
    val db = parser.parseDB(dbstring)
    (mln,db)
  }
  //--------------------------------------------------------------------------

//    describe("IMDB bug JanVH") {
//
//        val parser = new MLNParser
//        val parsere = new MLNParser
//        parser.setLearnModus(true)
//        parsere.setLearnModus(true)
//        // Smoking MLN
//        val mlnFile = new File("./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.janvh1.mln")
//        val mlnStr = Source.fromFile(mlnFile).mkString
//        // Database file for training
//        val dbFile = new File("./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.db")
//        val dbStr = Source.fromFile(dbFile).mkString
//
//        var mln  = MLN()
//        var db   = MLN()
//
//        it("MLN is parsable") {
//            //println("Testing MLN:\n"+mlnStr)
//            mln = parser.parseMLN(mlnStr)
//            //println(mln)
//        }
//
//        it("DB is parsable") {
//            db = parser.parseDB(dbStr)
//        }
//
//        it("Should have correct pseudo-likelihood") {
//            val pllstat = Likelihood.mlnPseudoLikelihood(mlnStr, Seq(dbStr), normalizepll=true)
//            pllstat.dblogll.head should be (-6.93147 +- 0.01)
//        }
//
//        //it("Should have correct likelihood") {
//          //val llstat = Likelihood.mlnLikelihood(mlnStr, Seq(dbStr))
//          //println("ll = %f" format llstat.dbll.head)
//        //}
//    }


	describe("IMDA bug3") {

		it("MLN and DB are parsable") {
			parse("./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.bug3.mln",
					"./src/benchmark/scala/liftedinference/benchmark/learning/imdb/imdb.1.db") match {
					case (m,d) => mln=m;db=d
			}
		}
		
		it("Compilable") {
			mln.toWeightedCNF(false).showNnfPdf(false, 10, "bug", true)
		}
		

		it("Learnable") {
			val learner = new LiftedLearning(mln, Seq(db), verbose=true)
			val learnedMLN = learner.learnParameters()
			println(learnedMLN)
		}
	}

}

