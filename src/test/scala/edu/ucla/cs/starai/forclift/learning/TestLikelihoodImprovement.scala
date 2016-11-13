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
import org.scalatest.FunSpec

import java.io._

import scala.io._
import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestLikelihoodImprovement extends FunSpec with Matchers {

  val db1String = Resource.fromFile("/imdb/imdb_fold1.db").mkString
  val db2String = Resource.fromFile("/imdb/imdb_fold2.db").mkString
  val db3String = Resource.fromFile("/imdb/imdb_fold3.db").mkString
  val db4String = Resource.fromFile("/imdb/imdb_fold4.db").mkString
  val db5String = Resource.fromFile("/imdb/imdb_fold5.db").mkString

  describe("Fold 3") {

    // Smoking MLN
    val baseMlnStr =
      """
male(person)
director(person)
actor(person)
movie(film,person)
workedUnder(person,person)
genre(person,type)
person= {}
type= {}
film= {}

-0.9537143249657458 workedUnder(x,y)
0.4290971316588201 male(x)
-0.9905030614641142 movie(x,y)
0.41378247401164747 genre(x,y)
2.628029806773831 actor(x)
-16.5005204380062 director(x)
-0.08581939285300337 (genre(x,y) v workedUnder(z,x))
-0.15340724525668825 (!genre(x,y) v !workedUnder(z,x))
8.709712379643232 (!workedUnder(x,y) v director(y))

"""

    val extendedMlnStr = baseMlnStr +
      """
0 movie(m,a) v actor(a)
0 movie(m,a) v !actor(a)
0 !movie(m,a) v actor(a)
//
0 movie(m,a) v director(a)
0 movie(m,a) v !director(a)
0 !movie(m,a) v director(a)
//
0 !movie(m1,a) v !movie(m2,a)
//
0 actor(x) v director(x)
0 actor(x) v !director(x)
0 !actor(x) v director(x)
//       
0 male(x) v director(x)
0 male(x) v !director(x)
0 !male(x) v director(x)
//      
0 male(x) v actor(x)
0 male(x) v !actor(x)
0 !male(x) v actor(x)
//   
0 male(x) v actor(x) v director(x)
0 male(x) v actor(x) v !director(x)
0 male(x) v !actor(x) v director(x)
0 male(x) v !actor(x) v !director(x)
0 !male(x) v actor(x) v director(x)
0 !male(x) v actor(x) v !director(x)
0 !male(x) v !actor(x) v director(x)
0 !male(x) v !actor(x) v !director(x)
//  
0 workedUnder(x,x)
//
0 workedUnder(x,x) v male(x)
0 workedUnder(x,x) v !male(x)
0 !workedUnder(x,x) v male(x)
//
0 workedUnder(x,x) v director(x)
0 workedUnder(x,x) v !director(x)
0 !workedUnder(x,x) v director(x)
//
0 workedUnder(x,x) v actor(x)
0 workedUnder(x,x) v !actor(x)
0 !workedUnder(x,x) v actor(x)
//   
0 workedUnder(x,x) v actor(x) v director(x)
0 workedUnder(x,x) v actor(x) v !director(x)
0 workedUnder(x,x) v !actor(x) v director(x)
0 workedUnder(x,x) v !actor(x) v !director(x)
0 !workedUnder(x,x) v actor(x) v director(x)
0 !workedUnder(x,x) v actor(x) v !director(x)
0 !workedUnder(x,x) v !actor(x) v director(x)
0 !workedUnder(x,x) v !actor(x) v !director(x)
//   
0 workedUnder(x,x) v male(x) v director(x)
0 workedUnder(x,x) v male(x) v !director(x)
0 workedUnder(x,x) v !male(x) v director(x)
0 workedUnder(x,x) v !male(x) v !director(x)
0 !workedUnder(x,x) v male(x) v director(x)
0 !workedUnder(x,x) v male(x) v !director(x)
0 !workedUnder(x,x) v !male(x) v director(x)
0 !workedUnder(x,x) v !male(x) v !director(x)
//   
0 workedUnder(x,x) v male(x) v actor(x)
0 workedUnder(x,x) v male(x) v !actor(x)
0 workedUnder(x,x) v !male(x) v actor(x)
0 workedUnder(x,x) v !male(x) v !actor(x)
0 !workedUnder(x,x) v male(x) v actor(x)
0 !workedUnder(x,x) v male(x) v !actor(x)
0 !workedUnder(x,x) v !male(x) v actor(x)
0 !workedUnder(x,x) v !male(x) v !actor(x)
//
//0 workedUnder(x,x) v male(x) v director(x) v actor(x)
//0 workedUnder(x,x) v male(x) v !director(x) v actor(x)
//0 workedUnder(x,x) v !male(x) v director(x) v actor(x)
//0 workedUnder(x,x) v !male(x) v !director(x) v actor(x)
//0 !workedUnder(x,x) v male(x) v director(x) v actor(x)
//0 !workedUnder(x,x) v male(x) v !director(x) v actor(x)
//0 !workedUnder(x,x) v !male(x) v director(x) v actor(x)
//0 !workedUnder(x,x) v !male(x) v !director(x) v actor(x)
//0 workedUnder(x,x) v male(x) v director(x) v !actor(x)
//0 workedUnder(x,x) v male(x) v !director(x) v !actor(x)
//0 workedUnder(x,x) v !male(x) v director(x) v !actor(x)
//0 workedUnder(x,x) v !male(x) v !director(x) v !actor(x)
//0 !workedUnder(x,x) v male(x) v director(x) v !actor(x)
//0 !workedUnder(x,x) v male(x) v !director(x) v !actor(x)
//0 !workedUnder(x,x) v !male(x) v director(x) v !actor(x)
//0 !workedUnder(x,x) v !male(x) v !director(x) v !actor(x)
"""

    it("should learn parameters for the small MLN") {

      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln1 = parser.parseMLN(baseMlnStr)

      val db1 = parser.parseDB(db1String)
      val db2 = parser.parseDB(db2String)
      val db3 = parser.parseDB(db3String)
      val db4 = parser.parseDB(db4String)
      val db5 = parser.parseDB(db5String)

      val trainingSet = Seq(db1, db2, db4, db5)
      val testSet = Seq(db3);
      val learner = new LiftedLearning(mln1, trainingSet, verbose = false, normalizeLH = true, testdbMLNs = testSet)
      learner.learnParameters()
    }

    // test ignored because too expensive
    ignore("should learn parameters for the big MLN") {

      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln2 = parser.parseMLN(baseMlnStr + extendedMlnStr)

      val db1 = parser.parseDB(db1String)
      val db2 = parser.parseDB(db2String)
      val db3 = parser.parseDB(db3String)
      val db4 = parser.parseDB(db4String)
      val db5 = parser.parseDB(db5String)

      val trainingSet = Seq(db1, db2, db4, db5)
      val testSet = Seq(db3);
      val learner = new LiftedLearning(mln2, trainingSet, verbose = false, normalizeLH = true, testdbMLNs = testSet)
      learner.learnParameters()
    }
  }

}
