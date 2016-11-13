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

import org.junit.runner.RunWith
import org.scalatest.Matchers

import java.io._

import scala.io._

import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestWeightLearningWebKB2Monotone extends FunSpec with Matchers {

  describe("WebKB2 bug Jan") {

    val parser = new MLNParser
    parser.setLearnModus(true)

    val mln1String =
      """

project(task,person)
courseTA(course,person)
student(person)
courseProf(course,person)
faculty(person)

-6.422947152786691 project(x,y)
-5.695767167916951 courseTA(x,y)
1.4831259809182646 faculty(x)
-11.139728791062351 courseProf(x,y)
3.7367248437726714 student(x)
1.838195270720632 ((!courseTA(x,y) v courseProf(z,y)) v student(y))
-0.054950476803692076 ((!project(x,y) v !student(y)) v courseProf(z,y))
0.5008872927556092 ((!faculty(x) v !student(x)) v courseProf(y,x))
-0.041883127014925955 ((!project(x,y) v faculty(y)) v project(z,y))
-3.1872644041834737 (!project(x,y) v student(y))
-7.395636151029688 ((!courseProf(x,y) v courseTA(x,y)) v student(y))
1.597474249223895 ((!courseProf(x,y) v faculty(y)) v student(y))

"""
    val mln2String =
      """

project(task,person)
courseTA(course,person)
student(person)
courseProf(course,person)
faculty(person)

-6.382050640574264 project(x,y)
-5.704564912287659 courseTA(x,y)
1.2076557998442516 faculty(x)
-11.24823353231588 courseProf(x,y)
3.4313874803679316 student(x)
0.332429990911885 ((!courseTA(x,y) v courseProf(z,y)) v student(y))
-0.05797672237840561 ((!project(x,y) v !student(y)) v courseProf(z,y))
-0.032766778435851684 ((!project(x,y) v faculty(y)) v project(z,y))
1.6376765954375099 ((!faculty(x) v !student(x)) v courseProf(y,x))
-3.1611671063568094 (!project(x,y) v student(y))
-7.256931187083652 ((!courseProf(x,y) v courseTA(x,y)) v student(y))
1.3960108536694396 ((!courseProf(x,y) v faculty(y)) v student(y))
-0.08649267695334978 (!courseProf(x,y) v !courseProf(z,y))

"""
    // Database file for training
    val db1String = Resource.fromFile("/webkb2/fold1.db").mkString
    val db2String = Resource.fromFile("/webkb2/fold2.db").mkString
    val db3String = Resource.fromFile("/webkb2/fold3.db").mkString
    val db4String = Resource.fromFile("/webkb2/fold4.db").mkString
  
    var mln1 = MLN()
    var mln2 = MLN()
    var db1 = MLN()
    var db2 = MLN()
    var db3 = MLN()
    var db4 = MLN()

    mln1 = parser.parseMLN(mln2String)
//    mln2 = parser.parseMLN(mln2String)

    db1 = parser.parseDB(db1String)
    db2 = parser.parseDB(db2String)
    db3 = parser.parseDB(db3String)
    db4 = parser.parseDB(db4String)

    // this test runs for hours (sometimes) -- too slow
    ignore("Small MLN is learnable") {
      val learner = new LiftedLearning(mln1, Seq(db1, db3, db4), testdbMLNs=Seq(db2), 
    		  							verbose = true, normalizeLH = false)
      val (learnedMLN,ll) = learner.learnParameters()
      println(learnedMLN)
      for(dbLh <- learner.testDatabaseLikelihoods){
        println("Test DBs")
	      println(s"db: $dbLh")
	      println(s"Z = ${dbLh.z.cachedWmc.logToDouble}")
	      println(s"loglikelihood = ${dbLh.likelihood.logToDouble}")
      }
    }
  }

}
