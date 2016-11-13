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
class TestWeightLearningUWCSEMonotone2 extends FunSpec with Matchers {

  describe("UWCSE bug Jan Short") {

    val parser = new MLNParser
    parser.setLearnModus(true)

    val mln2String =
      """
Publication(title,person)
Phase(person,phase)
Yearsinprogram(person,num)
Advisedby(person,person)
Ta(course,person,year)
Courselevel(course,level)
Taughtby(course,person,year)
Professor(person)
Tempadvisedby(person,person)

0 Ta(x,y,z)
0 Phase(x,y)
0 Yearsinprogram(x,y)
0 Taughtby(x,y,z)
0 Courselevel(x,y)
0 Advisedby(x,y)
0 Publication(x,y)
0 Tempadvisedby(x,y)
0 Professor(x)
//0 (!Courselevel(x,y) v !Courselevel(x,z))
0 (!Publication(x,y) v !Publication(z,y))
"""
    // Database file for training
    val db1String = Resource.fromFile("/uwcse/uwcse_fold1.db").mkString
    val db2String = Resource.fromFile("/uwcse/uwcse_fold2.db").mkString
    val db3String = Resource.fromFile("/uwcse/uwcse_fold3.db").mkString
    val db4String = Resource.fromFile("/uwcse/uwcse_fold4.db").mkString
    val db5String = Resource.fromFile("/uwcse/uwcse_fold5.db").mkString

    var mln1 = MLN()
    var mln2 = MLN()
    var db1 = MLN()
    var db2 = MLN()
    var db3 = MLN()
    var db4 = MLN()
    var db5 = MLN()

    mln1 = parser.parseMLN(mln2String)
//    mln2 = parser.parseMLN(mln2String)

    db1 = parser.parseDB(db1String)
    db2 = parser.parseDB(db2String)
    db3 = parser.parseDB(db3String)
    db4 = parser.parseDB(db4String)
    db5 = parser.parseDB(db5String)

    it("Small MLN is learnable") {
      val learner = new LiftedLearning(mln1, Seq(db1, db2, db3), testdbMLNs=Seq(db5), 
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
