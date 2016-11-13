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
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._
import edu.ucla.cs.starai.forclift.languages.mln._

@RunWith(classOf[JUnitRunner])
class TestWebkbBugs extends FunSpec with Matchers with ResourceParseHelper{

  //--------------------------------------------------------------------------
  println("Running from directory:")
  println(System.getProperty("user.dir"))
  var mln  = MLN()
  var db   = MLN()

  //--------------------------------------------------------------------------

  describe("Webkb bug1") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("/webkb/webkb_bug1.mln",
                             "/webkb/corn20_bug1.db")
      mln = mln2
      db = db2
      println(mln)
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

  describe("Webkb bug2") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("/webkb/webkb_bug2.mln",
                             "/webkb/corn20_bug2.db")
      mln = mln2
      db = db2
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

  describe("Webkb bug3") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("/webkb/webkb_bug3.mln",
                             "/webkb/corn20.db")
      mln = mln2
      db = db2
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

  describe("Webkb bug4") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("/webkb/webkb_bug4.mln",
                             "/webkb/corn20.db")
      mln = mln2
      db = db2
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

  describe("Webkb bug5") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("/webkb/webkb_bug5.mln",
                             "/webkb/corn20.db")
      mln = mln2
      db = db2
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose=true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }
}
