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
import org.scalatest.BeforeAndAfter

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import scala.io._

@RunWith(classOf[JUnitRunner])
class BenchmarkWebkbBugs extends FunSpec with Matchers {

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
    return (mln,db)
  }
  //--------------------------------------------------------------------------

  describe("Webkb bug1") {

    it("MLN and DB are parsable") {
      val (mln2,db2) = parse("./src/benchmark/scala/liftedinference/benchmark/learning/webkb/webkb_bug1.mln",
                             "./src/benchmark/scala/liftedinference/benchmark/learning/webkb/corn20_bug1.db")
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
      val (mln2,db2) = parse("./src/benchmark/scala/liftedinference/benchmark/learning/webkb/webkb_bug2.mln",
                             "./src/benchmark/scala/liftedinference/benchmark/learning/webkb/corn20_bug2.db")
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
      val (mln2,db2) = parse("./src/benchmark/scala/liftedinference/benchmark/learning/webkb/webkb_bug3.mln",
                             "./src/benchmark/scala/liftedinference/benchmark/learning/webkb/corn20.db")
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
      val (mln2,db2) = parse("./src/benchmark/scala/liftedinference/benchmark/learning/webkb/webkb_bug4.mln",
                             "./src/benchmark/scala/liftedinference/benchmark/learning/webkb/corn20.db")
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
      val (mln2,db2) = parse("./src/benchmark/scala/liftedinference/benchmark/learning/webkb/webkb_bug5.mln",
                             "./src/benchmark/scala/liftedinference/benchmark/learning/webkb/corn20.db")
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

