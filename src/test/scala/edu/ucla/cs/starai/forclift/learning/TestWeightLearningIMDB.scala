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
import org.scalatest._

import java.io._

import scala.io._
import edu.ucla.cs.starai.forclift.util.Resource
import org.scalatest.tagobjects.Slow

@RunWith(classOf[JUnitRunner])
class TestWeightLearningIMDB1 extends FunSpec with Matchers {

  describe("IMDB bug1") {

    val parser = new MLNParser
    val parsere = new MLNParser
    parser.setLearnModus(true)
    parsere.setLearnModus(true)
    // Smoking MLN
    val mlnString = Resource.fromFile("/imdb/imdb.bug1.mln").mkString
    // Database file for training
    val dbString = Resource.fromFile("/imdb/imdb.bug1.db").mkString

    var mln = MLN()
    var db = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("DB is parsable") {
      db = parser.parseDB(dbString)
    }

    it("Learnable") {
      val learner = new LiftedLearning(mln, Seq(db), verbose = false)
      val learnedMLN = learner.learnParameters()

      println(learnedMLN)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningIMDB2a extends FunSpec with Matchers {
  
  describe("IMDB bug2a") {

    val parser = new MLNParser
    val parsere = new MLNParser
    parser.setLearnModus(true)
    parsere.setLearnModus(true)
    // Smoking MLN
    val mlnString =
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

!workedUnder(v0,v1) v actor(v0) v genre(v1,v2)
"""
    // Database file for training
    val db1String = Resource.fromFile("/imdb/imdb_fold1.db").mkString
    val db2String = Resource.fromFile("/imdb/imdb_fold2.db").mkString
    val db3String = Resource.fromFile("/imdb/imdb_fold3.db").mkString
    val db4String = Resource.fromFile("/imdb/imdb_fold4.db").mkString
    val db5String = Resource.fromFile("/imdb/imdb_fold5.db").mkString

    var mln = MLN()
    var db1 = MLN()
    var db2 = MLN()
    var db3 = MLN()
    var db4 = MLN()
    var db5 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("DBs are parsable") {
      db1 = parser.parseDB(db1String)
      db2 = parser.parseDB(db2String)
      db3 = parser.parseDB(db3String)
      db4 = parser.parseDB(db4String)
      db5 = parser.parseDB(db5String)
    }

    it("DB MLNs can be converted into DB objects") {
      Databases.fromMLNs(mln, IndexedSeq(db1, db2, db3, db4))
    }

    it("Learnable without normalization", Slow) {
      val learner = new LiftedLearning(mln, Seq(db1, db2, db3, db4), verbose = false, normalizeLH = false)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningIMDB2b extends FunSpec with Matchers {
  describe("IMDB bug2b") {

    val parser = new MLNParser
    val parsere = new MLNParser
    parser.setLearnModus(true)
    parsere.setLearnModus(true)
    // Smoking MLN
    val mlnString =
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

!workedUnder(v0,v1) v actor(v0) v genre(v1,v2)
"""
    // Database file for training
    val db1String = Resource.fromFile("/imdb/imdb_fold1.db").mkString
    val db2String = Resource.fromFile("/imdb/imdb_fold2.db").mkString
    val db3String = Resource.fromFile("/imdb/imdb_fold3.db").mkString
    val db4String = Resource.fromFile("/imdb/imdb_fold4.db").mkString
    val db5String = Resource.fromFile("/imdb/imdb_fold5.db").mkString

    var mln = MLN()
    var db1 = MLN()
    var db2 = MLN()
    var db3 = MLN()
    var db4 = MLN()
    var db5 = MLN()

    mln = parser.parseMLN(mlnString)

    db1 = parser.parseDB(db1String)
    db2 = parser.parseDB(db2String)
    db3 = parser.parseDB(db3String)
    db4 = parser.parseDB(db4String)
    db5 = parser.parseDB(db5String)

    it("Learnable with normalization") {
      val learner = new LiftedLearning(mln, Seq(db1, db2, db3, db4), verbose = false, normalizeLH = true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningIMDB3 extends FunSpec with Matchers {
  describe("IMDB bug3") {

    val parser = new MLNParser
    parser.setLearnModus(true)

    // Smoking MLN
    val mlnString =
      """
//predicate declarations
workedUnder(person,person)
movie(film,person)
director(person)
male(person)
genre(person,type)
actor(person)

genre(a1,a2) v !genre(a1,a3)
"""
    // Database file for training
    val db1String = Resource.fromFile("/imdb/imdb_fold1.db").mkString
    val db2String = Resource.fromFile("/imdb/imdb_fold2.db").mkString
    val db3String = Resource.fromFile("/imdb/imdb_fold3.db").mkString
    val db4String = Resource.fromFile("/imdb/imdb_fold4.db").mkString
    val db5String = Resource.fromFile("/imdb/imdb_fold5.db").mkString

    var mln = MLN()
    var db1 = MLN()
    var db2 = MLN()
    var db3 = MLN()
    var db4 = MLN()
    var db5 = MLN()

    mln = parser.parseMLN(mlnString)

    db1 = parser.parseDB(db1String)
    db2 = parser.parseDB(db2String)
    db3 = parser.parseDB(db3String)
    db4 = parser.parseDB(db4String)
    db5 = parser.parseDB(db5String)

    it("Learnable with normalization") {
      val learner = new LiftedLearning(mln, Seq(db1, db2, db3, db4), verbose = false, normalizeLH = true)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

  describe("IMDB bug4") {

    val parser = new MLNParser
    parser.setLearnModus(true)

    // Smoking MLN
    val mlnString =
      """
//predicate declarations
workedUnder(person,person)
movie(film,person)
director(person)
male(person)
genre(person,type)
actor(person)

person={}
film={}
type={}

0.0916243  male(a1)
-2.23305   genre(a1,a2)
-0.987906  movie(a1,a2)
1.73093    actor(a1)
-0.590483  workedUnder(a1,a2)
-0.0172757 workedUnder(a1,a1)
-2.72023   director(a1)
1.0409     workedUnder(a1,a2) v !workedUnder(a1,a3) v director(a3)
"""
    // Database file for training
    val db5String = Resource.fromFile("/imdb/imdb_fold5.db").mkString

    var mln = parser.parseMLN(mlnString)
    var db5 = parser.parseDB(db5String)

    // ignored because too slow
    ignore("should be learnable") {
      val learner = new LiftedLearning(mln, Seq(db5), verbose = false, normalizeLH = false)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

}
