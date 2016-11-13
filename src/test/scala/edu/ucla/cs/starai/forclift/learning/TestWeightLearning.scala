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

import java.io._

import scala.io._

import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TestWeightLearning extends FunSpec with Matchers {

  println("Running from directory:")
  println(System.getProperty("user.dir"))

  val acc = 0.01

  //--------------------------------------------------------------------------
  describe("UWCSE") {

    var structure1 = MLN()
    var structure2 = MLN()
    var trdb1 = MLN()
    var trdb2 = MLN()
    var tedb2 = MLN()
    var formprob = 0.0
    var trainlll = 0.0
    val parser1 = new MLNParser
    parser1.isLearnModus = true
    val parser2 = new MLNParser
    parser2.isLearnModus = true

    val structureStr =
      """
// Predicate declarations
Advisedby(person,person)
Taughtby(course,person,year)
Tempadvisedby(person,person)

// Clauses
0.819459      !Advisedby(a1,a2)   v !Tempadvisedby(a1,a3)
"""

    val trainingDBStr =
      """
//Taughtby(Course11_4,Person57_1,Autumn_0001_6)
//Taughtby(Course147_4,Person201_1,Autumn_0001_6)
//Taughtby(Course77_4,Person165_1,Autumn_0001_6)
//Taughtby(Course160_4,Person331_1,Autumn_0001_6)
//Taughtby(Course66_4,Person298_1,Autumn_0001_6)
//Taughtby(Course11_4,Person298_1,Winter_0001_6)
//Taughtby(Course147_4,Person165_1,Winter_0001_6)
//Taughtby(Course165_4,Person364_1,Winter_0001_6)
//Taughtby(Course161_4,Person201_1,Winter_0001_6)
//Taughtby(Course68_4,Person331_1,Winter_0001_6)
Taughtby(Course137,Person165_1,Winter_0304_6)
Advisedby(Person309_1,Person378_1)
Tempadvisedby(Person383_1,Person165_1)
"""

    val testDBStr =
      """
//Taughtby(Course11_4,Person57_1,Autumn_0001_6)
//Taughtby(Course147_4,Person201_1,Autumn_0001_6)
Taughtby(Course77_4,Person165_1,Autumn_0001_6)
//Taughtby(Course160_4,Person331_1,Autumn_0001_6)
//Taughtby(Course66_4,Person298_1,Autumn_0001_6)
Taughtby(Course11_4,Person298_1,Winter_0001_6)
//Taughtby(Course147_4,Person165_1,Winter_0001_6)
Taughtby(Course165_4,Person364_1,Winter_0001_6)
//Taughtby(Course161_4,Person201_1,Winter_0001_6)
//Taughtby(Course68_4,Person331_1,Winter_0001_6)
//Taughtby(Course137,Person165_1,Winter_0304_6)
Advisedby(Person309_1,Person378_1)
Tempadvisedby(Person383_1,Person165_1)
"""

    it("Structure and training db are parsable") {
      structure1 = parser1.parseMLN(structureStr)
      trdb1 = parser1.parseDB(trainingDBStr)
    }

    it("Structure, training and test db are parsable") {
      structure2 = parser2.parseMLN(structureStr)
      trdb2 = parser2.parseDB(trainingDBStr)
      tedb2 = parser2.parseDB(testDBStr)
    }

    it("Learnable with only training db") {
      val learner = new LiftedLearning(structure1, Seq(trdb1), verbose = false)
      val learnedMLN = learner.learnParameters()
      learnedMLN._1.wformulas.length should be(4)

      println(learnedMLN)

      val complexform = learnedMLN._1.wformulas.find {
        _.formula match {
          case f: DisjFormula => true
          case _ => false
        }
      }
      complexform.isEmpty should be(false)

      formprob = complexform.get.weight
      println("Formula weight = " + formprob)

      trainlll = learner.trainDatabaseLikelihoods.map(_.likelihood).reduce{_ * _}.logToDouble
    }

    // ignored because we are not sure if this test should succeed or fail...
    // TODO for Wannes
    it("Learnable with training and test") {
      val learner = new LiftedLearning(structure2, Seq(trdb2), verbose = false, testdbMLNs = Seq(tedb2))
      //val learner = new LiftedLearning(structure2, Seq(trdb2, tedb2), verbose=true)
      val learnedMLN = learner.learnParameters()

      println(learnedMLN)

      val complexform = learnedMLN._1.wformulas.find {
        _.formula match {
          case f: DisjFormula => true
          case _ => false
        }
      }
      complexform.isEmpty should be(false)

      val nformprob = complexform.get.weight
      nformprob should be(formprob +- acc)

      learner.trainDatabaseLikelihoods.map(_.likelihood).reduce{_ * _}.logToDouble should be(trainlll +- acc)
    }
  }
}

//--------------------------------------------------------------------------

@RunWith(classOf[JUnitRunner])
class TestWeightLearningSmoking extends FunSpec with Matchers {

  /*
     * Test removed because of missing files.
     */

  //    describe("Smoking example") {
  //
  //        val parser = new MLNParser
  //        val parsere = new MLNParser
  //        parser.setLearnModus(true)
  //        parsere.setLearnModus(true)
  //        // Smoking MLN
  //        val mlnFile = new File("./src/test/scala/liftedinference/learning/smoking/smoking.mln")
  //        val mlnString = Source.fromFile(mlnFile).mkString
  //        // Smoking MLN with symmetry
  //        val mlneFile = new File("./src/test/scala/liftedinference/learning/smoking/smoking-symmetric.mln")
  //        val mlneString = Source.fromFile(mlneFile).mkString
  //        // Database file for training
  //        val dbFile = new File("./src/test/scala/liftedinference/learning/smoking/smoking-train.db")
  //        val dbString = Source.fromFile(dbFile).mkString
  //
  //        var mln  = MLN()
  //        var mlne = MLN()
  //        var db   = MLN()
  //
  //        it("MLN is parsable") {
  //            //println("Testing MLN:\n"+mlnString)
  //            mln = parser.parseMLN(mlnString)
  //            //println(mln)
  //        }
  //
  //        it("MLN has 2 lines") {
  //            mln.wformulas.length should be (2)
  //        }
  //
  //        it("DB is parsable") {
  //            db = parser.parseDB(dbString)
  //        }
  //
  //        it("DB has 22 facts") {
  //            db.evidence.length should be (22)
  //        }
  //
  //        it("Learnable") {
  //            val learner = new LiftedLearning(mln, Seq(db), verbose=true)
  //            val learnedMLN = learner.learnParameters()
  //
  //            println(learnedMLN)
  //        }
  //
  //        it("Extended MLN with symmetry is parsable") {
  //            //println("Testing MLN:\n"+mlneString)
  //            mlne = parsere.parseMLN(mlneString)
  //            //println(mlne)
  //        }
  //
  //        it("Extended MLN with symmetry has 3 lines") {
  //            mlne.wformulas.length should be (3)
  //        }
  //
  //        it("DB with extended is parsable") {
  //            db = parsere.parseDB(dbString)
  //        }
  //
  //        it("DB with extended has 22 facts") {
  //            db.evidence.length should be (22)
  //        }
  //
  //        it("Extended learnable") {
  //            val learner = new LiftedLearning(mlne, Seq(db), verbose=true)
  //            val learnedMLN = learner.learnParameters()
  //
  //            println(learnedMLN)
  //        }
  //    }
}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningYeast extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  describe("Yeast example (with equality)") {

    var structure = MLN()
    var db = MLN()
    val parser = new MLNParser
    parser.isLearnModus = true

    val structureStr =
      """
//predicate declarations
location(protein,location_id)
interaction(protein,protein)
protein_class(protein,pc_id)
enzyme(protein,enzyme_id)
function(protein,func_id)
complex(protein,complex_id)
phenotype(protein,phenotype_id)
//eq_protein(protein,protein)

// Hard clauses are not supported for learning
//eq_protein(a1,a1).
//!eq_protein(a1,a2). , a1 != a2

// TODO: Lifted Learning does not allow unit clauses in theory. Ignore instead of Exception?
//-3.03056   location(a1,a2)
//-5.44151   protein_class(a1,a2)
-5.14236   interaction(a1,a2)
//-5.38991   complex(a1,a2)
//-5.05341   phenotype(a1,a2)
//-5.61053   enzyme(a1,a2)
//2.00798    !function(a1,a2) v !function(a1,Func_id_99)
//0.359595   function(a1,Func_id_40) v !function(a1,a2)
//-0.287028  !interaction(a1,a3) v !interaction(a2,a1) v !interaction(a2,a3) v eq_protein(a1,a2)
-0.287028  !interaction(a1,a3) v !interaction(a2,a1) v !interaction(a2,a3) v a1=a2
//-0.179333  function(a1,Func_id_32005) v function(a1,Func_id_40010002002) v !function(a1,a2) v !phenotype(a1,a3)
//0.646094   function(a1,Func_id_16) v !function(a1,a2) v !function(a1,Func_id_1001003) v !phenotype(a1,a3)
"""

    it("Structure is parsable") {
      //println("Testing MLN:\n"+mlnString)
      structure = parser.parseMLN(structureStr)
      println("Structure:")
      println(structure)
      println
    }

    val trainingDBStr =
      """
interaction(Protein_YOR167c,Protein_YBL026w)
//interaction(Protein_YOR167c,Protein_YJR022w)
//interaction(Protein_YLR264w,Protein_YBL026w)
//interaction(Protein_YLR264w,Protein_YJR022w)
complex(Protein_YBR195c,Complex_id_90010)
//complex(Protein_YBR288c,Complex_id_260020030)
//complex(Protein_YBR289w,Complex_id_510190050)
//complex(Protein_YCR035c,Complex_id_440012010)
enzyme(Protein_YAR003w,Enzyme_id_2001001043)
//enzyme(Protein_YAR018c,Enzyme_id_2007001037)
//enzyme(Protein_YBR003w,Enzyme_id_2005001030)
//enzyme(Protein_YBR034c,Enzyme_id_2001001023)
//enzyme(Protein_YBR059c,Enzyme_id_2007001037)
function(Protein_YAL013w,Func_id_1)
//function(Protein_YAL013w,Func_id_11002)
//function(Protein_YAR003w,Func_id_10001009)
//function(Protein_YAR003w,Func_id_11002)
//function(Protein_YAR003w,Func_id_14)
//function(Protein_YAR003w,Func_id_42)
location(Protein_YAL013w,Location_id_725)
//location(Protein_YAR003w,Location_id_750)
//location(Protein_YBL026w,Location_id_725)
//location(Protein_YBL026w,Location_id_750)
//location(Protein_YBL026w,Location_id_750003)
protein_class(Protein_YAR018c,Pc_id_161041051)
//protein_class(Protein_YBR059c,Pc_id_161041041)
phenotype(Protein_YAL013w,Phenotype_id_12015)
//phenotype(Protein_YAL013w,Phenotype_id_32005)
//phenotype(Protein_YAL013w,Phenotype_id_42020)
//phenotype(Protein_YAL013w,Phenotype_id_52099)

"""

    it("Database is parsable") {
      db = parser.parseDB(trainingDBStr)
      println("DB:")
      println("Parsed db:")
      println(db.evidence)
      println("with domains:")
      println(db.domainSizes)
      println
      println
    }

    // Test removed because it is not learnable, because it contains a transitive clause.
    //        it("Learnable") {
    //            val learner = new LiftedLearning(structure, Seq(db), verbose=true)
    //            val learnedMLN = learner.learnParameters()
    //
    //            println(learnedMLN)
    //        }
  }
}
