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

import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestWeightLearningUWCSEnoformulas extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  describe("UWCSE without formulas (all unit clauses)") {

    var structure = MLN()
    var db = MLN()
    val parser = new MLNParser
    parser.isLearnModus = true

    val structureStr =
      """
Position(person,position)
Advisedby(person,person)
Taughtby(course,person,year)
Publication(title,person)
Projectmember(project,person)
Professor(person)
Courselevel(course,level)
Phase(person,phase)
Ta(course,person,year)
Yearsinprogram(person,num)
Tempadvisedby(person,person)
Student(person)

0  Courselevel(a1,a2)
0   Student(a1)
0   Publication(a1,a2)
0   Advisedby(a1,a2)
0   Phase(a1,a2)
0   Yearsinprogram(a1,a2)
0   Taughtby(a1,a2,a3)
0    Professor(a1)
0   Tempadvisedby(a1,a2)
0   Projectmember(a1,a2)
0   Position(a1,a2)
0   Ta(a1,a2,a3)
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
Taughtby(Course147_4,Person201_1,Autumn_0001_6)
Taughtby(Course147_4,Person324_1,Winter_0102_6)
Taughtby(Course68_4,Person201_1,Winter_0102_6)
Taughtby(Course11_4,Person324_1,Spring_0102_6)
Taughtby(Course11_4,Person324_1,Spring_0203_6)
Taughtby(Course161_4,Person201_1,Winter_0304_6)
Taughtby(Course68_4,Person324_1,Winter_0304_6)
Taughtby(Course97_4,Person324_1,Spring_0304_6)
Courselevel(Course11_4,Level_300_7)
Courselevel(Course147_4,Level_300_7)
Courselevel(Course104_4,Level_300_7)
Courselevel(Course68_4,Level_400_7)
Courselevel(Course161_4,Level_400_7)
Courselevel(Course97_4,Level_400_7)
Courselevel(Course84_4,Level_500_7)
Position(Person378_1,Faculty_8)
Position(Person201_1,Faculty_8)
Position(Person324_1,Faculty_8)
Projectmember(Project130_5,Person324_1)
Projectmember(Project119_5,Person201_1)
Projectmember(Project152_5,Person201_1)
Projectmember(Project94_5,Person324_1)
Advisedby(Person68_1,Person201_1)
Phase(Person191_1,Post_quals_9)
Tempadvisedby(Person205_1,Person324_1)
Tempadvisedby(Person182_1,Person201_1)
Yearsinprogram(Person191_1,4)
Ta(Course161_4,Person191_1,Winter_0304_6)
Ta(Course104_4,Person191_1,Spring_0102_6)
Ta(Course68_4,Person191_1,Winter_0102_6)
Taughtby(Course122,Person378_1,Spring_0203_6)
Taughtby(Course84_4,Person324_1,Winter_0203_6)
Professor(Person378_1)
Professor(Person201_1)
Professor(Person324_1)
Student(Person191_1)
Student(Person397_1)
Student(Person138_1)
Publication(Title164_3,Person378_1)
Publication(Title202_3,Person378_1)
Publication(Title152_3,Person378_1)
Publication(Title154_3,Person378_1)
Publication(Title334_3,Person378_1)
Publication(Title193_3,Person378_1)
Publication(Title326_3,Person378_1)
Publication(Title328_3,Person378_1)
Publication(Title327_3,Person378_1)
Publication(Title308_3,Person378_1)
Publication(Title136_3,Person378_1)
Publication(Title243_3,Person378_1)
Publication(Title127_3,Person378_1)
Publication(Title326_3,Person397_1)
Publication(Title62_3,Person138_1)
Publication(Title210_3,Person138_1)
Publication(Title287_3,Person138_1)
Publication(Title62_3,Person324_1)
Publication(Title158_3,Person324_1)
Publication(Title19_3,Person324_1)
Publication(Title210_3,Person324_1)
Publication(Title21_3,Person324_1)
Publication(Title27_3,Person324_1)
Publication(Title105_3,Person324_1)
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

    it("Learnable") {
      val learner = new LiftedLearning(structure, Seq(db), verbose = false)
      val learnedMLN = learner.learnParameters()

      println(learnedMLN)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningUWCSEstudprof extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  describe("UWCSE with stud and prof (all unit clauses)") {

    var structure = MLN()
    var db = MLN()
    val parser = new MLNParser
    parser.isLearnModus = true

    val structureStr =
      """
Professor(person)
Student(person)

0   Student(a1)
0    Professor(a1)
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
Professor(Person378_1)
Professor(Person201_1)
Professor(Person324_1)
Professor(Person191_1)
Student(Person397_1)
Student(Person138_1)
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

    it("Learnable") {
      val learner = new LiftedLearning(structure, Seq(db), verbose = false)
      val learnedMLN = learner.learnParameters()

      println(learnedMLN)
    }
  }

}

@RunWith(classOf[JUnitRunner])
class TestWeightLearningUWCSEBugs extends FunSpec with Matchers {

  describe("UWCSE bug1") {

    val parser = new MLNParser
    parser.setLearnModus(true)

    // Smoking MLN
    val mlnString =
      """
Taughtby(course,person,year)
Advisedby(person,person)
Publication(title,person)
Professor(person)
Courselevel(course,level)
Phase(person,phase)
Ta(course,person,year)
Yearsinprogram(person,num)
Tempadvisedby(person,person)

0 Tempadvisedby(a1,a2)
0 Taughtby(a1,a2,a3)
0 Yearsinprogram(a1,a2)
0 Advisedby(a1,a2)
0 Ta(a1,a2,a3)
0 Courselevel(a1,a2)
0 Publication(a1,a2)
0 Phase(a1,a2)
0 Advisedby(a1,a1)
0 Professor(a1)
0 Tempadvisedby(a1,a1)

0 !Taughtby(a1,a2,a3) v Professor(a2)
0 !Professor(a1) v !Yearsinprogram(a1,a2)
0 !Advisedby(a1,a2) v Professor(a2)
0 !Professor(a1) v !Phase(a1,a2)
0 Advisedby(a1,a2) v !Tempadvisedby(a1,a2)
0 Courselevel(a1,a2) v Courselevel(a1,a3)
0 !Professor(a1) v !Ta(a2,a1,a3)
0 !Professor(a1) v Phase(a1,a2) v !Yearsinprogram(a1,a3)
"""
    // Database file for training
    val db1String = Resource.fromFile("/uwcse/uwcse_fold1.db").mkString
    val db2String = Resource.fromFile("/uwcse/uwcse_fold2.db").mkString
    val db3String = Resource.fromFile("/uwcse/uwcse_fold3.db").mkString
    val db4String = Resource.fromFile("/uwcse/uwcse_fold4.db").mkString
    val db5String = Resource.fromFile("/uwcse/uwcse_fold5.db").mkString

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

    it("is learnable") {
      val learner = new LiftedLearning(mln, Seq(db1, db2, db3, db5), verbose = true, normalizeLH = false)
      val learnedMLN = learner.learnParameters()
      println(learnedMLN)
    }
  }

}
