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

package edu.ucla.cs.starai.forclift

import org.scalatest.FunSpec
import org.scalatest.Matchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift.languages.mln._
import edu.ucla.cs.starai.forclift.util.Resource

@RunWith(classOf[JUnitRunner])
class TestMLNParser extends FunSpec with Matchers {

  //println("Running from directory:")
  //println(System.getProperty("user.dir"))

  //--------------------------------------------------------------------------
  describe("Comments") {

    val parser = new MLNParser
    val mlnString =
      """
// Line

// Lines
// Lines

/* Line block */

/*
Clean lines block
*/

/**
 * Javadoc comment block
 */


/************/
/* Overdone */
/************/

/* Nested /* block */ */

// Domain
person = {Guy, Nima /* Wannes, Jesse, Luc */ }
 """
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have zero clauses") {
      mln.wformulas.length should be(0)
    }
  }

  //--------------------------------------------------------------------------

  describe("Weights") {

    val parser = new MLNParser
    val mlnString =
      """
// Domain
person = {Guy, Nima, Wannes, Jesse, Luc}

// Predicates
test(person)
// Non standard syntax to merge MLN syntax and WMC syntax
directweight 0.5 1

// Theory
1 test(x)
1.5 test(x)
-1 test(x)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
    }

    it("MLN should have three clause") {
      mln.wformulas.length should be(3)
    }

    it("MLN formula weights should be correct") {
      val weights = 1 :: 1.5 :: -1 :: Nil
      val formandweights = mln.wformulas.zip(weights)
      formandweights.forall {
        _ match {
          case (f, w) => (f.weight == w)
          case _ => false
        }
      } should be(true)
    }

    it("MLN directly set weights should be correct") {
      mln.weights.forall {
        _ match {
          case (p, w) => (w.posWDouble == 0.5 && w.negWDouble == 1.0)
          case _ => false
        }
      } should be(true)
    }
  }

  //--------------------------------------------------------------------------

  describe("Constants and Variables") {

    val parser = new MLNParser
    parser.isLearnModus = true
    val mlnString =
      """
// Domain
person = {"Guy Vdb", "Nima T", Wannes, "Jesse D", "Luc DR"}
// Domain with explicit size
places = 10 {}

// Predicates
test(person)

// Theory
1 test(x)
"""

    val trainingDBStr =
      """
test("Some more variables, no?")
"""

    var mln = MLN()
    var db = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
      //println(mln.toStringExt)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("DB is parsable") {
      db = parser.parseDB(trainingDBStr)
      //println("DB:")
      //println(db.toStringExt)
    }

    it("DB length") {
      db.evidence.length should be(1)
    }

    it("Variables are lowercase") {
      val mlnstr = mln.toStringExt
      mlnstr should include("x")
      mlnstr should include("Wannes")
    }
  }

  //--------------------------------------------------------------------------

  describe("No last empty line") {

    val parser = new MLNParser
    parser.isLearnModus = true
    val mlnString =
      """
// Domain
person = {"a", "b"}

// Predicates
test(person)

// Theory
1 test(x)"""

    var mln = MLN()
    var db = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
      //println(mln.toStringExt)
    }
  }

  //--------------------------------------------------------------------------
  describe("Include statement") {

    val parser = new MLNParser
    val mlnString =
      """
#include ./src/test/resources/mlns/TestMLNIncludeStatement.mln
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have three clause") {
      mln.wformulas.length should be(3)
    }

    val weights = 1 :: 1.5 :: -1 :: Nil
    val formandweights = mln.wformulas.zip(weights)
    it("MLN weights should be correct") {
      formandweights.forall {
        _ match {
          case (f, w) => (f.weight == w)
          case _ => false
        }
      } should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Disjunction") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
1 test(x) v test(y) v test(z)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should not be empty") {
      mln.wformulas.isEmpty should be(false)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("Formula should be a left-associative disjunction with three disjuncts") {
      (mln.wformulas.head.formula match {
        case DisjFormula(DisjFormula(_, _), _) => true
        case _ => false
      }) should be(true)
    }

    it("Correct syntax") {
      val mlnstr = mln.toStringExt
      mlnstr should include(" v ")
    }
  }

  //--------------------------------------------------------------------------
  describe("Conjunction") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
1 test(x) ^ test(y) ^ test(z)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should not be empty") {
      mln.wformulas.isEmpty should be(false)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("Formula should be a left-associative conjunction with three conjuncts") {
      (mln.wformulas.head.formula match {
        case ConjFormula(ConjFormula(_, _), _) => true
        case _ => false
      }) should be(true)
    }

    it("Correct syntax") {
      val mlnstr = mln.toStringExt
      mlnstr should include(" ^ ")
    }
  }

  //--------------------------------------------------------------------------
  // Left-associativity:
  // a => b => c  ==  a => (b => c)
  describe("Implication") {

    val parser = new MLNParser
    val mlnString =
      """
p
q
r
1 p => q => r
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause") {
      mln.wformulas.isEmpty should be(false)
      mln.wformulas.length should be(1)
    }

    it("Formula should be a left-associative set of implications") {
      mln.wformulas.length should be(1)
      (mln.wformulas.head.formula match {
        case f1: ImplFormula => {
          f1.condition match {
            case f2: ImplFormula => true
            case _ => false
          }
        }
        case _ => false
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Simple implication") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy,Nima}
Smokes(person)
Cancer(person)

0.0 Smokes(x) => Cancer(x)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause") {
      mln.wformulas.isEmpty should be(false)
      mln.wformulas.length should be(1)
    }

    it("CNF") {
      val mln_wmc = mln.toWeightedCNF()
      //println(mln_wmc)
      mln_wmc.cnf.clauses.length should be(3)
    }

  }

  //--------------------------------------------------------------------------
  describe("Hard clauses") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
test(x) => test(y) => test(z).
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("Clause should be hard clause") {
      mln.wformulas.head.isHard should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("To be learned clauses (exception)") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
test(x) => test(y) => test(z)
"""
    var mln = MLN()

    it("MLN parsing throws exception") {
      intercept[IllegalStateException] {
        //println("Testing MLN:\n"+mlnString)
        mln = parser.parseMLN(mlnString)
        //println(mln)
      }
    }

    it("MLN should have no clauses") {
      mln.wformulas.isEmpty should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("To be learned clauses (parsed)") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
test(x) => test(y) => test(z)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have no clauses") {
      mln.wformulas.length should be(1)
    }
  }

  //--------------------------------------------------------------------------
  describe("Learning with database") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
friends(person,person)
smokes(person)
friends(x,y) ^ smokes(x) => smokes(y)
"""
    val dbString =
      """
friends(Guy,Nima)
friends(Nima,Wannes)
friends(Jesse,Luc)
smokes(Wannes)
smokes(Nima)
"""
    var mln = MLN()
    var ev = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      //println("With DB:\n"+dbString)
      mln = parser.parseMLN(mlnString)
      ev = parser.parseDB(dbString)
      //println(mln.toStringExt)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("Evidence should contain 5 atoms") {
      ev.evidence.length should be(5)
    }
  }

  //--------------------------------------------------------------------------
  describe("With database and unknown constants") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnString =
      """
person = {}
friends(person,person)
smokes(person)
friends(x,y) ^ smokes(x) => smokes(y)
"""
    val dbString =
      """
friends(Guy,Nima)
friends(Nima,Wannes)
friends(Jesse,Luc)
smokes(Wannes)
smokes(Nima)
"""
    var mln = MLN()
    var ev = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      //println("With DB:\n"+dbString)
      mln = parser.parseMLN(mlnString)
      ev = parser.parseDB(dbString)
      //println(mln.toStringExt)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("Evidence should contain 5 atoms") {
      ev.evidence.length should be(5)
    }

    it("Person domain should have size 5") {
      // TODO
    }
  }

  //--------------------------------------------------------------------------
  describe("With multiple databases and unknown constants") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnString =
      """
person = {}
friends(person,person)
smokes(person)
friends(x,y) ^ smokes(x) => smokes(y)
"""
    val dbString1 =
      """
friends(Guy,Nima)
friends(Nima,Wannes)
friends(Jesse,Luc)
smokes(Wannes)
smokes(Nima)
"""
    val dbString2 =
      """
friends(Luc,Nima)
friends(Nima,Wannes)
friends(Jesse,Luc)
smokes(Jesse)
smokes(OtherPerson)
"""
    var mln = MLN()
    var ev1 = MLN()
    var ev2 = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(mlnString)
    }

    it("Evidence db 1 is parsable") {
      ev1 = parser.parseDB(dbString1)
    }

    it("Evidence db 2 is parsable") {
      ev2 = parser.parseDB(dbString2)
    }
  }

  //--------------------------------------------------------------------------
  describe("Exists clauses") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
test(person)
1 exist x test(x)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should expand to clause with 5 disjuncts") {
      var mln_na = mln.toMLNasExistFree
      //println("Non-Alchemy MLN:")
      //println(mln_na)
      // Should result in:
      // 1 Test(Guy) v Test(Nima) v Test(Wannes) v Test(Jesse) v Test(Luc)
      mln_na.wformulas.length should be(1)
      (mln_na.wformulas.exists { f =>
        f.weight == 1 &&
          (f.formula match {
            case DisjFormula(DisjFormula(DisjFormula(DisjFormula(LiteralFormula(_, true), LiteralFormula(_, true)), LiteralFormula(_, true)), LiteralFormula(_, true)), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Huth&Ryan example formula to CNF") {

    val parser = new MLNParser
    val mlnString = """
p
q
r
2 !p ^ q => p ^ (r => q)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }
    it("MLN clause should be correctly parsed") {
      (mln.wformulas.head.formula match {
        case ImplFormula(ConjFormula(NegFormula(_), _), ConjFormula(_, ImplFormula(_, _))) => true
        case _ => false
      }) should be(true)
    }

    it("MLN to Implication Free") {
      // Should be
      // !(!p ^ q) v (p ^ (!r v q))
      val mln_implfree = mln.toMLNasIMPLFREE
      //println("MLN Impl_free:")
      //println(mln_implfree)
      (mln_implfree.wformulas.head.formula match {
        case DisjFormula(NegFormula(ConjFormula(NegFormula(_), _)), ConjFormula(_, DisjFormula(NegFormula(_), _))) => true
        case _ => false
      }) should be(true)
    }

    it("MLN to NNF") {
      // Should be
      // (p v !q) v (p ^ (!r v q))
      val mln_nnf = mln.toMLNasNNF
      //println("MLN NNF:")
      //println(mln_nnf)
      (mln_nnf.wformulas.head.formula match {
        case DisjFormula(DisjFormula(LiteralFormula(_, true), LiteralFormula(_, false)),
          ConjFormula(LiteralFormula(_, true),
            DisjFormula(LiteralFormula(_, false),
              LiteralFormula(_, true)))) => true
        case _ => false
      }) should be(true)

    }

    it("MLN to Alchemy CNF") {
      // Should be
      // (p v !q v p) ^ (p v !q v !r v q)
      val mln_cnf = mln.toMLNasAlchemyCNF.normalize
      //println("MLN CNF:")
      //println(mln_cnf)
      (mln_cnf.wformulas.head.formula match {
        case DisjFormula(DisjFormula(LiteralFormula(_, true), LiteralFormula(_, false)), LiteralFormula(_, true)) => true
        case _ => false
      }) should be(true)
      (mln_cnf.wformulas.last.formula match {
        case DisjFormula(DisjFormula(DisjFormula(_, _), _), _) => true
        case _ => false
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("MLN in IJCAI11 paper") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
friends(person,person)
smokes(person)
2 friends(x,y) ^ smokes(x) => smokes(y)
"""
    // Note that this theory has only one clause therefore it will result
    // in the same wcnf irrespective of whether you use the Alchemy
    // transformation or not.

    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause") {
      mln.wformulas.length should be(1)
    }

    it("WCNF by Alchemy transformation") {
      val wcnf = mln.toWeightedCNF()
      //println("WCNF:")
      //println(wcnf)
      // Should result in
      // smokes(Y) v !smokes(X) v !friends(X,Y) v !f(X,Y)
      // friends(X,Y) v f(X,Y)
      // smokes(X) v f(X,Y)
      // !smokes(Y) v f(X,Y)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 1 && clause.negLits.length == 3) true
          else false
        }
      } should be(true)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 1 && clause.negLits.length == 1) true
          else false
        }
      } should be(true)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 2 && clause.negLits.length == 0) true
          else false
        }
      } should be(true)
    }

    it("WCNF by non-Alchemy transformation") {
      val wcnf = mln.toWeightedCNFWithoutSplitting()
      //println("WCNF:")
      //println(wcnf)
      // Should result in
      // smokes(Y) v !smokes(X) v !friends(X,Y) v !f(X,Y)
      // friends(X,Y) v f(X,Y)
      // smokes(X) v f(X,Y)
      // !smokes(Y) v f(X,Y)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 1 && clause.negLits.length == 3) true
          else false
        }
      } should be(true)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 1 && clause.negLits.length == 1) true
          else false
        }
      } should be(true)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 2 && clause.negLits.length == 0) true
          else false
        }
      } should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Example from Alchemy webpage") {

    val parser = new MLNParser
    val mlnString = """
person = {Guy, Nima, Wannes, Jesse, Luc}
p(person)
q(person)
r(person)
s(person)
2 p(x) ^ q(y) ^ (r(x) v s(x))
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause with weight 2") {
      // Hard clauses are not supported and ignored.
      mln.wformulas.isEmpty should be(false)
      mln.wformulas.length should be(1)
      mln.wformulas.head match {
        case wf: WeightedFormula => wf.weight should be(2)
      }
    }

    it("MLN to Alchemy CNF") {
      val mln_cnf = mln.toMLNasAlchemyCNF.normalize
      //println("MLN with subformula to CNF:")
      //println(mln_cnf)
      // Should result in
      // -1 !P(x) v !Q(x)
      //  1  R(x) v  S(x)
      (mln_cnf.wformulas.exists { f =>
        f.weight == -1 &&
          (f.formula match {
            case DisjFormula(LiteralFormula(_, false), LiteralFormula(_, false)) => true
            case _ => false
          })
      }) should be(true)
      (mln_cnf.wformulas.exists { f =>
        f.weight == 1 &&
          (f.formula match {
            case DisjFormula(LiteralFormula(_, true), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("MLN with indivisible subformulas (from Alchemy webpage)") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
p(person)
q(person)
r(person)
s(person)
2 p(x) ^ [ q(x) ^ (r(x) v s(x)) ]
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have one clause with weight 2") {
      mln.wformulas.length should be(1)
      mln.wformulas.head match {
        case wf: WeightedFormula => wf.weight should be(2)
      }
    }

    it("MLN to Alchemy CNF") {
      val mln_cnf = mln.toMLNasAlchemyCNF.normalize
      //println("MLN with subformula to CNF:")
      //println(mln_cnf)
      // Should result in:
      // 1 P(x)
      // 0.5 Q(x)
      // 0.5 R(x) v S(x)
      (mln_cnf.wformulas.exists { f =>
        f.weight == 1 &&
          (f.formula match {
            case a: LiteralFormula => true
            case _ => false
          })
      }) should be(true)
      (mln_cnf.wformulas.exists { f =>
        f.weight == 0.5 &&
          (f.formula match {
            case a: LiteralFormula => true
            case _ => false
          })
      }) should be(true)
      (mln_cnf.wformulas.exists { f =>
        f.weight == 0.5 &&
          (f.formula match {
            case DisjFormula(LiteralFormula(_, true), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
    }

    //it("WCNF should have same number of literals") {
    //val wcnf = mln.toWeightedCNF
    //println("WCNF:")
    //println(wcnf)
    //}

    //it("WCNF No Splitting should have same number of literals") {
    //val wcnf = mln.toWeightedCNFWithoutSplitting
    //println("WCNF:")
    //println(wcnf)
    //}
  }

  //--------------------------------------------------------------------------
  describe("Equivalence with indivisible formula") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
p(person)
q(person)
r(person)
s(person)
f(person)
2 (p(x) ^ [ q(x) ^ (r(x) v s(x)) ]) <=> f(x)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    //it("MLN to CNF") {
    //val mln_cnf = mln.toMLNasCNF.normalize
    //println("MLN with subformula to CNF:")
    //println(mln_cnf)
    //}

    //it("MLN to WCNF should fail") {
    //val wcnf = mln.toWeightedCNF
    //println("WCNF:")
    //println(wcnf)
    //}
  }

  //--------------------------------------------------------------------------
  describe("Wildcard negation") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
s(person)
p(person)
2 *s(x) ^ *p(x)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN to non-Alchemy operators") {
      var mln_na = mln.toMLNasAlchemyOpFree
      //println("Non-Alchemy MLN:")
      //println(mln_na)
      // Should result in:
      // 2.0  S(x) ^  P(x)
      // 2.0  S(x) ^ !P(x)
      // 2.0 !S(x) ^  P(x)
      // 2.0 !S(x) ^ !P(x)
      (mln_na.wformulas.exists { f =>
        f.weight == 2 &&
          (f.formula match {
            case ConjFormula(LiteralFormula(_, true), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
      (mln_na.wformulas.exists { f =>
        f.weight == 2 &&
          (f.formula match {
            case ConjFormula(LiteralFormula(_, true), NegFormula(LiteralFormula(_, true))) => true
            case _ => false
          })
      }) should be(true)
      (mln_na.wformulas.exists { f =>
        f.weight == 2 &&
          (f.formula match {
            case ConjFormula(NegFormula(LiteralFormula(_, true)), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
      (mln_na.wformulas.exists { f =>
        f.weight == 2 &&
          (f.formula match {
            case ConjFormula(NegFormula(LiteralFormula(_, true)), NegFormula(LiteralFormula(_, true))) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Learn per constant terms") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
position = {Faculty, Faculty_adjunct, Faculty_emiritus}
h(person,position)
2 h(x,+y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN to non-Alchemy operators") {
      var mln_na = mln.toMLNasAlchemyOpFree
      //println("Non-Alchemy MLN:")
      //println(mln_na)
      // Should result in:
      // 2 h(x,Faculty)
      // 2 h(x,Faculty_adjunct)
      // 2 h(x,Faculty_emiritus)
      mln_na.wformulas.length should be(3)
      (mln_na.wformulas.forall { f =>
        f.weight == 2.0 &&
          (f.formula match {
            case LiteralFormula(_, true) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Mutual exclusive terms") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
position = {Faculty, Faculty_adjunct, Faculty_emiritus}
h(person,position!)
2 h(x,y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      intercept[IllegalStateException] {
        //println("Testing MLN:\n"+mlnString)
        mln = parser.parseMLN(mlnString)
        //println(mln)
      }
    }

    it("MLN to non-Alchemy operators") {
      //var mln_na = mln.toMLNasAlchemyOpFree
      //println("Non-Alchemy MLN:")
      //println(mln_na)
      // todo: Ignored for now
    }
  }

  //--------------------------------------------------------------------------
  describe("Existential operator") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima}
p(person,person)
2 exist x,y p(x,y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN to non-Alchemy operators") {
      var mln_na = mln.toMLNasExistFree
      //println("Non-Alchemy MLN:")
      //println(mln_na)
      // Should result in:
      // 2.0 P(Guy,Guy) v P(Guy,Nima) v P(Nima,Guy) v P(Nima,Nima)
      mln_na.wformulas.length should be(1)
      (mln_na.wformulas.exists { f =>
        f.weight == 2 &&
          (f.formula match {
            case DisjFormula(DisjFormula(DisjFormula(LiteralFormula(_, true), LiteralFormula(_, true)), LiteralFormula(_, true)), LiteralFormula(_, true)) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Equality operator") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima}
p(person,person)
2 p(x,y) ^ !(x=y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should have 1 lines") {
      mln.wformulas.length should be(1)
    }

    //it("MLN should contain equality definition") {
    //(mln.wformulas.exists{f =>
    //f.hard == true &&
    //(f.formula match {
    //case LiteralFormula(Atom(Predicate(Symbol("eq"),2,_),_,_)) => true
    //case _ => false
    //})
    //}) should be (true)
    //(mln.wformulas.exists{f =>
    //f.hard == true &&
    //(f.formula match {
    //case NegFormula(LiteralFormula(Atom(Predicate(Symbol("eq"),2,_),_,_))) => true
    //case _ => false
    //})
    //}) should be (true)
    //}
  }

  //--------------------------------------------------------------------------

  describe("Social network example from tutorial (for learning)") {

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mlnString =
      """
// MLN for social networks section in tutorial

// Evidence
Friends(person, person)

// Some evidence, some query
Smokes(person)

// Query
Cancer(person)

// Rules
// If you smoke, you get cancer
Smokes(x) => Cancer(x)

// People with friends who smoke, also smoke
// and those with friends who don't smoke, don't smoke
Friends(x, y) => (Smokes(x) <=> Smokes(y))

"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN clauses") {
      //println(mln)
      mln.wformulas.length should be(2)
    }

    it("MLN to Alchemy CNF") {
      var mln_cnf = mln.toMLNasAlchemyCNF
      //println("CNF:")
      //println(mln_cnf)
      mln_cnf.wformulas.length should be(3)
      (mln_cnf.wformulas.exists { f =>
        f.weight == 0.0 &&
          (f.formula match {
            case DisjFormula(LiteralFormula(_, false), LiteralFormula(_, true)) => true
            case DisjFormula(LiteralFormula(_, true), LiteralFormula(_, false)) => true
            case _ => false
          })
      }) should be(true)
    }

    it("MLN to weighted CNF") {
      val wcnf = mln.toWeightedCNF()
      //println("WCNF:")
      //println(wcnf)
      wcnf.cnf.clauses.exists { clause =>
        {
          if (clause.posLits.length == 1 && clause.negLits.length == 3) true
          else false
        }
      } should be(true)
    }
  }
  //--------------------------------------------------------------------------

  describe("University example from Alchemy distribution") {

    val parser = new MLNParser
    val parsere = new MLNParser
    parser.setLearnModus(true)
    parsere.setLearnModus(true)
    val domFile = Resource.fromFile("/univ/univ-train.mln").mkString
    val mlnFile = Resource.fromFile("/univ/univ.mln").mkString
    val mlnString = domFile + mlnFile
    val dbFile = Resource.fromFile("/univ/univ-train.db").mkString
    val dbString = dbFile
    val mlneFile = Resource.fromFile("/univ/univ-extended.mln").mkString
    val mlneString = domFile + mlneFile

    var mln = MLN()
    var mlne = MLN()
    var db = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN has 7 lines") {
      mln.wformulas.length should be(7)
    }

    it("DB is parsable") {
      db = parser.parseDB(dbString)
    }

    it("DB has 60 facts") {
      db.evidence.length should be(60)
    }

    it("Extended MLN is parsable") {
      //println("Testing MLN:\n"+mlneString)
      mlne = parsere.parseMLN(mlneString)
      //println(mlne)
    }

    it("Extended MLN has 11 lines") {
      mlne.wformulas.length should be(11)
    }
  }

  //--------------------------------------------------------------------------

  describe("Guy's smokers learning example") {

    val structureStr =
      """
person = {Anna,Bob,Chris,Daniel,Edward,Frank,Gary,Helen}
friends(person, person)
smokes(person)
cancer(person)
-4.20887  friends(a1,a2)
5.13753  smokes(a1)
-6.17535  cancer(a1)
6.18039 !smokes(x) v cancer(x)
0.61212 !friends(x,y) v smokes(x) v !smokes(y)
0.61212 !friends(x,y) v !smokes(x) v smokes(y)
9.67615  friends(a1,a2) v !friends(a2,a1)
"""
    val trainingDBStr =
      """
friends(Anna, Bob)
friends(Bob, Anna)
friends(Anna, Edward)
friends(Edward, Anna)
friends(Anna, Frank)
friends(Frank, Anna)
friends(Bob, Chris)
friends(Chris, Bob)
friends(Chris, Daniel)
friends(Daniel, Chris)
friends(Edward, Frank)
friends(Frank, Edward)
friends(Gary, Helen)
friends(Helen, Gary)
friends(Gary, Anna)
friends(Anna, Gary)

smokes(Anna)
smokes(Edward)
smokes(Frank)
smokes(Gary)

cancer(Anna)
cancer(Edward)
"""

    val parser = new MLNParser
    parser.isLearnModus = true

    var mln = MLN()
    var db = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }

    it("MLN length") {
      mln.wformulas.length should be(7)
    }

    it("DB is parsable") {
      db = parser.parseDB(trainingDBStr)
      //println("DB:")
      //println(db.toStringExt)
    }

    it("DB length") {
      db.evidence.length should be(22)
    }
  }

  //--------------------------------------------------------------------------
  describe("Forall clauses") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
a(person,person)
1 exist x forall y a(x,y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should be Exists and Forall formulas") {
      mln.wformulas.length should be(1)
      (mln.wformulas.exists { f =>
        f.weight == 1 &&
          (f.formula match {
            case ExistFormula(_, ForallFormula(_, _)) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Quantifiers in formula") {

    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes, Jesse, Luc}
a(person,person)
b(person)
1 b(y) <=> exist x a(x,y)
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    it("MLN should be correct formula") {
      mln.wformulas.length should be(1)
      (mln.wformulas.exists { f =>
        f.weight == 1 &&
          (f.formula match {
            case EqFormula(_, ExistFormula(_, _)) => true
            case _ => false
          })
      }) should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("PLP for UWCSE") {

    val parser = new MLNParser
    val mlnString =
      """
Advisedby(person,person)
Courselevel(course,level)
Phase(person,phase)
Position(person, faculty)
Professor(person)
Projectmember(project, person)
Publication(title,person)
Samecourse(course, course)
Sameperson(person, person)
Sameproject(project, project)
Student(person)
Ta(course,person,year)
Taughtby(course,person,year)
Tempadvisedby(person,person)
Yearsinprogram(person,num)

PFptb(course, person, year)
PFpab(person, person)
PFpp(title, person)

PFsab(person, person)
PFsp(title, person)
PFsta(course, person, year)
PFsph(person, phase)
PFyip(person, num)

Professor(p) <=> (exist c, y Taughtby(c,p,y) ^ PFptb(c,p,y)) v
                 (exist s Advisedby(s,p) ^ PFpab(s,p)) v
                 (exist t Publication(t, p) ^ PFpp(t, p)).

Student(p) <=> (exist c, y Ta(c,p,y) ^ PFsta(c,p,y)) v 
               (exist s Advisedby(s,p) ^ PFsab(s,p)) v
               (exist t Publication(t, p) ^ PFsp(t, p)) v
               (exist ph Phase(p, ph) ^ PFsph(p, ph)) v
               (exist n Phase(p, n) ^ PFyip(p, n)).

!Professor(p) v !Student(p).
"""
    var mln = MLN()

    it("MLN is parsable") {
      //println("Testing MLN:\n"+mlnString)
      mln = parser.parseMLN(mlnString)
      //println(mln)
    }

    //        it("MLN should be correct") {
    //            println(mln)
    //            val mln2 = mln.toWeightedCNFWithoutSplitting(true, true)
    //            print(mln2)
    //        }
  }

  //--------------------------------------------------------------------------

  describe("Hierarchical domains") {

    val structureStr =
      """
person = {male, female}
male = 5 {}
female = 4 {}

friends(person,person)

2 friends(y,z), x in male, y in male
1 friends(x,y), x in male, y in female
"""

    val parser = new MLNParser

    var mln = MLN()

    it("MLN is parsable") {
      mln = parser.parseMLN(structureStr)
      //println("MLN:")
      //println(mln.toStringExt)
    }

    it("MLN length") {
      mln.wformulas.length should be(2)
    }
  }
  
  //--------------------------------------------------------------------------
  
}

// vim: set ts=4 sw=4 et:
