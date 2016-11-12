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

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import edu.ucla.cs.starai.forclift.learning._
import edu.ucla.cs.starai.forclift.learning.Likelihood
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser

@RunWith(classOf[JUnitRunner])
class TestLikelihood extends FunSpec with Matchers {

  // SOME TESTS HERE FAIL BECAUSE THE DOMAIN SIZE IS 1, BUT THE NUMBER OF VARIABLES PER FORMULA IS 2
  // THIS IS NOT ALLOWED AS INPUT TO THE WEIGHT LEARNER

  val acc = 0.01

  //--------------------------------------------------------------------------
  describe("Simple theory I") {

    val mlnStr =
      """
a(dom)
b(dom)

1 !a(a) v b(b)
"""

    val dbStr =
      """
a(X1)
b(X1)
"""
    val dbStrs = List(dbStr)

    ignore("Should have correct likelihood") {
      //println("===============================")
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val stat = Likelihood.mlnLikelihood(mln, db, verbose = true)
      //println(stat)
      // world = 3*e**1 + 1*e**0 = 9.154
      // db = a ^ b = 1*e**1 = 2.718
      // db/world = 0.296
      stat.head.toDouble should be(0.296 +- acc)
    }
  }

  //--------------------------------------------------------------------------
  describe("Simple theory II") {

    // @todo Make test case for predicates without arguments
    // @todo Make test case for domains with only one element

    val mlnStr =
      """
//a(dom2)
a(dom)
b(dom)

1 !a(a) v b(b)
"""

    val dbStr =
      """
b(X1)
b(X2)
"""
    val dbStrs = List(dbStr)
      
    it("Should have correct likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val stat = Likelihood.mlnLikelihood(mln, db, verbose = true)
      // world = 7*e**4 + 4*e**3 + 2*e**2 + 2*e**1 + 1*e**0 = 493.085
      // db = 1*e**4 = 54.598
      // db/world = 0.110
      stat.head.toDouble should be(0.110 +- acc)
    }

    it("Should have correct normalized likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val stat = Likelihood.mlnLikelihood(mln, db, verbose = true, normalizell = true)
      // log(world_form1 norm)= log(7*e**4 + 4*e**3 + 2*e**2 + 2*e**1 + 1*e**0)/4 = 1.550170
      // log(db_form1 norm) = (1*4+0*0)/4 = 1
      // db/world = exp(1-1.550170) = 0.57685
      stat.head.toDouble should be(0.57685 +- acc)
    }
  }

  //--------------------------------------------------------------------------
  describe("Simple theory I + II") {

    val mlnStr =
      """
a(dom)
b(dom)

1 !a(a) v b(b)
"""

    val dbStrs = List(
      """
a(X1)
b(X1)
""",
      """
b(X1)
b(X2)
""")
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = dbStrs.map(parser.parseDB(_))

    ignore("Should have correct likelihood") {
      //println("===============================")
      val stat = Likelihood.mlnLikelihood(mln, db, verbose = true)
      //println(stat)
      // world = 3*e**1 + 1*e**0 = 9.154
      // db = !a ^ b = 1*e**1 = 2.718
      // db/world = 0.296
      stat.exists { dbll =>
        (dbll.toDouble > 0.296 - acc) && (dbll.toDouble < 0.296 + acc)
      } should be(true)
      stat.exists { dbll =>
        (dbll.toDouble > 0.110 - acc) && (dbll.toDouble < 0.110 + acc)
      } should be(true)
    }
  }

  //--------------------------------------------------------------------------
  describe("Simple theory I + II, non-overlapping consts") {

    val mlnStr =
      """
a(dom)
b(dom)

1 !a(a) v b(b)
"""

    val dbStrs = Seq(
      """
a(X1)
b(X1)
""",
      """
b(X2)
b(X3)
""")

    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = dbStrs.map(parser.parseDB(_))
      
    ignore("Should have correct likelihood") {
      //println("===============================")
      val stat = Likelihood.mlnLikelihood(mln, db, verbose = true)
      //println(stat)
      // world = 3*e**1 + 1*e**0 = 9.154
      // db = !a ^ b = 1*e**1 = 2.718
      // db/world = 0.296
      stat.exists { dbll =>
        (dbll.toDouble > 0.296 - acc) && (dbll.toDouble < 0.296 + acc)
      } should be(true)
      stat.exists { dbll =>
        (dbll.toDouble > 0.110 - acc) && (dbll.toDouble < 0.110 + acc)
      } should be(true)
    }
  }
}
