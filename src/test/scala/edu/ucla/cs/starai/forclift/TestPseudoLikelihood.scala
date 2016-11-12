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

import edu.ucla.cs.starai.forclift.learning._
import edu.ucla.cs.starai.forclift.inference._
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift.learning.Likelihood
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser

@RunWith(classOf[JUnitRunner])
class TestPseudoLikelihood extends FunSpec with Matchers {

  //--------------------------------------------------------------------------
  describe("For independent unit clauses: pll==ll") {

    val mlnStr =
      """
a(dom)
b(dom)

1 a(x)
1 b(x)
"""

    val dbStr =
      """
a(X1)
b(X2)
"""
    

    it("Should have correct likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val llstat = Likelihood.mlnLikelihood(mln, db)
      // world = e**0+e**4+4*e**1+6*e**2+4*e**3 = 191.147
      // db = a1 ^ b2 = e**2 = 7.389
      // db/world = 0.0386
      llstat.head.toDouble should be(0.038 +- 0.001)

    }

    it("Should have correct pseudo likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      // db = a1 ^ b2
      // db(a1)  = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      // db(b2)  = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      // db(!a2) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!b1) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      //                                            -------       --------
      // PLL = db(a1)*db(b2)*db(a2)*db(b1)          = 0.038  LPLL = -3.252
      pllstat.head.toDouble should be(0.038 +- 0.001)
    }
  }

  //--------------------------------------------------------------------------
  describe("Pseudo likelihood for complex formulas") {

    val mlnStr =
      """
a(dom)
b(dom)

1 a(x) ^ b(x)
1 b(x)
"""

    val dbStr =
      """
a(X1)
b(X2)
"""
    
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = Seq(parser.parseDB(dbStr))

    it("Should have correct pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      // db = a1 ^ b2
      // db(a1)  = e**1/(e**1+e**1) diff =  0  PLL = 0.500  LPLL = -0.693
      // db(b2)  = e**1/(e**0+e**1) diff = -1  PLL = 0.731  LPLL = -0.313
      // db(!a2) = e**1/(e**2+e**1) diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!b1) = e**1/(e**3+e**1) diff = +2  PLL = 0.119  LPLL = -2.126
      //                                           -------       --------
      // db = db(a1)*db(b2)*db(a2)*db(b1)          = 0.117       = -4.445
      pllstat.head.toDouble should be(0.011 +- 0.001)
    }
  }

  //--------------------------------------------------------------------------
  describe("Conjunction with not used predicate") {

    val mlnStr =
      """
a(dom)
b(dom)
c(dom)

1 a(x) ^ b(x)
1 b(x)
"""

    val dbStr =
      """
a(X1)
b(X2)
"""
    
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = Seq(parser.parseDB(dbStr))

    it("Should have correct pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      // db = a1 ^ b2
      // db(a1)  = e**1/(e**1+e**1) diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a2) = e**1/(e**2+e**1) diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!b1) = e**1/(e**3+e**1) diff = +2  PLL = 0.119  LPLL = -2.126
      // db(b2)  = e**1/(e**0+e**1) diff = -1  PLL = 0.731  LPLL = -0.313
      // db(!c1) = e**1/(e**1+e**1) diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!c2) = e**1/(e**1+e**1) diff =  0  PLL = 0.500  LPLL = -0.693
      //                                           -------       --------
      // db = db(a1)*db(b2)*db(a2)*db(b1)          = 0.0029      = -5.831
     pllstat.head.logToDouble should be(-5.831 +- 0.01)
    }

    it("Should have correct normalized pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db, verbose = true, normalizepll = true)
      // Alchemy gives the following but this is because it wrongly translates
      // the formula into `1 !a(x) v !b(x)` instead of `-1 !a(x) v !b(x)`.
      //pllstat.dblogll.head should be (-1.69956 +- 0.01)
     pllstat.head.logToDouble should be(-2.9155 +- 0.01)
    }
  }

  //--------------------------------------------------------------------------
  describe("Disjunction with not used predicate") {

    val mlnStr =
      """
a(dom)
b(dom)
c(dom)

1 !a(x) v !b(x)
1 b(x)
"""

    val dbStr =
      """
a(X1)
b(X2)
"""
    
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = Seq(parser.parseDB(dbStr))

    it("Should have correct normalized pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db, normalizepll = true)
     pllstat.head.logToDouble should be(-1.69956 +- 0.01)
    }

    it("Should have correct pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db.toSeq)
      // db = a1 ^ b2
      // db( a1) = e**3/(e**3+e**3) diff =  0   LPLL = -0.693
      // db(!a2) = e**3/(e**2+e**3) diff = -1   LPLL = -0.313
      // db(!b1) = e**3/(e**3+e**3) diff =  0   LPLL = -0.693
      // db( b2) = e**3/(e**2+e**3) diff = -1   LPLL = -0.313
      // db(!c1) = e**3/(e**3+e**3) diff =  0   LPLL = -0.693
      // db(!c2) = e**3/(e**3+e**3) diff =  0   LPLL = -0.693
      //                                             --------
      //                                             = -3.398
     pllstat.head.logToDouble should be(-3.398 +- 0.01)
    }
  }

  //--------------------------------------------------------------------------
  describe("Pseudo likelihood for complex formulas and weight 0") {

    val mlnStr =
      """
a(dom)
b(dom)

0 a(x) ^ b(x)
0 b(x)
"""

    val dbStr =
      """
a(X1)
b(X2)
"""
    

    it("Should have correct likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val llstat = Likelihood.mlnLikelihood(mln, db)
      llstat.head.toDouble should be(0.062 +- 0.001)
    }

    it("Should have correct pseudo likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      //println("===============================")
      //println("pll stat = "+pllstat)
      // db = a1 ^ b2
      // db(*)  = e**0/(e**0+e**0) diff =  0  PLL = 0.500  LPLL = -0.693
      // ... 4 times
      //                                          -------       --------
      // db =                                     = 0.062       = -2.772
      pllstat.head.toDouble should be(0.062 +- 0.001)
    }
  }

  //--------------------------------------------------------------------------
  describe("Independent unit clauses with arity 2") {

    val mlnStr =
      """
a(dom,dom)

1 a(x,y)
"""

    val dbStr =
      """
a(X1,X2)
a(X1,X3)
"""
    val dbStrs = List(dbStr)

      
    it("Should have correct likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val llstat = Likelihood.mlnLikelihood(mln, db)
      //println("===============================")
      //println("ll stat = "+llstat)
      llstat.head.toDouble should be(5.45e-5 +- 1e-6)
    }

    it("Should have correct pseudo likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      //println("===============================")
      //println("pll stat = "+pllstat)
      // db = a12 ^ a13
      // db(!a11) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db( a12) = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      // db( a13) = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      // db(!a21) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a22) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a23) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a31) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a32) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a33) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      //                                             -------       --------
      // PLL = db(a1)*db(b2)*db(a2)*db(b1)           = 5.4e-5 LPLL = -9.817
      pllstat.head.toDouble should be(5.45e-5 +- 1e-6)
    }
  }

  //--------------------------------------------------------------------------
  describe("Independent unit clauses with identical arguments") {

    val mlnStr =
      """
a(dom,dom)

1 a(x,x)
"""

    val dbStr =
      """
a(X1,X1)
a(X1,X2)
a(X3,X3)
"""
    
    val parser = new MLNParser
    parser.setLearnModus(true)
    val mln = parser.parseMLN(mlnStr)
    val db = Seq(parser.parseDB(dbStr))

    //it("Should have correct likelihood") {
    //val llstat = Likelihood.mlnLikelihood(mlnStr, dbStrs.toSeq)
    //llstat.head.logToDouble should be (5.45e-5 +- 1e-6)
    //}

    it("Should have correct pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      // db = a11 ^ a12 ^ a33
      // db( a11) = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      // db( a12) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a13) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a21) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a22) = e**2/(e**3+e**2)  diff = +1  PLL = 0.268  LPLL = -1.313
      // db(!a23) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a31) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db(!a32) = e**2/(e**2+e**2)  diff =  0  PLL = 0.500  LPLL = -0.693
      // db( a33) = e**2/(e**1+e**2)  diff = -1  PLL = 0.731  LPLL = -0.313
      //                                             -------       --------
      // PLL = db(a1)*db(b2)*db(a2)*db(b1)           = 0.0022 LPLL = -6.097
     pllstat.head.logToDouble should be(-6.097 +- 0.1)
    }

    it("Should have correct normalized pseudo likelihood") {
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db, normalizepll = true)
      // -6.097/9=-0.677
     pllstat.head.logToDouble should be(-0.67763 +- 0.01)
    }
  }

  //--------------------------------------------------------------------------
  describe("Independent unit clauses with identical arguments and weight 0") {

    val mlnStr =
      """
a(dom,dom)

0 a(x,x)
"""

    val dbStr =
      """
a(X1,X1)
a(X1,X2)
a(X3,X3)
"""

    it("Should have correct likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val llstat = Likelihood.mlnLikelihood(mln, db)
      llstat.head.toDouble should be(0.0019 +- 0.0001)
    }

    it("Should have correct pseudo likelihood") {
      val parser = new MLNParser
      parser.setLearnModus(true)
      val mln = parser.parseMLN(mlnStr)
      val db = Seq(parser.parseDB(dbStr))
      val pllstat = Likelihood.mlnPseudoLikelihood(mln, db)
      // db = a11 ^ a12 ^ a33
      // db(*) = e**0/(e**0+e**0)  diff =  0  PLL = 0.500  LPLL = -0.693
      // ... 9 times
      //                                          -------       --------
      // PLL =                                    = 0.0019 LPLL = -6.237
      pllstat.head.toDouble should be(0.0019 +- 0.0001)
    }
  }
}
