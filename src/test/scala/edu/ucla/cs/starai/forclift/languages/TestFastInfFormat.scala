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

package edu.ucla.cs.starai.forclift.languages

import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner

import edu.ucla.cs.starai.forclift.Atom
import edu.ucla.cs.starai.forclift.Constant
import edu.ucla.cs.starai.forclift.PositiveUnitClause
import edu.ucla.cs.starai.forclift.Predicate
import edu.ucla.cs.starai.forclift.RootDomain
import edu.ucla.cs.starai.forclift.Var
import edu.ucla.cs.starai.forclift.languages.mln.MLNParser

@RunWith(classOf[JUnitRunner])
class TestFastInfFormat extends FunSpec with Matchers {

  describe("Unification") {

    describe("Subsumption") {
      val v = Constant("v")
      val D = new RootDomain("D", List(v))
      val p = Predicate(Symbol("pred"), 1, Seq(D))
      val V = new Var()

      it("Single term") {
        val a1 = Atom(p, v)
        val a2 = Atom(p, V)
        val a3 = Atom(p, v)
        val puc1 = new PositiveUnitClause(a1)
        val puc2 = new PositiveUnitClause(a2)
        val puc3 = new PositiveUnitClause(a3)
        puc1.subsumes(puc2) should be(false)
        puc2.subsumes(puc1) should be(true)
        puc3.subsumes(puc1) should be(false) //TODO why? theta exists (empty one) and would subsume here
      }
    }

  }

  describe("Smokers") {
    val parser = new MLNParser
    val mlnString =
      """
person = {Guy, Nima, Wannes}
friends(person,person)
smokes(person)
1 friends(x,y) ^ smokes(x) => smokes(y)
"""
    val mln = parser.parseMLN(mlnString)

    it("Correct FastInf output") {
      val fastInf = new FastInfFormat(mln, useparamtying = false, verbose = false)
      println(fastInf.toString)
    }
  }

}
