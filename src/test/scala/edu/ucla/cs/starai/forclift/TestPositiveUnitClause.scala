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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import edu.ucla.cs.starai.forclift.constraints._
import edu.ucla.cs.starai.forclift.inference._

import org.scalatest.FunSpec


@RunWith(classOf[JUnitRunner])
class TestPositiveUnitClause extends FunSpec with Matchers {

  describe("a clause without constraints") {

    describe("and with a root domain") {

      val D = new RootDomain("D", List(Constant("elem")))
      val X = new Var;
      val Y = new Var;
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y)

      def domainSize(size: Int) = {
        (new DomainSizes()) + (D -> size)
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(1)) should have length (1)
        clause.ground(domainSize(2)) should have length (4)
        clause.ground(domainSize(3)) should have length (9)
        clause.ground(domainSize(10)) should have length (100)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(1)) should be(1)
        clause.nbGroundings(domainSize(2)) should be(4)
        clause.nbGroundings(domainSize(3)) should be(9)
        clause.nbGroundings(domainSize(10)) should be(100)
      }
    }

    describe("and with a subdomain") {

      val D = new RootDomain("D", List(Constant("elem")))
      val D2 = D.subdomain()
      val X = new Var;
      val Y = new Var;
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(elemConstrs = ElemConstr(X -> D2, Y -> D2)))

      def domainSize(rootSize: Int, subSize: Int) = {
        (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2.complement -> (rootSize - subSize))
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(10, 1)) should have length (1)
        clause.ground(domainSize(10, 2)) should have length (4)
        clause.ground(domainSize(10, 3)) should have length (9)
        clause.ground(domainSize(11, 10)) should have length (100)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(10, 1)) should be(1)
        clause.nbGroundings(domainSize(10, 2)) should be(4)
        clause.nbGroundings(domainSize(10, 3)) should be(9)
        clause.nbGroundings(domainSize(11, 10)) should be(100)
      }
    }

    describe("and with a subdomain with excluded elem") {

      val a = Constant("a")
      val D = new RootDomain("D", List(a))
      val D2 = D.subdomain(excludedConstants = Set(a))
      val X = new Var;
      val Y = new Var;
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, a), (Y, a)), ElemConstr(X -> D2, Y -> D2)))

      def domainSize(rootSize: Int, subSize: Int) = {
        (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2.complement -> (rootSize - subSize - 1))
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(10, 1)) should have length (1)
        clause.ground(domainSize(10, 2)) should have length (4)
        clause.ground(domainSize(10, 3)) should have length (9)
        clause.ground(domainSize(11, 10)) should have length (100)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(10, 1)) should be(1)
        clause.nbGroundings(domainSize(10, 2)) should be(4)
        clause.nbGroundings(domainSize(10, 3)) should be(9)
        clause.nbGroundings(domainSize(11, 10)) should be(100)
      }
    }

  }

  describe("a clause with constraints") {

    describe("and with a root domain") {

      val X = new Var;
      val Y = new Var;
      val D = new RootDomain("D", List(Constant("elem")))
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, Y))))

      def domainSize(size: Int) = {
        (new DomainSizes()) + (D -> size)
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(1)) should have length (0)
        clause.ground(domainSize(2)) should have length (2)
        clause.ground(domainSize(3)) should have length (6)
        clause.ground(domainSize(10)) should have length (90)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(1)) should be(0)
        clause.nbGroundings(domainSize(2)) should be(2)
        clause.nbGroundings(domainSize(3)) should be(6)
        clause.nbGroundings(domainSize(10)) should be(90)
      }

    }

    describe("and with a subdomain") {

      val D = new RootDomain("D", List(Constant("elem")))
      val D2 = D.subdomain()
      val X = new Var;
      val Y = new Var;
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, Y)), ElemConstr(X -> D2, Y -> D2)))

      def domainSize(rootSize: Int, subSize: Int) = {
        (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2.complement -> (rootSize - subSize))
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(10, 1)) should have length (0)
        clause.ground(domainSize(10, 2)) should have length (2)
        clause.ground(domainSize(10, 3)) should have length (6)
        clause.ground(domainSize(11, 10)) should have length (90)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(10, 1)) should be(0)
        clause.nbGroundings(domainSize(10, 2)) should be(2)
        clause.nbGroundings(domainSize(10, 3)) should be(6)
        clause.nbGroundings(domainSize(11, 10)) should be(90)
      }
    }

    describe("and with a subdomain with 1 excluded elem") {

      val a = Constant("a")
      val D = new RootDomain("D", List(a))
      val D2 = D.subdomain(excludedConstants = Set(a))
      val X = new Var;
      val Y = new Var;
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, Y), (X, a), (Y, a)), ElemConstr(X -> D2, Y -> D2)))

      def domainSize(rootSize: Int, subSize: Int) = {
        (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2.complement -> (rootSize - subSize - 1))
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(10, 1)) should have length (0)
        clause.ground(domainSize(10, 2)) should have length (2)
        clause.ground(domainSize(10, 3)) should have length (6)
        clause.ground(domainSize(11, 10)) should have length (90)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(10, 1)) should be(0)
        clause.nbGroundings(domainSize(10, 2)) should be(2)
        clause.nbGroundings(domainSize(10, 3)) should be(6)
        clause.nbGroundings(domainSize(11, 10)) should be(90)
      }
    }

    describe("between different domains") {

      val X = new Var;
      val Y = new Var;
      val D = new RootDomain("D", List(Constant("elem")))
      val D2 = D.subdomain()
      val p = new Predicate('p, 2, Seq(D, D))
      val p_X_Y = p(X, Y)
      val clause = new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, Y)), ElemConstr((X, D), (Y, D2))))

      def domainSize(Dsize: Int, D2size: Int) = {
        (new DomainSizes()) + (D -> Dsize) + (D2 -> D2size) + (D2.complement -> (Dsize - D2size))
      }

      it("should have the correct grounding") {
        clause.ground(domainSize(1, 1)) should have length (0)
        clause.ground(domainSize(2, 2)) should have length (2)
        clause.ground(domainSize(2, 1)) should have length (1)
        clause.ground(domainSize(2, 0)) should have length (0)
        clause.ground(domainSize(3, 3)) should have length (6 + 0)
        clause.ground(domainSize(3, 2)) should have length (2 + 2)
        clause.ground(domainSize(3, 1)) should have length (0 + 2)
        clause.ground(domainSize(3, 0)) should have length (0 + 0)
        clause.ground(domainSize(10, 10)) should have length (90)
        clause.ground(domainSize(10, 0)) should have length (0)
      }

      it("should have the correct number of groundings") {
        clause.nbGroundings(domainSize(1, 1)) should be(0)
        clause.nbGroundings(domainSize(2, 2)) should be(2)
        clause.nbGroundings(domainSize(2, 1)) should be(1)
        clause.nbGroundings(domainSize(2, 0)) should be(0)
        clause.nbGroundings(domainSize(3, 3)) should be(6 + 0)
        clause.nbGroundings(domainSize(3, 2)) should be(2 + 2)
        clause.nbGroundings(domainSize(3, 1)) should be(0 + 2)
        clause.nbGroundings(domainSize(3, 0)) should be(0 + 0)
        clause.nbGroundings(domainSize(10, 10)) should be(90)
        clause.nbGroundings(domainSize(10, 0)) should be(0)
      }

    }

  }

  describe("a clause with constraints") {

    val X = new Var;
    val Y = new Var;
    val D = new RootDomain("D", List(Constant("elem")))
    val p = new Predicate('p, 2, Seq(D, D))
    val p_X_Y = p(X, Y)
    val clause = (new PositiveUnitClause(p_X_Y)).standardizeApart
    val clauseWithConstraints = (new PositiveUnitClause(p_X_Y, Constraints(IneqConstr((X, Y))))).standardizeApart

    it("should be subsumed by the same clause without constraints") {
      clause.subsumes(clauseWithConstraints) should be(true)
    }
    it("should not subsume the same clause without constraints") {
      clauseWithConstraints.subsumes(clause) should be(false)
    }

  }

}
