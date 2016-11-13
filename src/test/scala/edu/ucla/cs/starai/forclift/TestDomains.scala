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

import edu.ucla.cs.starai.forclift.inference._

import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class TestDomains extends FunSpec with Matchers {

  describe("a root domain") {

    val D = new RootDomain("D", Nil)

    def domainSize(size: Int) = {
      (new DomainSizes()) + (D -> size)
    }

    it("should have a correct size without excluded constants") {
      D.size(domainSize(0), Set()) should be(0)
      D.size(domainSize(1), Set()) should be(1)
      D.size(domainSize(2), Set()) should be(2)
      D.size(domainSize(3), Set()) should be(3)
      D.size(domainSize(100), Set()) should be(100)
    }

    it("should have a correct set of constants without excluded constants") {
      D.constants(domainSize(0), Set()) should have size (0)
      D.constants(domainSize(1), Set()) should have size (1)
      D.constants(domainSize(2), Set()) should have size (2)
      D.constants(domainSize(3), Set()) should have size (3)
      D.constants(domainSize(100), Set()) should have size (100)
    }

    it("should have a correct size with 1 excluded constant") {
      //        	for(c <- D.constants(domainSize(1))){
      //        		evaluating { D.size(domainSize(0), Set(c)) } should produce[Throwable]
      //        	}
      for (c <- D.constants(domainSize(1))) {
        D.size(domainSize(1), Set(c)) should be(0)
      }
      for (c <- D.constants(domainSize(2))) {
        D.size(domainSize(2), Set(c)) should be(1)
      }
      for (c <- D.constants(domainSize(3))) {
        D.size(domainSize(3), Set(c)) should be(2)
      }
      for (c <- D.constants(domainSize(100))) {
        D.size(domainSize(100), Set(c)) should be(99)
      }
    }
  }

  describe("a root domain with 1 known constant") {

    val D = new RootDomain("D", List(Constant("a")))

    def domainSize(size: Int) = {
      (new DomainSizes()) + (D -> size)
    }

    it("should have a correct size without excluded constants") {
      D.size(domainSize(1), Set()) should be(1)
      D.size(domainSize(2), Set()) should be(2)
      D.size(domainSize(3), Set()) should be(3)
      D.size(domainSize(100), Set()) should be(100)
    }

    it("should have a correct set of constants without excluded constants") {
      D.constants(domainSize(1), Set()) should have size (1)
      D.constants(domainSize(2), Set()) should have size (2)
      D.constants(domainSize(3), Set()) should have size (3)
      D.constants(domainSize(100), Set()) should have size (100)
    }

    it("should have the known constant in its set of constants") {
      D.constants(domainSize(1), Set()) should contain(Constant("a"))
      D.constants(domainSize(2), Set()) should contain(Constant("a"))
      D.constants(domainSize(3), Set()) should contain(Constant("a"))
      D.constants(domainSize(100), Set()) should contain(Constant("a"))
    }

    it("should have a correct size with 1 excluded constant") {
      for (c <- D.constants(domainSize(1))) {
        D.size(domainSize(1), Set(c)) should be(0)
      }
      for (c <- D.constants(domainSize(2))) {
        D.size(domainSize(2), Set(c)) should be(1)
      }
      for (c <- D.constants(domainSize(3))) {
        D.size(domainSize(3), Set(c)) should be(2)
      }
      for (c <- D.constants(domainSize(100))) {
        D.size(domainSize(100), Set(c)) should be(99)
      }
    }
  }

  describe("a subdomain without excluded constants") {

    val D = new RootDomain("D", List(Constant("a")))
    val D2 = D.subdomain()

    def domainSize(rootSize: Int, subSize: Int) = {
      (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize)
    }

    it("should have a correct size without excluded constants") {
      D2.size(domainSize(10, 0), Set()) should be(0)
      D2.size(domainSize(10, 1), Set()) should be(1)
      D2.size(domainSize(10, 2), Set()) should be(2)
      D2.size(domainSize(10, 3), Set()) should be(3)
      D2.size(domainSize(200, 100), Set()) should be(100)
    }

    it("should have a correct set of constants without excluded constants") {
      D2.constants(domainSize(10, 0), Set()) should have size (0)
      D2.constants(domainSize(10, 1), Set()) should have size (1)
      D2.constants(domainSize(10, 2), Set()) should have size (2)
      D2.constants(domainSize(10, 3), Set()) should have size (3)
      D2.constants(domainSize(200, 100), Set()) should have size (100)
    }

    it("should have a correct size with 1 excluded constant") {
      val excluded = Set(Constant("a"))
      //            evaluating { D2.size(domainSize(10, 0), excluded) } should produce[Throwable]
      D2.size(domainSize(10, 1), excluded) should be(0)
      D2.size(domainSize(10, 2), excluded) should be(1)
      D2.size(domainSize(10, 3), excluded) should be(2)
      D2.size(domainSize(100, 100), excluded) should be(99)
    }

    it("should have a correct set of constants with 1 excluded constant") {
      val excluded = Set(Constant("a"))
      //            evaluating { D2.constants(domainSize(10, 0), excluded) } should produce[Throwable]
      D2.constants(domainSize(10, 1), excluded) should have size (0)
      D2.constants(domainSize(10, 2), excluded) should have size (1)
      D2.constants(domainSize(10, 3), excluded) should have size (2)
      D2.constants(domainSize(100, 100), excluded) should have size (99)
    }

    it("should have a correct size with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant(1))
      //            evaluating { D2.size(domainSize(10, 1), excluded2) } should produce[Throwable]
      D2.size(domainSize(10, 2), excluded2) should be(0)
      D2.size(domainSize(10, 3), excluded2) should be(1)
      D2.size(domainSize(100, 100), excluded2) should be(98)
    }

    it("should have a correct set of constants with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant(1))
      //            evaluating { D2.constants(domainSize(10, 1), excluded2) } should produce[Throwable]
      D2.constants(domainSize(10, 2), excluded2) should have size (0)
      D2.constants(domainSize(10, 3), excluded2) should have size (1)
      D2.constants(domainSize(100, 100), excluded2) should have size (98)
    }
  }

  describe("a subdomain complement without excluded constants") {

    val D = new RootDomain("D", List(Constant("a")))
    val D2 = D.subdomain()
    val D2Complement = D2.complement

    def domainSize(rootSize: Int, subSize: Int, complSize: Int) = {
      (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2Complement -> complSize)
    }

    it("should have a correct size without excluded constants") {
      D2Complement.size(domainSize(10, 10, 0), Set()) should be(0)
      D2Complement.size(domainSize(10, 9, 1), Set()) should be(1)
      D2Complement.size(domainSize(10, 8, 2), Set()) should be(2)
      D2Complement.size(domainSize(10, 7, 3), Set()) should be(3)
      D2Complement.size(domainSize(100, 0, 100), Set()) should be(100)
    }

    it("should have a correct set of constants without excluded constants") {
      D2Complement.constants(domainSize(10, 10, 0), Set()) should have size (0)
      D2Complement.constants(domainSize(10, 9, 1), Set()) should have size (1)
      D2Complement.constants(domainSize(10, 8, 2), Set()) should have size (2)
      D2Complement.constants(domainSize(10, 7, 3), Set()) should have size (3)
      D2Complement.constants(domainSize(100, 0, 100), Set()) should have size (100)
    }

    it("should have a correct size with 1 excluded constant") {
      val excluded = Set(Constant("a"))
      //            evaluating { D2Complement.size(domainSize(10, 10, 0), excluded) } should produce[Throwable]
      D2Complement.size(domainSize(10, 9, 1), excluded) should be(0)
      D2Complement.size(domainSize(10, 8, 2), excluded) should be(1)
      D2Complement.size(domainSize(10, 7, 3), excluded) should be(2)
      D2Complement.size(domainSize(100, 0, 100), excluded) should be(99)
    }

    it("should have a correct set of constants with 1 excluded constant") {
      val excluded = Set(Constant("a"))
      //            evaluating { D2Complement.constants(domainSize(10, 10, 0), excluded) } should produce[Throwable]
      D2Complement.constants(domainSize(10, 9, 1), excluded) should have size (0)
      D2Complement.constants(domainSize(10, 8, 2), excluded) should have size (1)
      D2Complement.constants(domainSize(10, 7, 3), excluded) should have size (2)
      D2Complement.constants(domainSize(100, 0, 100), excluded) should have size (99)
    }

    it("should have a correct size with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant(10))
      //            evaluating { D2Complement.size(domainSize(10, 9, 1), excluded2) } should produce[Throwable]
      D2Complement.size(domainSize(10, 8, 2), excluded2) should be(0)
      D2Complement.size(domainSize(10, 7, 3), excluded2) should be(1)
      D2Complement.size(domainSize(100, 0, 100), Set(Constant("a"), Constant(90))) should be(98)
    }

    it("should have a correct set of constants with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant(9))
      //            evaluating { D2Complement.constants(domainSize(10, 9, 1), excluded2) } should produce[Throwable]
      D2Complement.constants(domainSize(10, 8, 2), excluded2) should have size (0)
      D2Complement.constants(domainSize(10, 7, 3), excluded2) should have size (1)
      D2Complement.constants(domainSize(100, 0, 100), Set(Constant("a"), Constant(99))) should have size (98)
    }
  }

  describe("a subdomain with 1 excluded element") {

    val D = new RootDomain("D", List(Constant("a"), Constant("b")))
    val D2 = D.subdomain(excludedConstants = Set(Constant("b")))

    def domainSize(rootSize: Int, subSize: Int) = {
      (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize)
    }

    it("should have a correct size without additional excluded constants") {
      val excluded = Set(Constant("b"))
      D2.size(domainSize(11, 0), excluded) should be(0)
      D2.size(domainSize(11, 1), excluded) should be(1)
      D2.size(domainSize(11, 2), excluded) should be(2)
      D2.size(domainSize(11, 3), excluded) should be(3)
      D2.size(domainSize(101, 100), excluded) should be(100)
    }

    it("should have a correct set of constants without additional excluded constants") {
      val excluded = Set(Constant("b"))
      D2.constants(domainSize(11, 0), excluded) should have size (0)
      D2.constants(domainSize(11, 1), excluded) should have size (1)
      D2.constants(domainSize(11, 2), excluded) should have size (2)
      D2.constants(domainSize(11, 3), excluded) should have size (3)
      D2.constants(domainSize(101, 100), excluded) should have size (100)
    }

    it("should have a correct size with 1 additional excluded constant") {
      val excluded = Set(Constant("b"), Constant("a"))
      //            evaluating { D2.size(domainSize(11, 0), excluded) } should produce[Throwable]
      D2.size(domainSize(11, 1), excluded) should be(0)
      D2.size(domainSize(11, 2), excluded) should be(1)
      D2.size(domainSize(11, 3), excluded) should be(2)
      D2.size(domainSize(101, 100), excluded) should be(99)
    }

    it("should have a correct set of constants with 1 additional excluded constant") {
      val excluded = Set(Constant("b"), Constant("a"))
      //            evaluating { D2.constants(domainSize(11, 0), excluded) } should produce[Throwable]
      D2.constants(domainSize(11, 1), excluded) should have size (0)
      D2.constants(domainSize(11, 2), excluded) should have size (1)
      D2.constants(domainSize(11, 3), excluded) should have size (2)
      D2.constants(domainSize(101, 100), excluded) should have size (99)
    }

    it("should have a correct size with 2 additional excluded constants") {
      val excluded2 = Set(Constant("b"), Constant("a"), Constant(1))
      //            evaluating { D2.size(domainSize(11, 1), excluded2) } should produce[Throwable]
      D2.size(domainSize(11, 2), excluded2) should be(0)
      D2.size(domainSize(11, 3), excluded2) should be(1)
      D2.size(domainSize(101, 100), excluded2) should be(98)
    }

    it("should have a correct set of constants with 2 additional excluded constants") {
      val excluded2 = Set(Constant("b"), Constant("a"), Constant(1))
      //            evaluating { D2.constants(domainSize(11, 1), excluded2) } should produce[Throwable]
      D2.constants(domainSize(11, 2), excluded2) should have size (0)
      D2.constants(domainSize(11, 3), excluded2) should have size (1)
      D2.constants(domainSize(101, 100), excluded2) should have size (98)
    }
  }

  describe("a subdomain complement with 1 excluded element") {

    val D = new RootDomain("D", List(Constant("a"), Constant("b")))
    val D2 = D.subdomain(excludedConstants = Set(Constant("b")))
    val D2Complement = D2.complement

    def domainSize(rootSize: Int, subSize: Int, complSize: Int) = {
      (new DomainSizes()) + (D -> rootSize) + (D2 -> subSize) + (D2Complement -> complSize)
    }

    it("should have a correct size without excluded constants") {
      val excluded = Set(Constant("b"))
      D2Complement.size(domainSize(11, 10, 0), excluded) should be(0)
      D2Complement.size(domainSize(11, 9, 1), excluded) should be(1)
      D2Complement.size(domainSize(11, 8, 2), excluded) should be(2)
      D2Complement.size(domainSize(11, 7, 3), excluded) should be(3)
      D2Complement.size(domainSize(101, 0, 100), excluded) should be(100)
    }

    it("should have a correct set of constants without excluded constants") {
      val excluded = Set(Constant("b"))
      D2Complement.constants(domainSize(11, 10, 0), excluded) should have size (0)
      D2Complement.constants(domainSize(11, 9, 1), excluded) should have size (1)
      D2Complement.constants(domainSize(11, 8, 2), excluded) should have size (2)
      D2Complement.constants(domainSize(11, 7, 3), excluded) should have size (3)
      D2Complement.constants(domainSize(101, 0, 100), excluded) should have size (100)
    }

    it("should have a correct size with 1 excluded constant") {
      val excluded = Set(Constant("a"), Constant("b"))
      //            evaluating { D2Complement.size(domainSize(11, 10, 0), excluded) } should produce[Throwable]
      D2Complement.size(domainSize(11, 9, 1), excluded) should be(0)
      D2Complement.size(domainSize(11, 8, 2), excluded) should be(1)
      D2Complement.size(domainSize(11, 7, 3), excluded) should be(2)
      D2Complement.size(domainSize(101, 0, 100), excluded) should be(99)
    }

    it("should have a correct set of constants with 1 excluded constant") {
      val excluded = Set(Constant("a"), Constant("b"))
      //            evaluating { D2Complement.constants(domainSize(11, 10, 0), excluded) } should produce[Throwable]
      D2Complement.constants(domainSize(11, 9, 1), excluded) should have size (0)
      D2Complement.constants(domainSize(11, 8, 2), excluded) should have size (1)
      D2Complement.constants(domainSize(11, 7, 3), excluded) should have size (2)
      D2Complement.constants(domainSize(101, 0, 100), excluded) should have size (99)
    }

    it("should have a correct size with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant("b"), Constant(10))
      //            evaluating { D2Complement.size(domainSize(11, 9, 1), excluded2) } should produce[Throwable]
      D2Complement.size(domainSize(11, 8, 2), excluded2) should be(0)
      D2Complement.size(domainSize(11, 7, 3), excluded2) should be(1)
      D2Complement.size(domainSize(101, 0, 100), Set(Constant("a"), Constant("b"), Constant(90))) should be(98)
    }

    it("should have a correct set of constants with 2 excluded constant") {
      val excluded2 = Set(Constant("a"), Constant("b"), Constant(9))
      //            evaluating { D2Complement.constants(domainSize(11, 9, 1), excluded2) } should produce[Throwable]
      D2Complement.constants(domainSize(11, 8, 2), excluded2) should have size (0)
      D2Complement.constants(domainSize(11, 7, 3), excluded2) should have size (1)
      D2Complement.constants(domainSize(101, 0, 100), Set(Constant("a"), Constant("b"), Constant(99))) should have size (98)
    }

  }

}
