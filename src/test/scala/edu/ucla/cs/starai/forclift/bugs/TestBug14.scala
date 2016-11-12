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

package edu.ucla.cs.starai.forclift.bugs

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._

@RunWith(classOf[JUnitRunner])
class TestBug14 extends ModelBehaviours {

  // this bug is probabilistic, happens on every 5th run on average

  describe("Bug14Model") {

    val correctLogWMC = 37.344925186552636 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Page 3 {a}
    
predicate word1(Page) 1 1.2
predicate word2(Page) 1 1.3
predicate word3(Page) 1 1.4
predicate word4(Page) 1 1.5
predicate word5(Page) 1 1.6
predicate word6(Page) 1 1.7
predicate class1(Page) 1 1.2
predicate class2(Page) 1 1.37
predicate class3(Page) 1 1.4
predicate linked(Page,Page) 1 1.5
predicate f1(Page) 1 1.1
predicate f2(Page) 1 1.2
predicate f3(Page) 1 1.3
predicate f4(Page) 1 1.4
predicate f5(Page) 1 1.5
predicate f6(Page) 1 1.6
predicate f7(Page) 1 1.7
predicate f8(Page) 1 1.81
predicate f9(Page) 1 1.91
predicate f10(Page) 1 1.12
predicate f11(Page) 1 1.13
predicate f12(Page) 1 1.14
predicate f13(Page) 1 1.15
predicate f14(Page) 1 1.16
predicate f15(Page) 1 1.17
predicate f16(Page) 1 1.18
predicate f17(Page) 1 1.19
predicate f18(Page) 1 1.13
predicate f19(Page,Page) 1 1.10
predicate f20(Page,Page) 1 1.15
predicate f21(Page,Page) 1 1.146
predicate f22(Page,Page) 1 1.154
predicate f23(Page,Page) 1 1.12123

class1(X) ∨ ¬f1(X) ∨ ¬word1(X)
class1(X) ∨ ¬f20(X,Y) ∨ ¬linked(Y,X) ∨ ¬class2(Y)
class1(X) ∨ ¬f21(X,Y) ∨ ¬linked(Y,X) ∨ ¬class3(Y)
class1(X) ∨ ¬f2(X) ∨ ¬word2(X)
class1(X) ∨ ¬f3(X) ∨ ¬word3(X)
class1(X) ∨ ¬f4(X) ∨ ¬word4(X)
class1(X) ∨ ¬f5(X) ∨ ¬word5(X)
class1(X) ∨ ¬f6(X) ∨ ¬word6(X)
class2(X) ∨ ¬f10(X) ∨ ¬word4(X)
class2(X) ∨ ¬f11(X) ∨ ¬word5(X)
class2(X) ∨ ¬f12(X) ∨ ¬word6(X)
class2(X) ∨ ¬f19(X,Y) ∨ ¬linked(Y,X) ∨ ¬class1(Y)
class2(X) ∨ ¬f22(X,Y) ∨ ¬linked(Y,X) ∨ ¬class3(Y)
class2(X) ∨ ¬f7(X) ∨ ¬word1(X)
class2(X) ∨ ¬f8(X) ∨ ¬word2(X)
class2(X) ∨ ¬f9(X) ∨ ¬word3(X)
class3(X) ∨ ¬f13(X) ∨ ¬word1(X)
class3(X) ∨ ¬f14(X) ∨ ¬word2(X)
class3(X) ∨ ¬f15(X) ∨ ¬word3(X)
class3(X) ∨ ¬f16(X) ∨ ¬word4(X)
class3(X) ∨ ¬f17(X) ∨ ¬word5(X)
class3(X) ∨ ¬f18(X) ∨ ¬word6(X)
linked(X,Y) ∨ ¬f23(X,Y) ∨ ¬linked(Y,X)
f10(X) ∨ word4(X)
f10(X) ∨ ¬class2(X)
f11(X) ∨ word5(X)
f11(X) ∨ ¬class2(X)
f12(X) ∨ word6(X)
f12(X) ∨ ¬class2(X)
f13(X) ∨ word1(X)
f13(X) ∨ ¬class3(X)
f14(X) ∨ word2(X)
f14(X) ∨ ¬class3(X)
f15(X) ∨ word3(X)
f15(X) ∨ ¬class3(X)
f16(X) ∨ word4(X)
f16(X) ∨ ¬class3(X)
f17(X) ∨ word5(X)
f17(X) ∨ ¬class3(X)
f18(X) ∨ word6(X)
f18(X) ∨ ¬class3(X)
f19(X,Y) ∨ class1(Y)
f19(X,Y) ∨ linked(Y,X)
f19(X,Y) ∨ ¬class2(X)
f1(X) ∨ word1(X)
f1(X) ∨ ¬class1(X)
f20(X,Y) ∨ class2(Y)
f20(X,Y) ∨ linked(Y,X)
f20(X,Y) ∨ ¬class1(X)
f21(X,Y) ∨ class3(Y)
f21(X,Y) ∨ linked(Y,X)
f21(X,Y) ∨ ¬class1(X)
f22(X,Y) ∨ class3(Y)
f22(X,Y) ∨ linked(Y,X)
f22(X,Y) ∨ ¬class2(X)
f23(X,Y) ∨ linked(Y,X)
f23(X,Y) ∨ ¬linked(X,Y)
f2(X) ∨ word2(X)
f2(X) ∨ ¬class1(X)
f3(X) ∨ word3(X)
f3(X) ∨ ¬class1(X)
f4(X) ∨ word4(X)
f4(X) ∨ ¬class1(X)
f5(X) ∨ word5(X)
f5(X) ∨ ¬class1(X)
f6(X) ∨ word6(X)
f6(X) ∨ ¬class1(X)
f7(X) ∨ word1(X)
f7(X) ∨ ¬class2(X)
f8(X) ∨ word2(X)
f8(X) ∨ ¬class2(X)
f9(X) ∨ word3(X)
f9(X) ∨ ¬class2(X)
"""
    }

    //        model.theory.showNnfPdf(true, 70, "bug", false)

    it should behave like smallModel(model, correctLogWMC)
  }

}
