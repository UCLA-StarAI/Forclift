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
class TestBug13 extends ModelBehaviours {

  // this bug is probabilistic, happens on every 5th run on average

  describe("Bug13Model") {

    val correctLogWMC = 23.88112692867715 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 4 {a}
                
predicate male(Person) 1 2
predicate workedunder(Person,Person) 1 2
predicate f1(Person,Person,Person) 1 2
                
f1(X,Y,Z) ∨ male(Y)
f1(X,Y,Z) ∨ workedunder(Z,Y)
f1(X,Y,Z) ∨ ¬male(X)
f1(X,Y,Z) ∨ ¬workedunder(X,Y)
male(X) ∨ workedunder(X,Y) ∨ ¬f1(X,Y,Z) ∨ ¬male(Y) ∨ ¬workedunder(Z,Y)
"""
    }

    it should behave like smallModel(model, correctLogWMC)
  }

}
