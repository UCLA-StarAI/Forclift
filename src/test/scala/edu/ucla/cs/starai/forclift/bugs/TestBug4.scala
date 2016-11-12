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
class TestBug4 extends ModelBehaviours {

  describe("Bug4Model") {

    val correctLogWMC = -6.256216253777501 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 10 {1}
        
predicate series 1.0 1.0
predicate f1(Person) 0.501 0.499
predicate seriesf1(Person) 1.0 1.0
predicate attr1f2(Person) 1.0 1.0
predicate f2(Person) 0.501 0.499
predicate attendsf1(Person) 1.0 1.0
predicate attr1 1.0 1.0
predicate attendsf2(Person) 1.0 1.0
predicate attends(Person) 1.0 1.0

attends(1)
attends(X) ∨ ¬attendsf1(X)
attends(X) ∨ ¬attendsf2(X)
attendsf1(X) ∨ ¬attends(X)
attendsf1(X) ∨ ¬f1(X)
attendsf2(X) ∨ ¬attends(X)
attendsf2(X) ∨ ¬f2(X)
attr1 ∨ ¬attr1f2(X)
attr1f2(X) ∨ ¬attr1
attr1f2(X) ∨ ¬f2(X)
f1(X) ∨ ¬seriesf1(X) ∨ ¬attendsf1(X)
f2(X) ∨ ¬attr1f2(X) ∨ ¬attendsf2(X)
series ∨ ¬seriesf1(X)
seriesf1(X) ∨ ¬f1(X)
seriesf1(X) ∨ ¬series
	"""
    }
    it should behave like verySmallModel(model, correctLogWMC)
  }
}
