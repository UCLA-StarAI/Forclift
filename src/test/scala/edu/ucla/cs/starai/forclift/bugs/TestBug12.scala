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
class TestBug12 extends ModelBehaviours {

  describe("Bug12Model") {

    val correctLogWMC = 146.52023270334251 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Page 4 {llqanon1}
                
predicate pageclassfaculty(Page) 1 2
predicate pageclassstudent(Page) 3 4
predicate linked(Page,Page) 3 4
predicate f1(Page,Page) 3 4
predicate f2(Page,Page) 2 4
predicate f3(Page,Page) 1 4
predicate f4(Page,Page) 5 4
predicate f5(Page,Page) 3 2
                
linked(X,Y) ∨ ¬f5(X,Y) ∨ ¬linked(Y,X)
pageclassfaculty(X) ∨ ¬f1(X,Y) ∨ ¬linked(Y,X) ∨ ¬pageclassstudent(Y)
pageclassstudent(X) ∨ ¬f2(X,Y) ∨ ¬linked(Y,X) ∨ ¬pageclassfaculty(Y)
pageclassstudent(llqanon1)
f1(X,Y) ∨ linked(Y,X)
f1(X,Y) ∨ pageclassstudent(Y)
f1(X,Y) ∨ ¬pageclassfaculty(X)
f2(X,Y) ∨ linked(Y,X)
f2(X,Y) ∨ pageclassfaculty(Y)
f2(X,Y) ∨ ¬pageclassstudent(X)
f4(X,Y) ∨ ¬linked(X,Y)
f5(X,Y) ∨ linked(Y,X)
f5(X,Y) ∨ ¬linked(X,Y)
"""
    }

    it should behave like smallModel(model, correctLogWMC)
  }

}
