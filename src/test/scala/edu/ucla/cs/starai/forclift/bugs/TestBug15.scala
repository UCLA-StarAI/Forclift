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
class TestBug15 extends ModelBehaviours {

  // this bug is probabilistic, happens on every 5th run on average

  describe("Bug15Model") {

    val correctLogWMC = 44.66478531783146 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 3 {}
domain Course 3 {}
domain Year 3 {}
    
predicate ta(Course,Person,Year) 1.9 1
predicate tempadvisedby(Person,Person) 1.15 1
predicate f2(Person) 1 1.2        
predicate f3(Course,Person,Year,Person) 1 1.2              
                
tempadvisedby(X,X) ∨ ¬f2(X)
f3(X,Y,Z,B) ∨ ta(X,Y,Z)
f3(X,Y,Z,B) ∨ tempadvisedby(Y,B)
¬f3(X,Y,Z,B) ∨ ¬ta(X,Y,Z) ∨ ¬tempadvisedby(Y,B)
"""
    }

    //        model.theory.showNnfPdf(true, 70, "bug", false)

    it should behave like smallModel(model, correctLogWMC)
  }

}
