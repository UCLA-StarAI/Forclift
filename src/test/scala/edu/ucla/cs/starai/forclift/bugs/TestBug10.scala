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
class TestBug10 extends ModelBehaviours {

  //    describe("Bug10Model - Denominator") {
  //
  //        val correctLogWMC = 157.9760083378145 +- 0.00001
  //
  //        def model = new WeightedCNFModel {
  //            def theoryString = """
  //
  //domain Course 1 {llqanon1}
  //domain Year 1 {llqanon1}
  //domain Person 4 {llqanon1,llqanon2}
  //          
  //predicate advisedby(Person,Person) 1.0276334856897869 1.0
  //predicate tempadvisedby(Person,Person) 1.0276334856897869 1.0
  //predicate f1(Person,Person,Person) 9.44762446064903 1.0
  //predicate taughtby(Course,Person,Year) 1.0337621302489077 1.0
  //          
  //f1(X,Y,Z) ∨ advisedby(X,Z)
  //f1(X,Y,Z) ∨ tempadvisedby(X,Y)
  //¬f1(X,Y,Z) ∨ ¬tempadvisedby(X,Y) ∨ ¬advisedby(X,Z)
  //	"""
  //        }
  //
  //        it should behave like verySmallModel(model, correctLogWMC)
  //    }

  describe("Bug10Model - Numerator") {

    val correctLogWMC = 160.13887507134228 +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """

domain Course 1 {llqanon1}
domain Year 1 {llqanon1}
domain Person 4 {llqanon1,llqanon2}
          
predicate advisedby(Person,Person) 1.0276334856897869 1.0
predicate tempadvisedby(Person,Person) 1.0276334856897869 1.0
predicate f1(Person,Person,Person) 9.44762446064903 1.0
predicate taughtby(Course,Person,Year) 1.0337621302489077 1.0
         
taughtby(llqanon1,llqanon1,llqanon1) 
f1(X,Y,Z) ∨ advisedby(X,Z)
f1(X,Y,Z) ∨ tempadvisedby(X,Y)
¬f1(X,Y,Z) ∨ ¬tempadvisedby(X,Y) ∨ ¬advisedby(X,Z)
	"""
    }

    it should behave like verySmallModel(model, correctLogWMC)
  }

}
