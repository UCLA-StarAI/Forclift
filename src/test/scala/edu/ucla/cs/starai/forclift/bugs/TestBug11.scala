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
class TestBug11 extends ModelBehaviours {

  describe("Bug11Model - Z") {

    val correctLogWMC = (math.log(2) * 12) +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 6 {llqanon1}
          
predicate student(Person) 1.0 1.0
predicate professor(Person) 1.0 1.0
"""
    }

    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("Bug11Model - Query") {

    val correctLogWMC = (math.log(2) * 11) +- 0.00001

    def model = new WeightedCNFModel {
      def theoryString = """
domain Person 6 {llqanon1}
          
predicate student(Person) 1.0 1.0
predicate professor(Person) 1.0 1.0
                
student(llqanon1)
"""
    }

    // test like big model, because c2d fails on this one, even the smooth root
    it should behave like bigModel(model, correctLogWMC)
  }

}
