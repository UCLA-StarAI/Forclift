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
import edu.ucla.cs.starai.forclift.inference.AllMarginalsExact

@RunWith(classOf[JUnitRunner])
class TestBug21 extends ModelBehaviours {

  describe("Bug21Model subdomains") {

//    val correctLogWMC = math.log(math.pow(math.pow(2,n)-1,n)) +- 0.00001

    def model = new MLNModel {
      def theoryString = s"""
person = {male, female}
male = 2 {}
female = 3 {}
friends(person,person)
friends(x,y), x in male, y in female.
1 friends(x,y)
"""
    }

    
    it("An exception should be thrown when trying to compute all marginals") {
      intercept[IllegalArgumentException] { 
    	val allmarginals = new AllMarginalsExact(true)
      allmarginals.computeAllMarginals(model.theory)
      }
    }
  }

}
