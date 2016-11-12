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
class TestBug18 extends ModelBehaviours {

  describe("Bug18Model from Leon Bergen (equality and universe)") {

    val correctLogWMC = math.log(4.0) +- 0.00001

    def model = new MLNModel {
      def theoryString = """
person = {1, 2}
friends(person,person)
forall y forall z y=z => friends(y,z).
"""
    }
    
    def modelq = new MLNModel {
      def theoryString = """
person = {1, 2}
friends(person,person)
forall y forall z y=z => friends(y,z).
friends(1,1).
"""
    }
    
//    it("Should compute wmc") {
//      val lwmc = model.theory.logWmc
//      lwmc should be (correctLogWMC)
//    }
    
//    it("Should compute query wmc") {
//      val lwmcq = modelq.theory.logWmc
//      println("lwmcq = "+lwmcq)
//      lwmcq should be (correctLogWMC)
//    }

//    it("Should compute wmc (nodist)") {
//      val lwmc = model.theory_nodist.logWmc
////      println("lwmc = "+lwmc)
//      lwmc should be (correctLogWMC)
//    }
//
    it("Should compute query wmc (nodist)") {
      val lwmc = modelq.theory.logSmoothWmc.logToDouble
//      println("lwmc = "+lwmc)
      lwmc should be (correctLogWMC)
    }
    
    it should behave like verySmallModel(model, correctLogWMC)
  }

}
