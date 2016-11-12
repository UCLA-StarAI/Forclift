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
class TestBug19 extends ModelBehaviours {

  val n = 3

  describe("Bug19Model from Leon Bergen (overcounting model simple)") {

    val correctLogWMC = 5.1298987149230735 +- 0.00001

    def model = new MLNModel {
      def theoryString = s"""
person = {1 ,..., $n}
friends(person,person)
exist b friends(a,b).
exist e forall f friends(f,e).
"""
    }

    it should behave like smallModel(model, correctLogWMC)
  }

  describe("Bug19Model from Leon Bergen (overcounting model)") {

    val correctLogWMC = 4.844187086458591 +- 0.00001

    def model = new MLNModel {
      def theoryString = s"""
person = {1 ,..., $n}
friends(person,person)
exist b friends(a,b).
exist d friends(d,c).
exist e forall f friends(f,e).
"""
    }

    it should behave like smallModel(model, correctLogWMC)
  }

  describe("Bug19Model from Leon Bergen overcounting query simple simple") {

    val correctLogWMC = 8.050703381470298 +- 0.00001

    def query = new MLNModel {
      def theoryString = s"""
person = {1 ,..., $n}
Friends(person,person)
S(person)
T(person)
S(x) v !Friends(x,y), x!=1, y!=1.
S(x) v !Friends(x,1), x!=1.
T(x) v Friends(y,x), x!=1, y!=1.
T(x) v Friends(1,x), x!=1.
"""
    }

//    query.theory.showNnfPdf(false, 10, "bug.nnf", true)
    it should behave like verySmallModel(query, correctLogWMC)
  }

  describe("Bug19Model from Leon Bergen overcounting query simple") {

    val correctLogWMC = 4.6913478822291435 +- 0.00001

    def query = new MLNModel {
      def theoryString = s"""
person = {1 ,..., $n}
friends(person,person)
friends(1,1).
exist b friends(a,b).
exist e forall f friends(f,e).
"""
    }

    println("Query: " + query.theory.logSmoothWmc)
    println("Query: " + query.theory.logSmoothPropWmc)
//    query.theory.showNnfPdf(false, 10, "bug.nnf", true)
    it should behave like smallModel(query, correctLogWMC)
  }

  describe("Bug19Model from Leon Bergen overcounting query") {

    val correctLogWMC = 4.477336814478206 +- 0.00001

    def query = new MLNModel {
      def theoryString = s"""
person = {1 ,..., $n}
friends(person,person)
friends(1,1).
exist b friends(a,b).
exist d friends(d,c).
exist e forall f friends(f,e).
"""
    }

    println("Query: " + query.theory.logSmoothWmc)
    println("Query: " + query.theory.logSmoothPropWmc)
//    query.theory.showNnfPdf(false, 10, "bug.nnf", true)
    it should behave like smallModel(query, correctLogWMC)
  }

}
