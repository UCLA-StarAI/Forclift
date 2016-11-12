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
class TestBug17 extends ModelBehaviours {

  describe("Bug12Model from Leon Bergen (size 2)") {

    val correctLogWMC = math.log(2.0) +- 0.00001

    def model = new MLNModel {
      def theoryString = """
person = {1,2}

friends(person,person)

friends(x,x).
friends(x,y) => friends(y,x).
"""
    }

    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("Bug12Model from Leon Bergen (size 5)") {

    val correctLogWMC = math.log(1024.0) +- 0.00001

    def model = new MLNModel {
      def theoryString = """
person = {1,...,5}

friends(person,person)

friends(x,x).
friends(x,y) => friends(y,x).
"""
    }

    it should behave like smallModel(model, correctLogWMC)
  }

}
