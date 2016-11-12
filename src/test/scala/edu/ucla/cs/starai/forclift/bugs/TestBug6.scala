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
class TestBug6 extends ModelBehaviours {

  val modelStr = """
domain Person 4
        
predicate p(Person) 0.6 0.8

!p(X) ∨ !p(Y), X!=Y
	"""

  // computed exactly as log(0.8^4 + 4* 0.8^3 * 0.6) = 0.493720
  val correctLogWMC = 0.4937201558630517 +- 0.00001

  describe("Bug6Model") {

    val defaultModel = new WeightedCNFModel {
      val theoryString = modelStr
    }
    it should behave like verySmallModel(defaultModel, correctLogWMC)

  }

}
