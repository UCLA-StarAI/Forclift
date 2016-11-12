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

import edu.ucla.cs.starai.forclift.examples.models.mln.OriginalSymmetricFriendsSmokerModel
import edu.ucla.cs.starai.forclift.examples.models.ModelBehaviours

@RunWith(classOf[JUnitRunner])
class TestBug5 extends ModelBehaviours {

  val correctLogWmc = 12.175520828021622 +- 0.00001
  val wmc = (new OriginalSymmetricFriendsSmokerModel(1)).theory
  val coShatteredCnf = wmc.cnf.coShatter
  val coShatteredWmc = wmc.copy(cnf = coShatteredCnf)

  describe("Bug5Model") {
    it("should have the correct lifted WMC") {
      wmc.logSmoothWmc.logToDouble should be(correctLogWmc)
    }
    it("should have the correct propositional WMC") {
      wmc.logSmoothPropWmc.logToDouble should be(correctLogWmc)
    }
    it("should succeed at internal verification") {
      wmc.verifyLogWmc
    }
  }

  describe("Bug5Model coshattered") {
    it("should have the correct lifted WMC") {
      coShatteredWmc.logSmoothWmc.logToDouble should be(correctLogWmc)
    }
    it("should have the correct propositional WMC") {
      coShatteredWmc.logSmoothPropWmc.logToDouble should be(correctLogWmc)
    }
    it("should succeed at internal verification") {
      coShatteredWmc.verifyLogWmc
    }
  }

  describe("Bug5Model coshattered an not") {
    it("should have the same model count") {
      (wmc.logSmoothWmc.logToDouble) should be(coShatteredWmc.logSmoothWmc.logToDouble +- 0.00001)
    }
  }
}
