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

package edu.ucla.cs.starai.forclift.examples.models

import org.scalatest.Spec
import org.scalatest.Matchers
import org.scalatest.FunSpec
import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.tagobjects.Slow

trait ModelBehaviours extends FunSpec with Matchers {

  def slowBigModel(model: StringModel, correctLogWmc: Spread[Double]) {

    it("should have the correct lifted WMC", Slow) {
      model.theory.logSmoothWmc.logToDouble should be(correctLogWmc)
    }

  }
  
  def bigModel(model: StringModel, correctLogWmc: Spread[Double]) {

    it("should have the correct lifted WMC") {
      model.theory.logSmoothWmc.logToDouble should be(correctLogWmc)
    }

  }

  def smallModel(model: StringModel, correctLogWmc: Spread[Double]) {

    it should behave like bigModel(model, correctLogWmc)

    // disabled for performance
    it("should have the correct propositional WMC") {
      model.theory.logSmoothPropWmc.logToDouble should be(correctLogWmc)
    }

  }

  def verySmallModel(model: StringModel, correctLogWmc: Spread[Double]) {

    it should behave like smallModel(model, correctLogWmc)

    // disabled for performance
    it("should succeed at internal verification") {
      model.theory.verifyLogWmc
    }

  }

}
