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

import org.junit.runner.RunWith
import org.scalatest.Finders
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestWorkshopAttributes extends ModelBehaviours {

  describe("WorkshopAttributesModel of size 3") {
    val correctLogWMC = -2.7541604288921384 +- 0.00001
    val model = new WorkshopAttributesModel(3)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("WorkshopAttributesModel of size 10") {
    val correctLogWMC = -11.68583398942432 +- 0.00001
    val model = new WorkshopAttributesModel(10)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("WorkshopAttributesModel of size 500") {
    val correctLogWMC = -618.0786890315445 +- 0.00001
    val model = new WorkshopAttributesModel(500)
    it should behave like bigModel(model, correctLogWMC)
  }

}
