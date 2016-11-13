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

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import edu.ucla.cs.starai.forclift._

@RunWith(classOf[JUnitRunner])
class TestCompetingWorkshops extends ModelBehaviours {

  describe("CompetingWorkshopsModel of size 3-3") {
    val correctLogWMC = -0.480775478137514 +- 0.00001
    val model = new CompetingWorkshopsModel(3, 3)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("CompetingWorkshopsModel of size 5-5") {
    val correctLogWMC = -4.005627871308871 +- 0.00001
    val model = new CompetingWorkshopsModel(5, 5)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("CompetingWorkshopsModel of size 7-3") {
    val correctLogWMC = -3.85528009251353 +- 0.00001
    val model = new CompetingWorkshopsModel(7, 3)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("CompetingWorkshopsModel of size 3-7") {
    val correctLogWMC = -1.0807010157870418 +- 0.00001
    val model = new CompetingWorkshopsModel(3, 7)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("CompetingWorkshopsModel of size 150-100") {
    val correctLogWMC = -32.75164395460291 +- 0.00001
    val model = new CompetingWorkshopsModel(15, 10)
    it should behave like bigModel(model, correctLogWMC)
  }

}
