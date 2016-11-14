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
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSickDeath extends ModelBehaviours {

  describe("SickDeathModel of size 3") {
    val correctLogWMC = -3.4339026018140526 +- 0.00001
    val model = new SickDeathModel(3)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("SickDeathModel of size 8") {
    val correctLogWMC = -10.12200574211647 +- 0.00001
    val model = new SickDeathModel(8)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("SickDeathModel of size 100") {
    val correctLogWMC = -126.12444346852573 +- 0.00001
    val model = new SickDeathModel(100)
    it should behave like bigModel(model, correctLogWMC)
  }

}
