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
class TestWebKB extends ModelBehaviours {

  describe("WebKBModel of size 2") {
    val correctLogWMC = 6.232409975434042 +- 0.00001
    val model = new WebKBModel(1)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("WebKBModel of size 6") {
    val correctLogWMC = 81.57801552207889 +- 0.00001
    val model = new WebKBModel(6)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("WebKBModel of size 50") {
    val correctLogWMC = 4078.5120011311465 +- 0.00001
    val model = new WebKBModel(50)
    it should behave like bigModel(model, correctLogWMC)
  }

}
