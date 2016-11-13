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
class TestBraz2 extends ModelBehaviours {

  describe("Braz2Model of size 3") {
    val correctLogWMC = 3.7856097573831686 +- 0.00001
    val model = new Braz2Model(3)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("Braz2Model of size 8") {
    val correctLogWMC = 8.907410509908743 +- 0.00001
    val model = new Braz2Model(8)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("Braz2Model of size 100") {
    val correctLogWMC = 110.52568313867783 +- 0.00001
    val model = new Braz2Model(100)
    it should behave like bigModel(model, correctLogWMC)
  }

}
