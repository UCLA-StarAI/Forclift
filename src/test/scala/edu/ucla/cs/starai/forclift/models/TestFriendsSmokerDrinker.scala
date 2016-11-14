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
class TestFriendsSmokerDrinker1 extends ModelBehaviours {

  describe("FriendsSmokerDrinkerModel of size 2") {
    val correctLogWMC = 6.919575403176769 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(2)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerDrinkerModel of size 8") {
    val correctLogWMC = 76.44237474352845 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(8)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerDrinkerModel of size 200") {
    val correctLogWMC = 42312.99807235724 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(200)
    it should behave like slowBigModel(model, correctLogWMC)
  }
}

@RunWith(classOf[JUnitRunner])
class TestFriendsSmokerDrinkerWithEvidence extends ModelBehaviours {

  describe("FriendsSmokerDrinkerModel of size 2 with evidence") {
    val correctLogWMC = 4.881748742732568 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(2, List("guy", "luc"), List("drinks(guy)", "drinks(luc)", "smokes(luc)"))
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerDrinkerModel of size 8 with evidence") {
    val correctLogWMC = 74.4227005248472 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(8, List("guy", "luc"), List("drinks(guy)", "drinks(luc)", "smokes(luc)"))
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerDrinkerModel of size 100 with evidence") {
    val correctLogWMC = 10577.938944961066 +- 0.00001
    val model = new FriendsSmokerDrinkerModel(100, List("guy", "luc"), List("drinks(guy)", "drinks(luc)", "smokes(luc)"))
    it should behave like bigModel(model, correctLogWMC)
  }

}
