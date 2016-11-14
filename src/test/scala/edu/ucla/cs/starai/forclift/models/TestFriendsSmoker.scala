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
class TestFriendsSmoker extends ModelBehaviours {

  describe("FriendsSmokerModel of size 3") {
    val correctLogWMC = 15.11455938018598 +- 0.00001
    val model = new FriendsSmokerModel(3)
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerModel of size 10") {
    val correctLogWMC = 121.80653199663033 +- 0.00001
    val model = new FriendsSmokerModel(10)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerModel of size 100") {
    val correctLogWMC = 10716.532380061839 +- 0.00001
    val model = new FriendsSmokerModel(100)
    it should behave like bigModel(model, correctLogWMC)
  }

  describe("FriendsSmokerModel of size 3 with evidence") {
    val correctLogWMC = 13.493571492290151 +- 0.00001
    val model = new FriendsSmokerModel(3, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like verySmallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerModel of size 10 with evidence") {
    val correctLogWMC = 119.89286177478779 +- 0.00001
    val model = new FriendsSmokerModel(10, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("FriendsSmokerModel of size 100 with evidence") {
    val correctLogWMC = 10698.228003257838 +- 0.00001
    val model = new FriendsSmokerModel(100, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like bigModel(model, correctLogWMC)
  }

}
