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
class TestSymmetricFriendsSmoker extends ModelBehaviours {

  describe("SymmetricFriendsSmokerModel of size 3") {
    val correctLogWMC = 13.046039049951757 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(3)
    // small becuase internal verification is very slow!
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("SymmetricFriendsSmokerModel of size 10") {
    val correctLogWMC = 90.74813712898454 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(10)
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("SymmetricFriendsSmokerModel of size 100") {
    val correctLogWMC = 7285.453839400913 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(100)
    it should behave like bigModel(model, correctLogWMC)
  }

  describe("SymmetricFriendsSmokerModel of size 3 with evidence") {
    val correctLogWMC = 11.375057907486354 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(3, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("SymmetricFriendsSmokerModel of size 10 with evidence") {
    val correctLogWMC = 88.80875296020481 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(10, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like smallModel(model, correctLogWMC)
  }

  describe("SymmetricFriendsSmokerModel of size 100 with evidence") {
    val correctLogWMC = 7267.860735203318 +- 0.00001
    val model = new SymmetricFriendsSmokerModel(100, List("guy", "luc", "bert"), List("friends(guy,luc)", "smokes(luc)"))
    it should behave like bigModel(model, correctLogWMC)
  }

}
