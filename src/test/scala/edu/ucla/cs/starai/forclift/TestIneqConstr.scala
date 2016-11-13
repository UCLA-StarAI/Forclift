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

package edu.ucla.cs.starai.forclift

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import constraints._

import org.scalatest.FunSpec


@RunWith(classOf[JUnitRunner])
class TestIneqConstr extends FunSpec with Matchers {

  describe("a an equality constraint between two variables") {

    val x = new Var
    val y = new Var
    val ineqConstr = IneqConstr((x, y))

    it("should have an empty projection to one variable") {
      ineqConstr.project(Set(x)) should be('empty)
      ineqConstr.project(Set(y)) should be('empty)
    }

    it("should have an equal projection to two variables") {
      ineqConstr.project(Set(x, y)) should be(ineqConstr)
    }

  }

}
